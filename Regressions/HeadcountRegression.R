################################################################################
# Headcount Regression: Effect of Drafting on Remaining Office Size
#
# Outcome: n_remaining = n_workers - n_drafted (headcount after draft exits)
# Treatment: n_drafted_male
# Unit: kakari x position x year (1938-1945)
# FE: ka + position + year
# Cluster: office_id
################################################################################

library(tidyverse)
library(fixest)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945

# ============================================================
# KYOKU NORMALIZATION (same as NewTable1_Reallocation.R)
# ============================================================

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "\u696d\u6240\u9577.*\u5065\u6c11|\u5bb6\u6240\u9577.*\u5065\u6c11") ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u696d\u6240\u9577.*\u539a\u751f")              ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u6771\u4eac\u5e02\u4e3b\u4e8b.*\u539a\u751f")  ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u6e05\u6383\u76e3\u7763.*\u539a\u751f")        ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u793e\u4f1a")       ~ "\u793e\u4f1a\u5c40",
    str_detect(k, "\u4fdd\u5065")       ~ "\u4fdd\u5065\u5c40",
    str_detect(k, "\u4e0a\u6728")       ~ "\u571f\u6728\u5c40",
    str_detect(k, "\u571f\u6728")       ~ "\u571f\u6728\u5c40",
    str_detect(k, "\u5e02\u4f1a\u4e8b\u52d9|Y\u4e8b\u52d9") ~ "\u5e02\u4f1a\u4e8b\u52d9\u5c40",
    str_detect(k, "\u5e9c\u4f1a\u4e8b\u52d9")   ~ "\u5e9c\u4f1a\u4e8b\u52d9\u5c40",
    str_detect(k, "\u767c\u80b2")       ~ "\u6559\u80b2\u5c40",
    str_detect(k, "\u6559\u80b2")       ~ "\u6559\u80b2\u5c40",
    str_detect(k, "\u6c34\u9055")       ~ "\u6c34\u9053\u5c40",
    str_detect(k, "\u6c34\u9053")       ~ "\u6c34\u9053\u5c40",
    str_detect(k, "\u96fb\u6c17")       ~ "\u96fb\u6c17\u5c40",
    str_detect(k, "\u990a\u80b2|\u6cbc\u52d9\u6240") ~ "\u990a\u80b2\u9662",
    str_detect(k, "\u8ca1\u52d9")       ~ "\u8ca1\u52d9\u5c40",
    str_detect(k, "\u7e70\u6e08|\u767c\u7d4c\u6e08|\u300a\u7d4c\u6e08|\u5b66\u6821\u4f53\u80b2\u8ab2\u7d4c\u6e08") ~ "\u7d4c\u6e08\u5c40",
    str_detect(k, "\u7d4c\u6e08")       ~ "\u7d4c\u6e08\u5c40",
    str_detect(k, "\u539a\u751f")       ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u5e02\u6c11")       ~ "\u5e02\u6c11\u5c40",
    str_detect(k, "\u4e2d\u592e\u5378\u58f2")   ~ "\u4e2d\u592e\u5378\u58f2\u5e02\u5834",
    str_detect(k, "\u6e2f\u6e96")       ~ "\u6e2f\u6e7e\u5c40",
    str_detect(k, "\u6e2f\u6e7e")       ~ "\u6e2f\u6e7e\u5c40",
    str_detect(k, "\u76e3\u67fb")       ~ "\u76e3\u67fb\u5c40",
    str_detect(k, "\u7523\u696d")       ~ "\u7523\u696d\u5c40",
    str_detect(k, "\u57ce\u6771\u75c5\u9662")   ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u5065\u6c11")       ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u6230\u6642\u751f\u6d3b")   ~ "\u6230\u6642\u751f\u6d3b\u5c40",
    str_detect(k, "\u7d4c\u3013")       ~ "\u7d4c\u7406\u5c40",
    str_detect(k, "\u7d4c\u7406")       ~ "\u7d4c\u7406\u5c40",
    str_detect(k, "\u52b4\u50cd")       ~ "\u52b4\u50cd\u5c40",
    str_detect(k, "\u5efa\u7bc9|\u5fa1\u7bc9")  ~ "\u5efa\u7bc9\u5c40",
    str_detect(k, "\u9053\u8def")       ~ "\u9053\u8def\u5c40",
    str_detect(k, "\u5fa9\u8208|\u4f0a\u8208\u4e8b\u696d") ~ "\u5fa9\u8208\u4e8b\u696d\u5c40",
    str_detect(k, "\u4ea4\u901a")       ~ "\u4ea4\u901a\u5c40",
    str_detect(k, "\u8a08[\u6641\u753b]")   ~ "\u8a08\u753b\u5c40",
    str_detect(k, "\u6c11\u5c40")       ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u5f8c\u9189\u9662")     ~ "\u96fb\u6c17\u5c40",
    str_detect(k, "\u7bc9\u5730\u7523\u9662|\u8352\u7523\u9662") ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u9632\u885b")       ~ "\u9632\u885b\u5c40",
    str_detect(k, "\u6c11\u751f")       ~ "\u6c11\u751f\u5c40",
    str_detect(k, "\u9577\u5b98\u5b98\u623f")   ~ "\u9577\u5b98\u5b98\u623f",
    TRUE ~ NA_character_
  )
}

assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("\u96fb\u6c17\u5c40", "\u4ea4\u901a\u5c40")             ~ "transport",
    nk == "\u6c34\u9053\u5c40"                             ~ "water",
    nk == "\u6e2f\u6e7e\u5c40"                             ~ "port",
    nk == "\u6559\u80b2\u5c40"                             ~ "education",
    nk %in% c("\u571f\u6728\u5c40", "\u8a08\u753b\u5c40", "\u7d4c\u7406\u5c40",
              "\u5efa\u7bc9\u5c40", "\u9053\u8def\u5c40")               ~ "infrastructure",
    nk %in% c("\u6230\u6642\u751f\u6d3b\u5c40", "\u7d4c\u6e08\u5c40",
              "\u4e2d\u592e\u5378\u58f2\u5e02\u5834", "\u7523\u696d\u5c40")         ~ "economy",
    nk %in% c("\u5065\u6c11\u5c40", "\u539a\u751f\u5c40", "\u793e\u4f1a\u5c40",
              "\u4fdd\u5065\u5c40", "\u6c11\u751f\u5c40", "\u990a\u80b2\u9662",
              "\u52b4\u50cd\u5c40")                         ~ "welfare",
    nk %in% c("\u5e02\u4f1a\u4e8b\u52d9\u5c40", "\u5e9c\u4f1a\u4e8b\u52d9\u5c40")      ~ "assembly",
    nk %in% c("\u8ca1\u52d9\u5c40", "\u76e3\u67fb\u5c40", "\u9577\u5b98\u5b98\u623f")  ~ "finance_admin",
    nk == "\u5fa9\u8208\u4e8b\u696d\u5c40"                         ~ "reconstruction",
    nk == "\u5e02\u6c11\u5c40"                             ~ "citizen",
    nk == "\u9632\u885b\u5c40"                             ~ "defense",
    TRUE ~ NA_character_
  )
}

df <- df %>%
  mutate(norm_kyoku  = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku))

# --- Rank assignment (pre-1948) ---
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

# --- Cumulative male baseline ---
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# ============================================================
# OUTCOME: Workers at kakari x position x year level
# ============================================================

# Count ALL workers (including drafted, since they were listed that year)
all_workers <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_workers_total = n(),
    n_drafted       = sum(drafted == TRUE, na.rm = TRUE),
    n_drafted_male  = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_remaining = n_workers_total - n_drafted,
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

# --- Merge cumulative male baseline ---
panel <- all_workers %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(cumul_n_male = replace_na(cumul_n_male, 0))

panel_ka <- panel %>% filter(!is.na(ka_id))
cat("Panel:", nrow(panel), "obs. With ka_id:", nrow(panel_ka), "\n")

# ============================================================
# REGRESSIONS
# ============================================================

# C1: Baseline — remaining headcount
c1 <- feols(n_remaining ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: × Rank interactions (rank 2 = base)
panel_ka <- panel_ka %>%
  mutate(
    pos_rank = assign_rank(pos_norm),
    is_rank1 = as.integer(pos_rank == 1),
    is_rank3 = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

c2 <- feols(n_remaining ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: × Engineer interaction
c3 <- feols(n_remaining ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: × Gender (female_share)
panel_ka <- panel_ka %>%
  mutate(female_share = {
    # Need female count from named workers
    fc <- df %>%
      filter(year_num %in% years_of_interest) %>%
      group_by(office_id, kakari, pos_norm, year_num) %>%
      summarise(n_female = sum(is_female, na.rm = TRUE),
                n_total_named = n(), .groups = "drop")
    left_join(panel_ka, fc, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
      mutate(fs = n_female / pmax(n_total_named, 1)) %>%
      pull(fs)
  })

c4 <- feols(n_remaining ~ n_drafted_male + n_drafted_male:female_share +
              female_share + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n===== HEADCOUNT REGRESSION: REMAINING WORKERS =====\n\n")
etable(c1, c2, c3, c4, se.below = TRUE, fitstat = ~n)

# ============================================================
# EXPORT LaTeX
# ============================================================

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else {
    tex_raw
  }
}

clean_depvar <- function(tex_content) {
  drop_idx <- grep("n\\_remaining", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  drop_idx3 <- grep("^\\s*&\\s*(OLS|Poisson)", tex_content)
  if (length(drop_idx3) > 0) tex_content <- tex_content[-drop_idx3]
  tex_content
}

dict <- c(
  n_drafted_male                = "No. drafted workers",
  "n_drafted_male:is_rank1"     = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3"     = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer"  = "No. drafted $\\times$ Engineer",
  "n_drafted_male:female_share" = "No. drafted $\\times$ Female share",
  female_share                  = "Female share"
)

tex <- etable(c1, c2, c3, c4,
              dict = dict,
              order = c("No. drafted"),
              drop = "log|Female share$",
              headers = c("Baseline", "$\\times$ Rank", "$\\times$ Eng.",
                          "$\\times$ Gender"),
              se.below = TRUE, fitstat = ~n, tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Effect of Military Drafting on Remaining Office Size}",
  "\\label{tab:headcount}",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: position $\\times$ kakari $\\times$ year (1938--1945). ",
         "Dependent variable: remaining headcount (total workers minus drafted workers). ",
         "Column~(2) interacts the treatment with rank indicators (rank~1 = \\emph{yatoi}/\\emph{shokutaku}; ",
         "rank~3 = \\emph{shuji}/\\emph{gishi}; rank~2 is the omitted category). ",
         "Column~(3) interacts with an engineer indicator. ",
         "Column~(4) interacts with the position's female employment share. ",
         "Rank and engineer main effects are absorbed by position fixed effects. ",
         "All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline (not shown). ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("HeadcountRegression.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "HeadcountRegression.tex"))
cat("\nTable exported to HeadcountRegression.tex\n")
