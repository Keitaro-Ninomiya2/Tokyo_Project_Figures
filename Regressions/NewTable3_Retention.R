################################################################################
# Table 3: Retention of Pre-Existing (Non-Drafted) Workers
# Does drafting cause remaining workers to stay?
# Unit: individual worker × year (1938-1945)
# Sample: non-drafted workers observed in year t-1
# Outcome: retained (=1 if appears in year t)
# Treatment: drafts at their office × position in year t
# Columns: Baseline, × Gender, × Rank, × Engineer (all full sample)
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

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""),
         pos_rank = assign_rank(pos_norm, year_num)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""),
         pos_rank = assign_rank(pos_norm, year_num)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Identify years with any draft info
draft_years <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(year_num) %>%
  pull(year_num) %>%
  sort()
cat("Years with draft info:", paste(draft_years, collapse = ", "), "\n")

# ============================================================
# 1. BUILD RETENTION PANEL
# ============================================================

# Draft counts at office x pos x year
position_drafts <- df_all %>%
  filter(year_num %in% draft_years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Exclude anyone ever drafted
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# Cumulative male baseline
cumul_male_stock <- map_dfr(draft_years, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# For each draft year t: take non-drafted workers from 1938..t-1, check if in t
# Use worker's own ka/kyoku from most recent year for FE
pool_start <- min(draft_years) - 1  # include at least one year before first draft year
retention_rows <- list()
for (t in draft_years) {
  pre_existing <- df %>%
    filter(year_num %in% pool_start:(t - 1), !(staff_id %in% drafted_ids)) %>%
    group_by(staff_id) %>%
    slice_max(year_num, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(staff_id, office_id, pos_norm, pos_rank, is_female, ka, kyoku)

  appears_t <- df %>%
    filter(year_num == t) %>%
    distinct(staff_id) %>%
    mutate(retained = 1L)

  ret <- pre_existing %>%
    left_join(appears_t, by = "staff_id") %>%
    mutate(retained = replace_na(retained, 0L), draft_year = t)

  retention_rows[[as.character(t)]] <- ret
}

retention_panel <- bind_rows(retention_rows) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "draft_year" = "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "draft_year" = "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    year_num = draft_year
  )

panel_ka <- retention_panel %>%
  filter(!is.na(ka_id)) %>%
  mutate(
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

cat("Retention panel:", nrow(panel_ka), "obs\n")
cat("N female:", sum(panel_ka$is_female), " N male:", sum(!panel_ka$is_female), "\n")
cat("Positions with drafts:", sum(panel_ka$n_drafted_male > 0), "\n")

# ============================================================
# 2. REGRESSIONS (all full sample, different interactions)
# ============================================================

# C1: Baseline
m1 <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: × Gender
m2 <- feols(retained ~ n_drafted_male * is_female + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: × Rank (rank 2 = base; rank main effects absorbed by pos_norm FE)
m3 <- feols(retained ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: × Engineer (is_engineer main effect absorbed by pos_norm FE)
m4 <- feols(retained ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# 3. PRINT
# ============================================================

cat("\n===== TABLE 3: RETENTION =====\n\n")
etable(m1, m2, m3, m4,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Baseline", "x Gender", "x Rank", "x Engineer"))

# ============================================================
# 4. EXPORT LaTeX
# ============================================================

dict <- c(
  n_drafted_male                  = "No. drafted workers",
  "is_femaleTRUE"                 = "Female",
  "n_drafted_male:is_femaleTRUE"  = "No. drafted $\\times$ Female",
  "n_drafted_male:is_rank1"       = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3"       = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer"    = "No. drafted $\\times$ Engineer",
  "log(cumul_n_male + 1)"         = "log(Male baseline + 1)",
  ka_id                           = "Ka"
)

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
  drop_idx <- grep("\\bretained\\b", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  tex_content
}

tex <- etable(m1, m2, m3, m4,
              dict = dict,
              order = c("No. drafted w", "No. drafted.*Female",
                        "No. drafted.*Rank", "No. drafted.*Engineer",
                        "^Female"),
              drop = "log",
              headers = c("Baseline", "$\\times$ Gender",
                          "$\\times$ Rank", "$\\times$ Engineer"),
              se.below = TRUE, fitstat = ~n,
              tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var row after the header line (first \midrule)
midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_row <- "\\multicolumn{5}{l}{\\textit{Dep. var: Retained (=1 if worker appears in year $t$)}} \\\\"
tc <- append(tc, depvar_row, after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Retention of Non-Drafted Workers}",
  "\\label{tab:retention}",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} OLS linear probability model. Dependent variable: retained (=1 if worker appears in year $t$). Treatment: number of male workers drafted from the same office $\\times$ position in year $t$. All four columns use the full sample of non-drafted workers observed in any year from 1937 to $t-1$, using their most recent observation for office and position assignment. Column~1 estimates the baseline effect. Column~2 interacts the treatment with a female indicator. Column~3 interacts with rank dummies (Rank~1 = low-rank/temporary; Rank~3 = high-rank; Rank~2 is the omitted category). Column~4 interacts with an engineer-position indicator. All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline (not shown). Standard errors clustered at the office level in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable3_Retention.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable3_Retention.tex"))
cat("\nTable 3 exported to NewTable3_Retention.tex and Tokyo_Project\n")
