################################################################################
# Table 7: New Hiring in Response to Military Drafting
# C1: Baseline (total new hires)
# C2: × Rank interactions (rank 2 = base)
# C3: × Engineer interaction
# C4: Female new hires
# C5: Male new hires
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

# --- Rank assignment (pre-1948) ---
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

# --- Cumulative male baseline (kakari level) ---
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# --- Transitions ---
office_initial_year <- df %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    pos_rank = assign_rank(pos_norm)
  )

# --- Position outcomes (kakari level) ---
position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

# --- Draft counts (kakari level) ---
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted = n(), n_drafted_male = sum(!is_female, na.rm = TRUE),
            .groups = "drop")

# --- New hires by gender and rank (kakari level) ---
gender_hires <- staff_transitions %>%
  filter(is_new_hire == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_female    = sum(is_female, na.rm = TRUE),
    n_new_male      = sum(!is_female, na.rm = TRUE),
    n_new_female_r1 = sum(is_female & pos_rank == 1, na.rm = TRUE),
    n_new_female_r2 = sum(is_female & pos_rank >= 2, na.rm = TRUE),
    n_new_male_r1   = sum(!is_female & pos_rank == 1, na.rm = TRUE),
    n_new_male_r2   = sum(!is_female & pos_rank >= 2, na.rm = TRUE),
    .groups = "drop"
  )

# --- Build panel ---
position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(gender_hires, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male, n_new_female, n_new_male,
             n_new_female_r1, n_new_female_r2, n_new_male_r1, n_new_male_r2),
           ~replace_na(., 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

panel_ka <- position_panel %>% filter(!is.na(ka_id))
cat("Panel:", nrow(position_panel), "obs. With ka_id:", nrow(panel_ka), "\n")

# ============================================================
# REGRESSIONS
# ============================================================

# C1: Baseline — total new hires
c1 <- feols(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: × Rank interactions (rank 2 = base; rank main effects absorbed by pos_norm FE)
c2 <- feols(n_new_hires ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: × Engineer interaction (engineer main effect absorbed by pos_norm FE)
c3 <- feols(n_new_hires ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: Female new hires (all)
c4 <- feols(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C5: Female new hires, rank 1
c5 <- feols(n_new_female_r1 ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C6: Female new hires, rank 2+
c6 <- feols(n_new_female_r2 ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C7: Male new hires (all)
c7 <- feols(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C8: Male new hires, rank 1
c8 <- feols(n_new_male_r1 ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C9: Male new hires, rank 2+
c9 <- feols(n_new_male_r2 ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n===== TABLE 7: NEW HIRES =====\n\n")
etable(c1, c2, c3, c4, c5, c6, c7, c8, c9, se.below = TRUE, fitstat = ~n,
       headers = c("Baseline", "x Rank", "x Eng.",
                   "F: All", "F: R1", "F: R2+", "M: All", "M: R1", "M: R2+"))

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
  drop_idx <- grep("n\\_new", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  tex_content
}

dict <- c(
  n_drafted_male                = "No. drafted workers",
  "n_drafted_male:is_rank1"     = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3"     = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer"  = "No. drafted $\\times$ Engineer"
)

tex <- etable(c1, c2, c3, c4, c5, c6, c7, c8, c9,
              dict = dict,
              order = c("No. drafted"),
              drop = "log",
              headers = c("Baseline", "$\\times$ Rank", "$\\times$ Eng.",
                          "All", "Rank 1", "Rank 2+",
                          "All", "Rank 1", "Rank 2+"),
              se.below = TRUE, fitstat = ~n, tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var header rows after the first \midrule
midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_row <- " & \\multicolumn{3}{c}{No.\\ new hires} & \\multicolumn{3}{c}{No.\\ new female} & \\multicolumn{3}{c}{No.\\ new male} \\\\"
tc <- append(tc, c(depvar_row, "\\midrule"), after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{New Hiring in Response to Military Drafting}",
  "\\label{tab:reallocation-newhires}",
  "\\scriptsize",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: position $\\times$ kakari $\\times$ year (1938--1945). ",
         "Columns~(1)--(3): dependent variable is total new hires. ",
         "Columns~(4)--(6): new female hires (all, rank~1, rank~2+). ",
         "Columns~(7)--(9): new male hires (all, rank~1, rank~2+). ",
         "Column~(2) interacts the treatment with rank indicators (rank~1 = \\emph{yatoi}/\\emph{shokutaku}; ",
         "rank~3 = \\emph{shuji}/\\emph{gishi}; rank~2 is the omitted category). ",
         "Column~(3) interacts with an engineer indicator. ",
         "Rank and engineer main effects are absorbed by position fixed effects. ",
         "All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline (not shown). ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable1b_NewHires.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable1b_NewHires.tex"))
cat("\nTable 7 exported to NewTable1b_NewHires.tex\n")
