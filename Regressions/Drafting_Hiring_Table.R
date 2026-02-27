################################################################################
# Drafting & Hiring: Final Poisson/OLS Table
# Outputs a single table with specifications 1–7
################################################################################

library(tidyverse)
library(fixest)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

years_of_interest <- 1938:1945

# ============================================================
# 1. CUMULATIVE BASELINE & TRANSITIONS
# ============================================================

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  select(staff_id, year_num, lag_office = office_id, lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    )
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires        = sum(is_new_hire, na.rm = TRUE),
    n_new_hires_female = sum(is_new_hire & is_female, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 2. DRAFT COUNTS
# ============================================================

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_drafted      = n(),
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. OCCUPATION CLASSIFICATION
# ============================================================
# Engineer: 技 in position name
# 雇 (yato): 雇 or 傭
# Non-engineer: other

position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    occupation = case_when(
      str_detect(pos_norm, "技")           ~ "engineer",
      str_detect(pos_norm, "雇|傭")        ~ "yato",
      TRUE                                 ~ "non_engineer"
    ),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

# ============================================================
# 4. NEIGHBOR DRAFT MEASURES
# ============================================================

# 4a. Within 係 (kakari): total drafts in same kakari
kakari_totals <- position_panel %>%
  group_by(office_id, kakari, year_num) %>%
  summarise(kakari_total_drafts = sum(n_drafted), .groups = "drop")

# 4b. Within 係, by occupation (for "across occupation")
kakari_occ_totals <- position_panel %>%
  group_by(office_id, kakari, occupation, year_num) %>%
  summarise(kakari_occ_drafts = sum(n_drafted), .groups = "drop")

# 4c. Within 課 (office), by occupation (for "within occupation")
office_occ_totals <- position_panel %>%
  group_by(office_id, occupation, year_num) %>%
  summarise(office_occ_drafts = sum(n_drafted), .groups = "drop")

position_panel <- position_panel %>%
  left_join(kakari_totals, by = c("office_id", "kakari", "year_num")) %>%
  left_join(kakari_occ_totals, by = c("office_id", "kakari", "occupation", "year_num")) %>%
  left_join(office_occ_totals, by = c("office_id", "occupation", "year_num")) %>%
  mutate(
    # Within 係, across occupation: drafts from other occupation groups in same kakari
    adj_kakari_across_occ = kakari_total_drafts - kakari_occ_drafts,
    # Within 課, within occupation: drafts from same occupation in same office, excluding own
    adj_ka_within_occ     = office_occ_drafts - n_drafted,
    # Within 係, all neighbors (for spec 6)
    adj_kakari_all        = kakari_total_drafts - n_drafted
  )

# ============================================================
# 5. REGRESSIONS
# ============================================================
# Unit: kakari-level (office × kakari × pos_norm × year)
# FE: year + kyoku + pos_norm
# Cluster: office_id

# 1. Immediate effect, new hires (all) — Poisson
m1 <- fepois(
  n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 2. Immediate effect, female new hires — Poisson
m2 <- fepois(
  n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 2.5. Immediate effect, female new hires — OLS
m2_5 <- feols(
  n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 3. Neighboring (within 係, across occupation), new hires — Poisson
m3 <- fepois(
  n_new_hires ~ adj_kakari_across_occ + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 4. Neighboring (within 係, across occupation), female new hires — Poisson
m4 <- fepois(
  n_new_hires_female ~ adj_kakari_across_occ + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 5. Neighboring (within 課, within occupation), new hires — Poisson
m5 <- fepois(
  n_new_hires ~ adj_ka_within_occ + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# 7. Immediate effect by engineer vs non-engineer — Poisson
m7 <- fepois(
  n_new_hires ~ n_drafted_male * is_engineer + log(cumul_n_male + 1) | year_num + kyoku + pos_norm,
  data = position_panel, cluster = ~office_id
)

# ============================================================
# 6. OUTPUT TABLE
# ============================================================

dict <- c(
  n_drafted_male         = "Own drafts",
  adj_kakari_across_occ  = "Adj. (係, across occ)",
  adj_ka_within_occ      = "Adj. (課, within occ)",
  "log(cumul_n_male + 1)" = "log(M+1)",
  "n_drafted_male:is_engineer" = "Own × Engineer",
  is_engineer            = "Engineer"
)

cat("\n")
cat("===== DRAFTING & HIRING: POISSON/OLS TABLE =====\n\n")

etable(
  m1, m2, m2_5, m3, m4, m5, m7,
  dict = dict,
  headers = c(
    "(1)\nNew hires",
    "(2)\nFemale",
    "(2.5)\nFemale OLS",
    "(3)\nNew hires\n係 across",
    "(4)\nFemale\n係 across",
    "(5)\nNew hires\n課 within",
    "(6)\nNew x Eng."
  ),
  se.below = TRUE,
  fitstat = "n"
)

# Export to LaTeX (run from Regressions/; file goes to parent)
etable(m1, m2, m2_5, m3, m4, m5, m7, dict = dict,
       file = "../Drafting_Hiring_Table.tex", replace = TRUE,
       se.below = TRUE, fitstat = "n")
