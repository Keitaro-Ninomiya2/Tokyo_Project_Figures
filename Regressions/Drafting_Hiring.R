################################################################################
# First Stage Investigation: Mobilization & Labor Substitution (OCR-Robust)
################################################################################

library(tidyverse)
library(fixest)
library(here)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- here::here(
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

# Load named staff and pre-process
# Note: Ensure 'kyoku' is selected and preserved here
df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

# Load full master (for draft counts)
df_all <- read_csv(DATA_PATH,
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

# ============================================================
# 1. CUMULATIVE BASELINE (Robust to OCR errors)
# ============================================================

years_of_interest <- 1938:1945

cat("Calculating cumulative male baselines...\n")
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(
      cumul_n_male = n_distinct(staff_id),
      .groups = "drop"
    ) %>%
    mutate(year_num = yr)
})

# ============================================================
# 2. TRANSITIONS AND OUTCOMES
# ============================================================

staff_lag <- df %>%
  select(staff_id, year_num, 
         lag_office = office_id, 
         lag_kakari = kakari, 
         lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = is.na(lag_office),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    is_poached_internal = !is.na(lag_office) & 
      (lag_office == office_id) & 
      (lag_kakari != kakari)
  )

# Aggregate outcomes - keep kyoku in the grouping
position_outcomes <- staff_transitions %>%
  group_by(kyoku, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires        = sum(is_new_hire, na.rm = TRUE),
    n_new_hires_female = sum(is_new_hire & is_female, na.rm = TRUE),
    n_transfers_in     = sum(is_transfer_in, na.rm = TRUE),
    n_poached_internal = sum(is_poached_internal, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. DRAFT COUNTS & PANEL MERGE
# ============================================================

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_drafted      = n(),
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    male_draft_share = ifelse(cumul_n_male > 0, n_drafted_male / cumul_n_male, 0),
    no_male_baseline = as.integer(cumul_n_male == 0)
  )

# Adjacent Shock calculation
office_totals <- position_panel %>%
  group_by(office_id, year_num) %>%
  summarise(office_total_drafts = sum(n_drafted), .groups = "drop")

position_panel <- position_panel %>%
  left_join(office_totals, by = c("office_id", "year_num")) %>%
  mutate(adjacent_drafts = office_total_drafts - n_drafted)

# ============================================================
# 4. DECOMPOSITION PREP
# ============================================================

# Define rank groups
staff_decomposed <- staff_transitions %>%
  mutate(
    rank_group = case_when(
      str_detect(position, "書記") ~ "shoki",
      str_detect(position, "技手") ~ "gishu",
      str_detect(position, "雇|傭")  ~ "yato",
      TRUE ~ "other"
    )
  ) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_new_male   = sum(is_new_hire & !is_female, na.rm = TRUE),
    n_new_shoki  = sum(is_new_hire & rank_group == "shoki", na.rm = TRUE),
    n_new_gishu  = sum(is_new_hire & rank_group == "gishu", na.rm = TRUE),
    n_new_yato   = sum(is_new_hire & rank_group == "yato", na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate to Office-Position-Year (Conditioning on kyoku)
pos_office_panel <- position_panel %>%
  group_by(kyoku, office_id, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(n_new_hires),
    n_new_hires_female = sum(n_new_hires_female),
    n_poached_internal = sum(n_poached_internal),
    n_drafted_male = sum(n_drafted_male),
    cumul_n_male = sum(cumul_n_male),
    adjacent_drafts = sum(adjacent_drafts),
    .groups = "drop"
  ) %>%
  left_join(staff_decomposed, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(across(starts_with("n_new_"), ~replace_na(., 0)))

# ============================================================
# 5. REGRESSIONS (Fixed Effects: kyoku)
# ============================================================

cat("\n===== DECOMPOSED POISSON MODELS (FE: Year + Kyoku + Pos) =====\n")

# Use fepois to handle count data and conditional fixed effects
# 1. Gender Split
m_male <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | 
                   year_num + kyoku + pos_norm, data = pos_office_panel, cluster = ~office_id)

m_female <- fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) | 
                     year_num + kyoku + pos_norm, data = pos_office_panel, cluster = ~office_id)

# 2. Rank Decomposition
m_shoki <- fepois(n_new_shoki ~ n_drafted_male + log(cumul_n_male + 1) | 
                    year_num + kyoku + pos_norm, data = pos_office_panel, cluster = ~office_id)

m_gishu <- fepois(n_new_gishu ~ n_drafted_male + log(cumul_n_male + 1) | 
                    year_num + kyoku + pos_norm, data = pos_office_panel, cluster = ~office_id)

m_yato  <- fepois(n_new_yato ~ n_drafted_male + log(cumul_n_male + 1) | 
                    year_num + kyoku + pos_norm, data = pos_office_panel, cluster = ~office_id)

etable(m_male, m_female, m_shoki, m_gishu, m_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)