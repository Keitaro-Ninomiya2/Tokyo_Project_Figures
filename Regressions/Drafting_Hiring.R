################################################################################
# First Stage Investigation: Mobilization & Labor Substitution (OCR-Robust)
################################################################################

library(tidyverse)
library(fixest)
library(here)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

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
# New hire = first appearance ever in the data (not just absent in t-1)
# NA in initial year of each entity (1938 Tokyo Fu, 1937 Tokyo-Shi): can't distinguish new hires

# Initial year per office (proxy for entity: Tokyo-Shi offices start 1937, Tokyo Fu 1938)
office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  select(staff_id, year_num,
         lag_office = office_id,
         lag_kakari = kakari,
         lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    # NA when in entity's initial year (data started then—can't know if new hire)
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    is_poached_internal = !is.na(lag_office) &
      (lag_office == office_id) &
      (lag_kakari != kakari)
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires        = sum(is_new_hire, na.rm = TRUE),
    n_new_hires_female = sum(is_new_hire & is_female, na.rm = TRUE),
    n_new_hires_male   = sum(is_new_hire & !is_female, na.rm = TRUE),
    n_transfers_in     = sum(is_transfer_in, na.rm = TRUE),
    n_poached_internal = sum(is_poached_internal, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. DRAFT COUNTS & KAKARI-LEVEL PANEL
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

# Adjacent Shock calculation (neighboring positions = other kakari in same office)
office_totals <- position_panel %>%
  group_by(office_id, year_num) %>%
  summarise(office_total_drafts = sum(n_drafted), .groups = "drop")

position_panel <- position_panel %>%
  left_join(office_totals, by = c("office_id", "year_num")) %>%
  mutate(adjacent_drafts = office_total_drafts - n_drafted)

# Rank category (官吏 vs 公吏) per TimeSeriesChihouKomuin.R
prewar_elite_pat <- "^主事$|^技師$|^視学$|^理事$"
position_panel <- position_panel %>%
  mutate(
    is_elite_title = year_num <= 1945 & str_detect(pos_norm, prewar_elite_pat),
    rank_category  = if_else(is_elite_title, "官吏", "公吏"),
    is_kanri       = as.integer(rank_category == "官吏")
  )

# ============================================================
# 4. DECOMPOSITION PREP
# ============================================================

# --- 4a. Kakari-level decomposition (for kakari-level OLS) ---
staff_decomposed_kakari <- staff_transitions %>%
  mutate(
    rank_group = case_when(
      str_detect(position, "書記") ~ "shoki",
      str_detect(position, "技手") ~ "gishu",
      str_detect(position, "雇|傭")  ~ "yato",
      TRUE ~ "other"
    )
  ) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_male   = sum(is_new_hire & !is_female, na.rm = TRUE),
    n_new_shoki  = sum(is_new_hire & rank_group == "shoki", na.rm = TRUE),
    n_new_gishu  = sum(is_new_hire & rank_group == "gishu", na.rm = TRUE),
    n_new_yato   = sum(is_new_hire & rank_group == "yato", na.rm = TRUE),
    .groups = "drop"
  )

kakari_panel <- position_panel %>%
  left_join(staff_decomposed_kakari,
            by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(across(starts_with("n_new_"), ~replace_na(., 0)))

# --- 4b. Office-position-level decomposition (for Poisson) ---
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

pos_office_panel <- position_panel %>%
  group_by(kyoku, office_id, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(n_new_hires),
    n_new_hires_female = sum(n_new_hires_female),
    n_poached_internal = sum(n_poached_internal),
    n_drafted_male = sum(n_drafted_male),
    cumul_n_male = sum(cumul_n_male),
    rank_category = first(rank_category),
    is_kanri = first(is_kanri),
    .groups = "drop"
  ) %>%
  left_join(office_totals, by = c("office_id", "year_num")) %>%
  mutate(adjacent_drafts = office_total_drafts - n_drafted_male) %>%
  left_join(staff_decomposed, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(across(starts_with("n_new_"), ~replace_na(., 0)))

# --- 4c. Office-year panel (most power for OLS) ---
office_year_panel <- position_panel %>%
  left_join(staff_decomposed_kakari,
            by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(across(starts_with("n_new_"), ~replace_na(., 0))) %>%
  group_by(kyoku, office_id, year_num) %>%
  summarise(
    n_new_hires        = sum(n_new_hires),
    n_new_hires_female = sum(n_new_hires_female),
    n_new_hires_male   = sum(n_new_hires_male),
    n_new_male         = sum(n_new_male),
    n_new_shoki        = sum(n_new_shoki),
    n_new_gishu        = sum(n_new_gishu),
    n_new_yato         = sum(n_new_yato),
    n_poached_internal = sum(n_poached_internal),
    n_drafted_male     = sum(n_drafted_male),
    cumul_n_male       = sum(cumul_n_male),
    adjacent_drafts    = sum(adjacent_drafts),
    .groups = "drop"
  )

# ============================================================
# 5. DIAGNOSTICS
# ============================================================

cat("\n===== PANEL DIAGNOSTICS =====\n")

cat("\nKakari-level panel:\n")
kakari_panel %>%
  summarise(
    n_obs = n(),
    n_with_drafts = sum(n_drafted_male > 0),
    pct_zero_new_male = mean(n_new_male == 0)
  ) %>% print()

cat("\nOffice-position panel:\n")
pos_office_panel %>%
  summarise(
    n_obs = n(),
    n_with_drafts = sum(n_drafted_male > 0),
    pct_zero_new_male = mean(n_new_male == 0)
  ) %>% print()

cat("\nOffice-year panel:\n")
office_year_panel %>%
  summarise(
    n_obs = n(),
    n_with_drafts = sum(n_drafted_male > 0),
    pct_zero_new_male = mean(n_new_male == 0)
  ) %>% print()

cat("\nRank category (官吏 vs 公吏) distribution:\n")
pos_office_panel %>%
  count(rank_category) %>%
  print()

# ============================================================
# 6. POISSON MODELS (Office-Position-Year)
# ============================================================
# 6a. IMMEDIATE EFFECT: drafting in own position
# 6b. NEIGHBORING POSITIONS: drafting in other positions within same office
# 6c. BOTH effects
# 6d. HETEROGENEITY by position rank (官吏 vs 公吏)

cat("\n===== 6a. IMMEDIATE EFFECT (Own Position's Drafts) =====\n")

m_poi_male <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) |
                       year_num + kyoku + pos_norm,
                     data = pos_office_panel, cluster = ~office_id)

m_poi_female <- fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                         year_num + kyoku + pos_norm,
                       data = pos_office_panel, cluster = ~office_id)

m_poi_shoki <- fepois(n_new_shoki ~ n_drafted_male + log(cumul_n_male + 1) |
                        year_num + kyoku + pos_norm,
                      data = pos_office_panel, cluster = ~office_id)

m_poi_gishu <- fepois(n_new_gishu ~ n_drafted_male + log(cumul_n_male + 1) |
                        year_num + kyoku + pos_norm,
                      data = pos_office_panel, cluster = ~office_id)

m_poi_yato <- fepois(n_new_yato ~ n_drafted_male + log(cumul_n_male + 1) |
                       year_num + kyoku + pos_norm,
                     data = pos_office_panel, cluster = ~office_id)

cat("\n6a. Immediate (Own Position) Results:\n")
cat(capture.output(etable(m_poi_male, m_poi_female, m_poi_shoki, m_poi_gishu, m_poi_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 6b. NEIGHBORING POSITIONS (Spillover Within Office) =====\n")

m_poi_adj_male <- fepois(n_new_male ~ adjacent_drafts + log(cumul_n_male + 1) |
                           year_num + kyoku + pos_norm,
                         data = pos_office_panel, cluster = ~office_id)

m_poi_adj_female <- fepois(n_new_hires_female ~ adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku + pos_norm,
                           data = pos_office_panel, cluster = ~office_id)

m_poi_adj_shoki <- fepois(n_new_shoki ~ adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

m_poi_adj_gishu <- fepois(n_new_gishu ~ adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

m_poi_adj_yato <- fepois(n_new_yato ~ adjacent_drafts + log(cumul_n_male + 1) |
                           year_num + kyoku + pos_norm,
                         data = pos_office_panel, cluster = ~office_id)

cat("\n6b. Neighboring (Spillover) Results:\n")
cat(capture.output(etable(m_poi_adj_male, m_poi_adj_female, m_poi_adj_shoki, m_poi_adj_gishu, m_poi_adj_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 6c. BOTH: Immediate + Neighboring =====\n")

m_poi_both_male <- fepois(n_new_male ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

m_poi_both_female <- fepois(n_new_hires_female ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                              year_num + kyoku + pos_norm,
                            data = pos_office_panel, cluster = ~office_id)

m_poi_both_shoki <- fepois(n_new_shoki ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku + pos_norm,
                           data = pos_office_panel, cluster = ~office_id)

m_poi_both_gishu <- fepois(n_new_gishu ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku + pos_norm,
                           data = pos_office_panel, cluster = ~office_id)

m_poi_both_yato <- fepois(n_new_yato ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

cat("\n6c. Both Effects Results:\n")
cat(capture.output(etable(m_poi_both_male, m_poi_both_female, m_poi_both_shoki, m_poi_both_gishu, m_poi_both_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 6d. HETEROGENEITY BY POSITION RANK (官吏 vs 公吏) =====\n")

m_poi_het_male <- fepois(n_new_male ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                           year_num + kyoku + pos_norm,
                         data = pos_office_panel, cluster = ~office_id)

m_poi_het_female <- fepois(n_new_hires_female ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                             year_num + kyoku + pos_norm,
                           data = pos_office_panel, cluster = ~office_id)

m_poi_het_shoki <- fepois(n_new_shoki ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

m_poi_het_gishu <- fepois(n_new_gishu ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                            year_num + kyoku + pos_norm,
                          data = pos_office_panel, cluster = ~office_id)

m_poi_het_yato <- fepois(n_new_yato ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                           year_num + kyoku + pos_norm,
                         data = pos_office_panel, cluster = ~office_id)

cat("\n6d. Heterogeneity by Rank (官吏=1, 公吏=0):\n")
cat(capture.output(etable(m_poi_het_male, m_poi_het_female, m_poi_het_shoki, m_poi_het_gishu, m_poi_het_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

# ============================================================
# 7. OLS MODELS — KAKARI LEVEL
# ============================================================

cat("\n===== 7a. OLS KAKARI — IMMEDIATE (Own Position) =====\n")

m_ols_k_male <- feols(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) |
                         year_num + kyoku^pos_norm,
                       data = kakari_panel, cluster = ~office_id)

m_ols_k_female <- feols(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                           year_num + kyoku^pos_norm,
                         data = kakari_panel, cluster = ~office_id)

m_ols_k_shoki <- feols(n_new_shoki ~ n_drafted_male + log(cumul_n_male + 1) |
                          year_num + kyoku^pos_norm,
                        data = kakari_panel, cluster = ~office_id)

m_ols_k_gishu <- feols(n_new_gishu ~ n_drafted_male + log(cumul_n_male + 1) |
                          year_num + kyoku^pos_norm,
                        data = kakari_panel, cluster = ~office_id)

m_ols_k_yato <- feols(n_new_yato ~ n_drafted_male + log(cumul_n_male + 1) |
                         year_num + kyoku^pos_norm,
                       data = kakari_panel, cluster = ~office_id)

cat("\nOLS Kakari-Level Results (7a Immediate):\n")
cat(capture.output(etable(m_ols_k_male, m_ols_k_female, m_ols_k_shoki, m_ols_k_gishu, m_ols_k_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 7b. OLS KAKARI — NEIGHBORING (Spillover) =====\n")

m_ols_k_adj_male <- feols(n_new_male ~ adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku^pos_norm,
                          data = kakari_panel, cluster = ~office_id)

m_ols_k_adj_female <- feols(n_new_hires_female ~ adjacent_drafts + log(cumul_n_male + 1) |
                              year_num + kyoku^pos_norm,
                            data = kakari_panel, cluster = ~office_id)

m_ols_k_adj_shoki <- feols(n_new_shoki ~ adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

m_ols_k_adj_gishu <- feols(n_new_gishu ~ adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

m_ols_k_adj_yato <- feols(n_new_yato ~ adjacent_drafts + log(cumul_n_male + 1) |
                            year_num + kyoku^pos_norm,
                          data = kakari_panel, cluster = ~office_id)

cat("\n7b. Neighboring Results:\n")
cat(capture.output(etable(m_ols_k_adj_male, m_ols_k_adj_female, m_ols_k_adj_shoki, m_ols_k_adj_gishu, m_ols_k_adj_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 7c. OLS KAKARI — BOTH EFFECTS =====\n")

m_ols_k_both_male <- feols(n_new_male ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

m_ols_k_both_female <- feols(n_new_hires_female ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                               year_num + kyoku^pos_norm,
                             data = kakari_panel, cluster = ~office_id)

m_ols_k_both_shoki <- feols(n_new_shoki ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                              year_num + kyoku^pos_norm,
                            data = kakari_panel, cluster = ~office_id)

m_ols_k_both_gishu <- feols(n_new_gishu ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                              year_num + kyoku^pos_norm,
                            data = kakari_panel, cluster = ~office_id)

m_ols_k_both_yato <- feols(n_new_yato ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

cat("\n7c. Both Effects Results:\n")
cat(capture.output(etable(m_ols_k_both_male, m_ols_k_both_female, m_ols_k_both_shoki, m_ols_k_both_gishu, m_ols_k_both_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

cat("\n===== 7d. OLS KAKARI — HETEROGENEITY BY RANK (官吏 vs 公吏) =====\n")

m_ols_k_het_male <- feols(n_new_male ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                            year_num + kyoku^pos_norm,
                          data = kakari_panel, cluster = ~office_id)

m_ols_k_het_female <- feols(n_new_hires_female ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                              year_num + kyoku^pos_norm,
                            data = kakari_panel, cluster = ~office_id)

m_ols_k_het_shoki <- feols(n_new_shoki ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

m_ols_k_het_gishu <- feols(n_new_gishu ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                             year_num + kyoku^pos_norm,
                           data = kakari_panel, cluster = ~office_id)

m_ols_k_het_yato <- feols(n_new_yato ~ n_drafted_male * is_kanri + log(cumul_n_male + 1) |
                            year_num + kyoku^pos_norm,
                          data = kakari_panel, cluster = ~office_id)

cat("\n7d. Heterogeneity Results:\n")
cat(capture.output(etable(m_ols_k_het_male, m_ols_k_het_female, m_ols_k_het_shoki, m_ols_k_het_gishu, m_ols_k_het_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

# ============================================================
# 8. OLS MODELS — OFFICE-YEAR LEVEL
# ============================================================

cat("\n===== OLS MODELS — OFFICE-YEAR LEVEL (FE: Year + Kyoku) =====\n")

m_ols_o_male <- feols(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) |
                         year_num + kyoku,
                       data = office_year_panel, cluster = ~office_id)

m_ols_o_female <- feols(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                           year_num + kyoku,
                         data = office_year_panel, cluster = ~office_id)

m_ols_o_shoki <- feols(n_new_shoki ~ n_drafted_male + log(cumul_n_male + 1) |
                          year_num + kyoku,
                        data = office_year_panel, cluster = ~office_id)

m_ols_o_gishu <- feols(n_new_gishu ~ n_drafted_male + log(cumul_n_male + 1) |
                          year_num + kyoku,
                        data = office_year_panel, cluster = ~office_id)

m_ols_o_yato <- feols(n_new_yato ~ n_drafted_male + log(cumul_n_male + 1) |
                         year_num + kyoku,
                       data = office_year_panel, cluster = ~office_id)

cat("\nOLS Office-Year Results:\n")
cat(capture.output(etable(m_ols_o_male, m_ols_o_female, m_ols_o_shoki, m_ols_o_gishu, m_ols_o_yato,
       headers = c("New Male", "New Female", "Shoki", "Gishu", "Yato"),
       se.below = TRUE)), sep = "\n")

# ============================================================
# 9. OLS RATE MODELS — OFFICE-YEAR LEVEL
# ============================================================

cat("\n===== OLS RATE MODELS — OFFICE-YEAR LEVEL =====\n")

office_year_rates <- office_year_panel %>%
  filter(cumul_n_male > 0) %>%
  mutate(
    draft_rate       = n_drafted_male / cumul_n_male,
    hire_rate_male   = n_new_male / cumul_n_male,
    hire_rate_female = n_new_hires_female / cumul_n_male,
    hire_rate_shoki  = n_new_shoki / cumul_n_male,
    hire_rate_gishu  = n_new_gishu / cumul_n_male,
    hire_rate_yato   = n_new_yato / cumul_n_male
  )

m_rate_male <- feols(hire_rate_male ~ draft_rate | year_num + kyoku,
                     data = office_year_rates, cluster = ~office_id)

m_rate_female <- feols(hire_rate_female ~ draft_rate | year_num + kyoku,
                       data = office_year_rates, cluster = ~office_id)

m_rate_shoki <- feols(hire_rate_shoki ~ draft_rate | year_num + kyoku,
                      data = office_year_rates, cluster = ~office_id)

m_rate_gishu <- feols(hire_rate_gishu ~ draft_rate | year_num + kyoku,
                      data = office_year_rates, cluster = ~office_id)

m_rate_yato <- feols(hire_rate_yato ~ draft_rate | year_num + kyoku,
                     data = office_year_rates, cluster = ~office_id)

cat("\nOLS Rate Results:\n")
cat(capture.output(etable(m_rate_male, m_rate_female, m_rate_shoki, m_rate_gishu, m_rate_yato,
       headers = c("Male Rate", "Female Rate", "Shoki Rate", "Gishu Rate", "Yato Rate"),
       se.below = TRUE)), sep = "\n")