################################################################################
# COMBINED IV REGRESSION: Wartime Female Share (instrumented by draft intensity)
# on Post‑1950 Female Managerial Appointments
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

# Load named staff (for transitions, outcomes)
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
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

# ============================================================
# 1. WARTIME VARIABLES: Female share & Draft intensity
#    (following Drafting_Hiring.R, but aggregated to office-year)
# ============================================================

years_wartime <- 1937:1945

# ---- Cumulative male baseline (pre-war stock) ----
cat("Calculating cumulative male baselines...\n")
cumul_male_stock <- map_dfr(years_wartime, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(
      cumul_n_male = n_distinct(staff_id),
      .groups = "drop"
    ) %>%
    mutate(year_num = yr)
})

# ---- Draft counts (male) ----
draft_counts <- df_all %>%
  filter(year_num %in% years_wartime, drafted == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Merge to get draft share ----
wartime_panel <- cumul_male_stock %>%
  left_join(draft_counts, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    male_draft_share = ifelse(cumul_n_male > 0, n_drafted_male / cumul_n_male, 0)
  ) %>%
  select(office_id, pos_norm, year_num, cumul_n_male, n_drafted_male, male_draft_share)

# ---- Female share during war ----
wartime_female_share <- df %>%
  filter(year_num %in% years_wartime) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    female_share = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

# Combine both measures
wartime_measures <- wartime_panel %>%
  left_join(wartime_female_share, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(female_share = replace_na(female_share, 0))

# ============================================================
# 2. IDENTIFY MANAGERS (課長) AND THEIR WARTIME POSITIONS
#    (following ExposureByManager.R)
# ============================================================

df <- df %>%
  mutate(is_kacho = str_detect(pos_norm, "課長"))

# Managers active post‑war (1944+ to capture those who appear after war)
kacho_post <- df %>%
  filter(is_kacho, year_num >= 1944) %>%
  select(staff_id, office_id, kyoku, year_num)

# Keep only managers who also appear during wartime (to have exposure)
wartime_ids <- df %>%
  filter(year_num <= 1945) %>%
  distinct(staff_id)

kacho_wartime <- df %>%
  filter(year_num %in% years_wartime, staff_id %in% wartime_ids$staff_id) %>%
  select(staff_id, office_id, year_num, pos_norm)

# Merge wartime measures (female share and draft share) to each manager-year
kacho_wartime_merged <- kacho_wartime %>%
  left_join(wartime_measures,
            by = c("office_id", "pos_norm", "year_num"))

# Summarise at manager level: average exposure and average draft intensity
manager_exposure <- kacho_wartime_merged %>%
  group_by(staff_id) %>%
  summarise(
    mgr_exposure         = mean(female_share, na.rm = TRUE),
    mgr_draft_intensity  = mean(male_draft_share, na.rm = TRUE),
    mgr_any_exposure     = as.integer(any(female_share > 0)),
    mgr_wartime          = 1L,   # all these managers served during war
    .groups = "drop"
  ) %>%
  mutate(across(c(mgr_exposure, mgr_draft_intensity), ~ replace_na(., 0)))

cat("Managers with wartime exposure data:", nrow(manager_exposure), "\n")

# ============================================================
# 3. POST‑1950 OUTCOMES (female managerial appointments)
# ============================================================

outcome <- df %>%
  mutate(
    is_elite = case_when(
      year_num <= 1945 ~ str_detect(pos_norm, "^主事$|^技師$|^視学$|^理事$"),
      year_num >  1945 ~ str_detect(pos_norm, "長"),
      TRUE ~ FALSE
    )
  ) %>%
  filter(year_num >= 1950) %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_kanri          = sum(is_elite),
    n_female_kanri   = sum(is_elite & is_female),
    has_female_kanri = as.integer(n_female_kanri > 0),
    .groups = "drop"
  )

# ============================================================
# 4. OFFICE‑LEVEL CONTROLS (post‑1950)
# ============================================================

office_controls <- df %>%
  filter(year_num >= 1950) %>%
  group_by(office_id, year_num) %>%
  summarise(
    office_size      = n(),
    female_share_all = mean(is_female, na.rm = TRUE),
    n_engineer       = sum(str_detect(pos_norm, "技師|技手|技術"), na.rm = TRUE),
    engineer_share   = n_engineer / office_size,
    .groups = "drop"
  ) %>%
  mutate(log_office_size = log(office_size))

# Modal kyoku for each office (post‑1950)
office_kyoku <- df %>%
  filter(year_num >= 1950) %>%
  count(office_id, kyoku) %>%
  group_by(office_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, kyoku_modal = kyoku)

# ============================================================
# 5. BUILD OFFICE‑YEAR MANAGER PANEL WITH INSTRUMENT
# ============================================================

# Managers active post‑1950
post1950_kacho <- kacho_post %>%
  filter(year_num >= 1950) %>%
  left_join(manager_exposure, by = "staff_id")

# Aggregate to office‑year: average exposure and average instrument
manager_panel <- post1950_kacho %>%
  group_by(office_id, year_num) %>%
  summarise(
    mgr_exposure        = mean(mgr_exposure, na.rm = TRUE),
    mgr_draft_intensity = mean(mgr_draft_intensity, na.rm = TRUE),
    mgr_any_exposure    = mean(mgr_any_exposure, na.rm = TRUE),
    mgr_wartime         = mean(mgr_wartime, na.rm = TRUE),  # should be 1 for all
    n_managers          = n(),
    .groups = "drop"
  ) %>%
  left_join(outcome, by = c("office_id", "year_num")) %>%
  left_join(office_controls, by = c("office_id", "year_num")) %>%
  left_join(office_kyoku, by = "office_id") %>%
  mutate(
    log_n_kanri = log(pmax(n_kanri, 1)),
    # Fill missing values (offices without managers in a given year)
    across(c(mgr_exposure, mgr_draft_intensity, mgr_any_exposure, mgr_wartime),
           ~ replace_na(., 0))
  )

cat("Office‑years in panel:", nrow(manager_panel), "\n")
cat("Unique kyoku:", n_distinct(manager_panel$kyoku_modal), "\n")

# ============================================================
# 6. INSTRUMENTAL VARIABLES REGRESSIONS
# ============================================================

# ---- First stage: mgr_exposure ~ mgr_draft_intensity + FE ----
first_stage <- feols(
  mgr_exposure ~ mgr_draft_intensity + log_n_kanri + log_office_size + engineer_share
  | kyoku_modal + year_num,
  data = manager_panel,
  cluster = ~office_id
)
cat("\n===== FIRST STAGE =====\n")
etable(first_stage)

# ---- Reduced form: outcome ~ instrument + FE ----
reduced_form <- feols(
  has_female_kanri ~ mgr_draft_intensity + log_n_kanri + log_office_size + engineer_share
  | kyoku_modal + year_num,
  data = manager_panel,
  cluster = ~office_id
)
cat("\n===== REDUCED FORM =====\n")
etable(reduced_form)

# ---- IV (2SLS) ----
iv_model <- feols(
  has_female_kanri ~ log_n_kanri + log_office_size + engineer_share
  | kyoku_modal + year_num
  | mgr_exposure ~ mgr_draft_intensity,
  data = manager_panel,
  cluster = ~office_id
)
cat("\n===== IV (2SLS) =====\n")
etable(iv_model)

# ---- Compare with OLS (as in ExposureByManager.R) ----
ols_model <- feols(
  has_female_kanri ~ mgr_exposure + log_n_kanri + log_office_size + engineer_share
  | kyoku_modal + year_num,
  data = manager_panel,
  cluster = ~office_id
)
cat("\n===== OLS (for comparison) =====\n")
etable(ols_model)

# ---- Output all together ----
cat("\n========== SUMMARY ==========\n")
etable(first_stage, reduced_form, iv_model, ols_model,
       headers = c("First Stage", "Reduced Form", "IV (2SLS)", "OLS"),
       se.below = TRUE,
       fitstat = c("ivf", "ivwald", "ar"))