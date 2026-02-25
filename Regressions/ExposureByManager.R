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

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

# ============================================================
# 1. IDENTIFY 課長
# ============================================================

df <- df %>%
  mutate(
    is_kacho = str_detect(pos_norm, "課長")
  )

kacho_year_post <- df %>%
  filter(is_kacho, year >= 1944) %>%
  select(staff_id, office_id, kyoku, year_num)

cat("Unique 課長 Post war:", n_distinct(kacho_year_post$staff_id), "\n")

# ============================================================
# 2. BUILD POST-1950 OUTCOME (Female 管理職)
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
# 3a. CONSTRUCT WARTIME EXPOSURE FOR EACH 課長
# ============================================================

wartime_female_share <- df %>%
  filter(year_num >= 1937, year_num <= 1945) %>%
  group_by(office_id, year_num, pos_norm) %>%
  summarise(
    female_share = mean(is_female, na.rm = TRUE),
    female_sum = sum(is_female, na.rm = TRUE),
    .groups = "drop"
  )

kacho_wartime <- df %>%
  filter(year_num >= 1937, year_num <= 1945) %>%
  select(staff_id, office_id, year_num, pos_norm) %>%
  inner_join(wartime_female_share, by = c("office_id", "year_num", 'pos_norm'))

kacho_exposure <- kacho_wartime %>%
  group_by(staff_id) %>%
  summarise(
    mgr_exposure     = mean(female_share, na.rm = TRUE),
    mgr_exposure_sum = mean(female_sum, na.rm = TRUE),
    mgr_any_exposure = as.integer(any(female_share > 0)),
    .groups = "drop"
  )

cat("Managers with wartime exposure data:", nrow(kacho_exposure), "\n")

wartime_ids <- df %>%
  filter(year_num <= 1945) %>%
  distinct(staff_id)

# ============================================================
# 3b. OFFICE-LEVEL CONTROLS (post-1950)
# ============================================================

office_controls <- df %>%
  filter(year_num >= 1950) %>%
  group_by(office_id, year_num) %>%
  summarise(
    office_size      = n(),
    female_share_all = mean(is_female, na.rm = TRUE),
    n_engineer       = sum(str_detect(pos_norm, "技師|技手|技術"), na.rm = TRUE),
    engineer_share   = n_engineer / office_size,
    is_eng_office    = as.integer(engineer_share > 0.1),
    .groups = "drop"
  ) %>%
  mutate(
    log_office_size = log(office_size)
  )

# ============================================================
# 3c. OFFICE-TO-KYOKU MAPPING (modal kyoku per office in post-1950)
# ============================================================

office_kyoku <- df %>%
  filter(year_num >= 1950) %>%
  count(office_id, kyoku) %>%
  group_by(office_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, kyoku_modal = kyoku)

# ============================================================
# 4. BUILD POST-1950 OFFICE-YEAR MANAGER PANEL
# ============================================================

post1950_kacho <- kacho_year_post %>%
  filter(year_num >= 1950) %>%
  left_join(kacho_exposure, by = "staff_id") %>%
  mutate(
    mgr_wartime      = as.integer(staff_id %in% wartime_ids$staff_id),
    mgr_exposure     = replace_na(mgr_exposure, 0),
    mgr_any_exposure = replace_na(mgr_any_exposure, 0)
  )

manager_panel <- post1950_kacho %>%
  group_by(office_id, year_num) %>%
  summarise(
    mgr_exposure     = mean(mgr_exposure),
    mgr_any_exposure = mean(mgr_any_exposure),
    mgr_wartime      = mean(mgr_wartime),
    .groups = "drop"
  ) %>%
  left_join(outcome, by = c("office_id", "year_num")) %>%
  left_join(office_controls, by = c("office_id", "year_num")) %>%
  left_join(office_kyoku, by = "office_id") %>%
  mutate(
    log_n_kanri = log(pmax(n_kanri, 1))
  )

cat("Office-years with manager data:", nrow(manager_panel), "\n")
cat("Unique kyoku:", n_distinct(manager_panel$kyoku_modal), "\n")

# Diagnostics
cat("\n--- Descriptive stats ---\n")

postwar_mgrs_with_wartime_exp <- post1950_kacho %>%
  filter(mgr_wartime == 1) %>%
  summarise(n_unique_mgrs = n_distinct(staff_id))
print(postwar_mgrs_with_wartime_exp)

share_analysis <- post1950_kacho %>%
  summarise(
    total_postwar_mgrs   = n_distinct(staff_id),
    wartime_veteran_mgrs = n_distinct(staff_id[mgr_wartime == 1]),
    share_veterans       = (wartime_veteran_mgrs / total_postwar_mgrs) * 100
  )
print(share_analysis)

# Within-kyoku variation in exposure
within_var_kyoku <- manager_panel %>%
  group_by(kyoku_modal) %>%
  summarise(
    n_offices = n_distinct(office_id),
    sd_exp    = sd(mgr_exposure, na.rm = TRUE),
    .groups   = "drop"
  )
cat("\nWithin-kyoku SD of mgr_exposure:\n")
print(summary(within_var_kyoku$sd_exp))

# ============================================================
# 5. REGRESSIONS
# ============================================================

# --- Original specs ---

mgr_ols1 <- feols(
  has_female_kanri ~ mgr_exposure | year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

mgr_ols2 <- feols(
  has_female_kanri ~ mgr_exposure + log_n_kanri | year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

mgr_ols3 <- feols(
  has_female_kanri ~ mgr_any_exposure + log_n_kanri | year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

mgr_ols4 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri | year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

# --- Augmented specs ---

# + office size
mgr_ols5 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size |
    year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

# + engineer share (continuous)
mgr_ols6 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size +
    engineer_share |
    year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

# + engineer office dummy
mgr_ols7 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size +
    is_eng_office |
    year_num + kyoku_modal,
  data = manager_panel, cluster = ~office_id
)

# + contemporary female share (bad-control caveat — robustness only)
mgr_ols8 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size +
    engineer_share + female_share_all |
    year_num + kyoku_modal,
  data = manager_panel, cluster = ~kyoku_modal
)

# Kyoku FE (replaces office FE — enough cross-office variation within kyoku)
mgr_ols9 <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size +
    engineer_share |
    kyoku_modal + year_num,
  data = manager_panel, cluster = ~kyoku_modal
)

# ============================================================
# 6. OUTPUT
# ============================================================

cat("\n========== ORIGINAL SPECIFICATIONS ==========\n")
etable(
  mgr_ols1, mgr_ols2, mgr_ols3, mgr_ols4,
  headers  = c("Exposure", "+Size", "Binary+Size", "+Wartime"),
  se.below = TRUE
)

cat("\n========== AUGMENTED SPECIFICATIONS ==========\n")
etable(
  mgr_ols4, mgr_ols5, mgr_ols6, mgr_ols7, mgr_ols8, mgr_ols9,
  headers  = c("Baseline", "+OffSize", "+EngShare", "+EngDummy",
               "+FemShare", "Kyoku FE"),
  se.below = TRUE
)

summary(mgr_ols6)
summary(mgr_ols9)

mgr_ols9_office <- feols(
  has_female_kanri ~ mgr_exposure + mgr_wartime + log_n_kanri + log_office_size +
    engineer_share |
    kyoku_modal + year_num,
  data = manager_panel, cluster = ~office_id
)

summary(mgr_ols9_office)
