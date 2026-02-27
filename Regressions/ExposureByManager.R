library(tidyverse)
library(fixest)
library(here)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num   = as.numeric(year),
    is_female  = gender_modern == "female",
    pos_norm   = str_replace_all(position, "\\s+", ""),
    is_kakacho = str_detect(pos_norm, "係長"),
    is_kacho   = str_detect(pos_norm, "課長")
  )

wartime_start <- 1937
wartime_end   <- 1945
postwar_start <- 1947

# ============================================================
# 1. WARTIME FEMALE EXPOSURE (position x kakari x year)
# ============================================================

wartime_pos_kakari_female <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(female_share = mean(is_female, na.rm = TRUE), .groups = "drop")

# Draft shock at position level (male drafts / male baseline)
wartime_pos_kakari_drafts <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_male        = sum(!is_female, na.rm = TRUE),
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share   = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    .groups = "drop"
  )

staff_wartime_cells <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  select(staff_id, office_id, ka, kakari, pos_norm, year_num) %>%
  inner_join(wartime_pos_kakari_female,
             by = c("office_id", "ka", "kakari", "pos_norm", "year_num")) %>%
  left_join(wartime_pos_kakari_drafts,
            by = c("office_id", "ka", "kakari", "pos_norm", "year_num"))

staff_exposure <- staff_wartime_cells %>%
  group_by(staff_id) %>%
  summarise(
    exposure_max  = max(female_share, na.rm = TRUE),
    exposure_mean = mean(female_share, na.rm = TRUE),
    .groups = "drop"
  )

cat("Staff with wartime exposure data:", nrow(staff_exposure), "\n")

# ============================================================
# 2. POSTWAR KAKARI PANEL
# ============================================================

kakari_outcome <- df %>%
  filter(year_num >= postwar_start) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    has_female  = as.integer(any(is_female, na.rm = TRUE)),
    n_female    = sum(is_female, na.rm = TRUE),
    kakari_size = n(),
    .groups = "drop"
  )

ka_controls <- df %>%
  filter(year_num >= postwar_start) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    ka_size        = n(),
    n_engineer     = sum(str_detect(pos_norm, "技師|技手|技術"), na.rm = TRUE),
    engineer_share = n_engineer / ka_size,
    n_kakacho_ka   = sum(is_kakacho, na.rm = TRUE),
    .groups = "drop"
  )

# Kakaricho exposure (aggregated to kakari level)
kakacho_exp_kakari <- df %>%
  filter(is_kakacho, year_num >= postwar_start) %>%
  select(staff_id, office_id, ka, kakari, year_num) %>%
  left_join(staff_exposure, by = "staff_id") %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    kakacho_exp_max  = mean(exposure_max, na.rm = TRUE),
    kakacho_exp_mean = mean(exposure_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Kacho exposure (aggregated to ka level, applied to all kakari)
kacho_exp_ka <- df %>%
  filter(is_kacho, year_num >= postwar_start) %>%
  select(staff_id, office_id, ka, year_num) %>%
  left_join(staff_exposure, by = "staff_id") %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    kacho_exp_max  = mean(exposure_max, na.rm = TRUE),
    kacho_exp_mean = mean(exposure_mean, na.rm = TRUE),
    .groups = "drop"
  )

ka_kyoku <- df %>%
  filter(year_num >= postwar_start) %>%
  count(office_id, ka, kyoku) %>%
  group_by(office_id, ka) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, ka, kyoku_modal = kyoku)

kakari_panel <- kakari_outcome %>%
  left_join(kakacho_exp_kakari,
            by = c("office_id", "ka", "kakari", "year_num")) %>%
  left_join(kacho_exp_ka,
            by = c("office_id", "ka", "year_num")) %>%
  left_join(ka_controls, by = c("office_id", "ka", "year_num")) %>%
  left_join(ka_kyoku, by = c("office_id", "ka"))

cat("\n--- Panel diagnostics ---\n")
cat("Total kakari-years:", nrow(kakari_panel), "\n")
cat("With kakaricho exposure (max):", sum(!is.na(kakari_panel$kakacho_exp_max)), "\n")
cat("With kacho exposure (max):", sum(!is.na(kakari_panel$kacho_exp_max)), "\n")

# ============================================================
# 3. REGRESSIONS
# ============================================================

est_kakacho <- kakari_panel %>% filter(!is.na(kakacho_exp_max))
est_kacho   <- kakari_panel %>% filter(!is.na(kacho_exp_max))

cat("Kakaricho est. sample:", nrow(est_kakacho), "\n")
cat("Kacho est. sample:",     nrow(est_kacho), "\n")

# Col 1: kakaricho max exposure
lpm1 <- feols(
  has_female ~ kakacho_exp_max + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kakacho, cluster = ~office_id
)

# Col 2: kakaricho mean exposure
lpm2 <- feols(
  has_female ~ kakacho_exp_mean + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kakacho, cluster = ~office_id
)

# Col 3: kacho max exposure
lpm3 <- feols(
  has_female ~ kacho_exp_max + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kacho, cluster = ~office_id
)

# Col 4-6: number of females as dependent variable
lpm4 <- feols(
  n_female ~ kakacho_exp_max + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kakacho, cluster = ~office_id
)

lpm5 <- feols(
  n_female ~ kakacho_exp_mean + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kakacho, cluster = ~office_id
)

lpm6 <- feols(
  n_female ~ kacho_exp_max + engineer_share +
    n_kakacho_ka + kakari_size |
    kyoku_modal,
  data = est_kacho, cluster = ~office_id
)

cat("\n========== PANEL A: HAS FEMALE (BINARY) ==========\n")
etable(
  lpm1, lpm2, lpm3,
  headers  = c("Kakaricho (Max)", "Kakaricho (Mean)", "Kacho (Max)"),
  order    = c("kakacho_exp_max", "kakacho_exp_mean", "kacho_exp_max"),
  se.below = TRUE
)

cat("\n========== PANEL B: NUMBER OF FEMALES ==========\n")
etable(
  lpm4, lpm5, lpm6,
  headers  = c("Kakaricho (Max)", "Kakaricho (Mean)", "Kacho (Max)"),
  order    = c("kakacho_exp_max", "kakacho_exp_mean", "kacho_exp_max"),
  se.below = TRUE
)
