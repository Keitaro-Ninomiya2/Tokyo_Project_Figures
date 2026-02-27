################################################################################
# Direct Draft Shock IV: Wartime Exposure → Postwar Female Integration (Kakari)
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
    year_num   = as.numeric(year),
    is_female  = gender_modern == "female",
    pos_norm   = str_replace_all(position, "\\s+", ""),
    is_kakacho = str_detect(pos_norm, "係長")
  )

wartime_start <- 1937
wartime_end   <- 1945
postwar_start <- 1947

# ============================================================
# 1. WARTIME POSITION-LEVEL EXPOSURE + DRAFT SHOCK
# ============================================================

wartime_pos_kakari_female <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    female_share = mean(is_female, na.rm = TRUE),
    n_female     = sum(is_female, na.rm = TRUE),
    .groups = "drop"
  )

wartime_pos_kakari_drafts <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_male         = sum(!is_female, na.rm = TRUE),
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share    = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    .groups = "drop"
  )

staff_wartime_cells <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  select(staff_id, office_id, ka, kakari, pos_norm, year_num) %>%
  inner_join(wartime_pos_kakari_female,
             by = c("office_id", "ka", "kakari", "pos_norm", "year_num")) %>%
  left_join(wartime_pos_kakari_drafts,
            by = c("office_id", "ka", "kakari", "pos_norm", "year_num"))

wartime_staff_ids <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  distinct(staff_id)

cat("Staff with wartime observation:", nrow(wartime_staff_ids), "\n")

# ============================================================
# 2. POSTWAR KAKARI PANEL (wartime survivors only)
# ============================================================

postwar_staff <- df %>%
  filter(year_num >= postwar_start,
         staff_id %in% wartime_staff_ids$staff_id)

kakari_outcome <- postwar_staff %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    has_female  = as.integer(any(is_female, na.rm = TRUE)),
    n_female    = sum(is_female, na.rm = TRUE),
    kakari_size = n(),
    .groups = "drop"
  )

ka_controls <- postwar_staff %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    ka_size        = n(),
    n_engineer     = sum(str_detect(pos_norm, "技師|技手|技術"), na.rm = TRUE),
    engineer_share = n_engineer / ka_size,
    n_kakacho_ka   = sum(is_kakacho, na.rm = TRUE),
    .groups = "drop"
  )

ka_kyoku <- postwar_staff %>%
  count(office_id, ka, kyoku) %>%
  group_by(office_id, ka) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, ka, kyoku_modal = kyoku)

kakari_pos <- postwar_staff %>%
  count(office_id, ka, kakari, year_num, pos_norm) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, ka, kakari, year_num, pos_modal = pos_norm)

postwar_kakari_staff <- postwar_staff %>%
  select(staff_id, office_id, ka, kakari, year_num) %>%
  distinct()

# ============================================================
# 3. EXPOSURE VARIANTS + IV (DIRECT DRAFT SHOCK)
# ============================================================

# ---- 3a. Conditioned on kakari-cho wartime offices ----
kakari_cho_postwar <- postwar_staff %>%
  filter(is_kakacho) %>%
  select(staff_id, office_id, ka, kakari, year_num) %>%
  distinct()

kakari_cho_wartime_offices <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end,
         staff_id %in% kakari_cho_postwar$staff_id) %>%
  select(staff_id, wartime_office_id = office_id) %>%
  distinct()

kakari_cho_office_set <- kakari_cho_postwar %>%
  select(post_office_id = office_id, ka, kakari, year_num, staff_id) %>%
  left_join(kakari_cho_wartime_offices, by = "staff_id") %>%
  filter(!is.na(wartime_office_id)) %>%
  distinct(post_office_id, ka, kakari, year_num, wartime_office_id)

cond_cells <- postwar_kakari_staff %>%
  rename(
    post_office_id = office_id,
    post_ka = ka,
    post_kakari = kakari,
    post_year = year_num
  ) %>%
  left_join(kakari_cho_office_set,
            by = c("post_office_id", "post_ka" = "ka",
                   "post_kakari" = "kakari", "post_year" = "year_num")) %>%
  inner_join(staff_wartime_cells,
             by = c("staff_id", "wartime_office_id" = "office_id"))

cond_staff_exp <- cond_cells %>%
  group_by(post_office_id, post_ka, post_kakari, post_year, staff_id) %>%
  summarise(
    exp_mean  = mean(female_share, na.rm = TRUE),
    exp_count = sum(n_female, na.rm = TRUE),
    .groups = "drop"
  )

cond_staff_iv <- cond_cells %>%
  group_by(post_office_id, post_ka, post_kakari, post_year, staff_id) %>%
  summarise(
    z_draft_mean = mean(draft_share, na.rm = TRUE),
    .groups = "drop"
  )

cond_kakari <- cond_staff_exp %>%
  left_join(cond_staff_iv,
            by = c("post_office_id", "post_ka", "post_kakari", "post_year", "staff_id")) %>%
  group_by(post_office_id, post_ka, post_kakari, post_year) %>%
  summarise(
    exp_mean_cond      = mean(exp_mean, na.rm = TRUE),
    exp_count_cond     = mean(exp_count, na.rm = TRUE),
    z_draft_mean_cond  = mean(z_draft_mean, na.rm = TRUE),
    n_staff_cond       = n(),
    .groups = "drop"
  )

# ---- 3b. Unconditioned: offices with any wartime survivor ----
survivor_ids <- postwar_staff %>%
  distinct(staff_id)

survivor_wartime_offices <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end,
         staff_id %in% survivor_ids$staff_id) %>%
  distinct(office_id)

staff_wartime_survivor_offices <- staff_wartime_cells %>%
  filter(office_id %in% survivor_wartime_offices$office_id)

uncond_cells <- postwar_kakari_staff %>%
  rename(
    post_office_id = office_id,
    post_ka = ka,
    post_kakari = kakari,
    post_year = year_num
  ) %>%
  inner_join(staff_wartime_survivor_offices, by = "staff_id")

uncond_staff_exp <- uncond_cells %>%
  group_by(post_office_id, post_ka, post_kakari, post_year, staff_id) %>%
  summarise(
    exp_mean  = mean(female_share, na.rm = TRUE),
    exp_count = sum(n_female, na.rm = TRUE),
    .groups = "drop"
  )

uncond_staff_iv <- uncond_cells %>%
  group_by(post_office_id, post_ka, post_kakari, post_year, staff_id) %>%
  summarise(
    z_draft_mean = mean(draft_share, na.rm = TRUE),
    .groups = "drop"
  )

uncond_kakari <- uncond_staff_exp %>%
  left_join(uncond_staff_iv,
            by = c("post_office_id", "post_ka", "post_kakari", "post_year", "staff_id")) %>%
  group_by(post_office_id, post_ka, post_kakari, post_year) %>%
  summarise(
    exp_mean_uncond     = mean(exp_mean, na.rm = TRUE),
    exp_count_uncond    = mean(exp_count, na.rm = TRUE),
    z_draft_mean_uncond = mean(z_draft_mean, na.rm = TRUE),
    n_staff_uncond      = n(),
    .groups = "drop"
  )

# ============================================================
# 4. PANEL ASSEMBLY
# ============================================================

kakari_panel <- kakari_outcome %>%
  left_join(cond_kakari,
            by = c("office_id" = "post_office_id",
                   "ka" = "post_ka",
                   "kakari" = "post_kakari",
                   "year_num" = "post_year")) %>%
  left_join(uncond_kakari,
            by = c("office_id" = "post_office_id",
                   "ka" = "post_ka",
                   "kakari" = "post_kakari",
                   "year_num" = "post_year")) %>%
  left_join(kakari_pos, by = c("office_id", "ka", "kakari", "year_num")) %>%
  left_join(ka_controls, by = c("office_id", "ka", "year_num")) %>%
  left_join(ka_kyoku, by = c("office_id", "ka"))

cat("\n--- Panel diagnostics ---\n")
cat("Total kakari-years:", nrow(kakari_panel), "\n")
cat("With conditioned exposure:", sum(!is.na(kakari_panel$exp_mean_cond)), "\n")
cat("With unconditioned exposure:", sum(!is.na(kakari_panel$exp_mean_uncond)), "\n")
cat("With conditioned draft shock:", sum(!is.na(kakari_panel$z_draft_mean_cond)), "\n")
cat("With unconditioned draft shock:", sum(!is.na(kakari_panel$z_draft_mean_uncond)), "\n")

# ============================================================
# 5. FIRST STAGE + IV (DIRECT DRAFT SHOCK)
# ============================================================

est_cond <- kakari_panel %>%
  filter(!is.na(exp_mean_cond), !is.na(exp_count_cond), !is.na(z_draft_mean_cond))

est_uncond <- kakari_panel %>%
  filter(!is.na(exp_mean_uncond), !is.na(exp_count_uncond), !is.na(z_draft_mean_uncond))

cat("Conditioned est. sample:", nrow(est_cond), "\n")
cat("Unconditioned est. sample:", nrow(est_uncond), "\n")

fs_cond <- feols(
  exp_mean_cond ~ z_draft_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_cond, cluster = ~office_id
)

fs_uncond <- feols(
  exp_mean_uncond ~ z_draft_mean_uncond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_uncond, cluster = ~office_id
)

iv_cond <- feols(
  n_female ~ engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal |
    exp_mean_cond ~ z_draft_mean_cond,
  data = est_cond, cluster = ~office_id
)

iv_uncond <- feols(
  n_female ~ engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal |
    exp_mean_uncond ~ z_draft_mean_uncond,
  data = est_uncond, cluster = ~office_id
)

cat("\n========== FIRST STAGE ==========\n")
etable(
  fs_cond, fs_uncond,
  headers = c("Cond", "Uncond"),
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

cat("\n========== IV: NUMBER OF FEMALES ==========\n")
etable(
  iv_cond, iv_uncond,
  headers = c("Cond", "Uncond"),
  se.below = TRUE,
  fitstat = ~ n + ivf + ivwald
)

# ============================================================
# 6. CONTROL FUNCTION (POISSON FIRST STAGE)
# ============================================================

fs_pois_cond <- fepois(
  exp_mean_cond ~ z_draft_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_cond, cluster = ~office_id
)

fs_pois_uncond <- fepois(
  exp_mean_uncond ~ z_draft_mean_uncond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_uncond, cluster = ~office_id
)

fs_pois_count_cond <- fepois(
  exp_count_cond ~ z_draft_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_cond, cluster = ~office_id
)

fs_pois_count_uncond <- fepois(
  exp_count_uncond ~ z_draft_mean_uncond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_uncond, cluster = ~office_id
)

est_cond <- est_cond %>%
  mutate(
    cf_pred_mean = predict(fs_pois_cond, newdata = est_cond, type = "response"),
    cf_resid_mean = exp_mean_cond - cf_pred_mean,
    cf_pred_count = predict(fs_pois_count_cond, newdata = est_cond, type = "response"),
    cf_resid_count = exp_count_cond - cf_pred_count
  )

est_uncond <- est_uncond %>%
  mutate(
    cf_pred_mean = predict(fs_pois_uncond, newdata = est_uncond, type = "response"),
    cf_resid_mean = exp_mean_uncond - cf_pred_mean,
    cf_pred_count = predict(fs_pois_count_uncond, newdata = est_uncond, type = "response"),
    cf_resid_count = exp_count_uncond - cf_pred_count
  )

cf_cond <- feols(
  n_female ~ exp_mean_cond + cf_resid_mean + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_cond, cluster = ~office_id
)

cf_uncond <- feols(
  n_female ~ exp_mean_uncond + cf_resid_mean + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_uncond, cluster = ~office_id
)

cf_count_cond <- feols(
  n_female ~ exp_count_cond + cf_resid_count + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_cond, cluster = ~office_id
)

cf_count_uncond <- feols(
  n_female ~ exp_count_uncond + cf_resid_count + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_uncond, cluster = ~office_id
)

cat("\n========== CONTROL FUNCTION (POISSON FIRST STAGE) ==========\n")
etable(
  fs_pois_cond, fs_pois_uncond,
  headers = c("Cond FS", "Uncond FS"),
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

etable(
  cf_cond, cf_uncond,
  headers = c("Cond CF", "Uncond CF"),
  se.below = TRUE
)

cat("\n========== CONTROL FUNCTION (POISSON FS, COUNT EXPOSURE) ==========\n")
etable(
  fs_pois_count_cond, fs_pois_count_uncond,
  headers = c("Cond FS (Count)", "Uncond FS (Count)"),
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

etable(
  cf_count_cond, cf_count_uncond,
  headers = c("Cond CF (Count)", "Uncond CF (Count)"),
  se.below = TRUE
)

# ============================================================
# 7. EXPORT LaTeX SUMMARY (Single Table)
# ============================================================

ols_cond <- feols(
  n_female ~ exp_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_cond, cluster = ~office_id
)

ols_uncond <- feols(
  n_female ~ exp_mean_uncond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_uncond, cluster = ~office_id
)

dict_table <- c(
  exp_mean_cond     = "Exposure (cond)",
  exp_mean_uncond   = "Exposure (uncond)",
  fit_exp_mean_cond = "Exposure (cond)",
  fit_exp_mean_uncond = "Exposure (uncond)",
  engineer_share    = "Engineer share",
  n_kakacho_ka      = "No. Kakari-cho",
  kakari_size       = "Kakari size",
  z_draft_mean_cond   = "Draft shock (cond)",
  z_draft_mean_uncond = "Draft shock (uncond)"
)

panel_a <- etable(
  ols_cond, ols_uncond, iv_cond, iv_uncond,
  headers = c("OLS (Cond)", "OLS (Uncond)", "IV (Cond)", "IV (Uncond)"),
  order = c("Exposure", "Engineer", "Kakari-cho", "Kakari size"),
  dict = dict_table,
  se.below = TRUE,
  fitstat = ~ n + r2 + ivf,
  tex = TRUE,
  label = "tab:panel_a",
  title = "Panel A: Second stage (Dep. var.: N females)"
)

panel_b <- etable(
  fs_cond, fs_uncond,
  headers = c("Cond", "Uncond"),
  order = c("Draft", "Engineer", "Kakari-cho", "Kakari size"),
  dict = dict_table,
  se.below = TRUE,
  fitstat = ~ n + r2,
  tex = TRUE,
  label = "tab:panel_b",
  title = "Panel B: First stage (Dep. var.: Exposure)"
)

tex_out <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{geometry}",
  "\\geometry{margin=1in}",
  "\\begin{document}",
  "\\section*{Direct Draft Shock: OLS vs IV}",
  panel_a,
  "\\vspace{1em}",
  panel_b,
  "\\end{document}"
)

writeLines(tex_out, "IV_ControlFunction_Results.tex")
