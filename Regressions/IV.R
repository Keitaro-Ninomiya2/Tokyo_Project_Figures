################################################################################
# Direct Draft Shock IV: Wartime Exposure → Postwar Female Integration (Kakari)
# Manager-linked: exposure/draft shock from ALL wartime workers in kakari-cho's
# wartime workplace (no survivorship bias)
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
# 1. WARTIME KAKARI-LEVEL MEASURES (ALL wartime workers, no survivorship)
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

# Aggregate to wartime kakari level (all workers in cell, no survivor filter)
wartime_kakari_measures <- wartime_pos_kakari_female %>%
  left_join(wartime_pos_kakari_drafts,
            by = c("office_id", "ka", "kakari", "pos_norm", "year_num")) %>%
  group_by(office_id, ka, kakari) %>%
  summarise(
    exp_mean  = mean(female_share, na.rm = TRUE),
    exp_count = mean(n_female, na.rm = TRUE),
    z_draft   = mean(draft_share, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 2. POSTWAR KAKARI PANEL (all postwar staff for outcomes/controls)
# ============================================================

postwar_staff <- df %>%
  filter(year_num >= postwar_start)

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

# ============================================================
# 3. MANAGER-LINKED EXPOSURE (kakari-cho's wartime kakari, ALL workers)
# ============================================================

# Postwar kakari-cho
kakari_cho_postwar <- postwar_staff %>%
  filter(is_kakacho) %>%
  select(staff_id, office_id, ka, kakari, year_num) %>%
  distinct()

# Kakari-cho's wartime (office, ka, kakari) - where they worked DURING war
kakari_cho_wartime_cells <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end,
         staff_id %in% kakari_cho_postwar$staff_id) %>%
  select(staff_id, wartime_office_id = office_id, wartime_ka = ka,
         wartime_kakari = kakari) %>%
  distinct()

# Link postwar kakari -> kakari-cho -> wartime kakari measures
# Use ALL wartime workers in those cells (no survivorship)
manager_linked <- kakari_cho_postwar %>%
  left_join(kakari_cho_wartime_cells, by = "staff_id") %>%
  filter(!is.na(wartime_office_id)) %>%
  left_join(wartime_kakari_measures,
            by = c("wartime_office_id" = "office_id",
                   "wartime_ka" = "ka",
                   "wartime_kakari" = "kakari")) %>%
  filter(!is.na(exp_mean), !is.na(z_draft)) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    exp_mean_cond     = mean(exp_mean, na.rm = TRUE),
    exp_count_cond    = mean(exp_count, na.rm = TRUE),
    z_draft_mean_cond = mean(z_draft, na.rm = TRUE),
    n_kakacho_linked  = n(),
    .groups = "drop"
  )

# ============================================================
# 4. PANEL ASSEMBLY
# ============================================================

kakari_panel <- kakari_outcome %>%
  left_join(manager_linked,
            by = c("office_id", "ka", "kakari", "year_num")) %>%
  left_join(kakari_pos, by = c("office_id", "ka", "kakari", "year_num")) %>%
  left_join(ka_controls, by = c("office_id", "ka", "year_num")) %>%
  left_join(ka_kyoku, by = c("office_id", "ka"))

cat("\n--- Panel diagnostics (manager-linked, wartime workers only) ---\n")
cat("Total postwar kakari-years:", nrow(kakari_panel), "\n")
cat("With manager-linked exposure:", sum(!is.na(kakari_panel$exp_mean_cond)), "\n")

# ============================================================
# 5. FIRST STAGE + IV (DIRECT DRAFT SHOCK)
# ============================================================

est_cond <- kakari_panel %>%
  filter(!is.na(exp_mean_cond), !is.na(exp_count_cond), !is.na(z_draft_mean_cond))

cat("Estimation sample (postwar kakari with kakari-cho wartime link):", nrow(est_cond), "\n")

fs_cond <- feols(
  exp_mean_cond ~ z_draft_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_cond, cluster = ~office_id
)

iv_cond <- feols(
  n_female ~ engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal |
    exp_mean_cond ~ z_draft_mean_cond,
  data = est_cond, cluster = ~office_id
)

cat("\n========== FIRST STAGE (manager-linked) ==========\n")
etable(
  fs_cond,
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

cat("\n========== IV: NUMBER OF FEMALES (manager-linked) ==========\n")
etable(
  iv_cond,
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

fs_pois_count_cond <- fepois(
  exp_count_cond ~ z_draft_mean_cond + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal + pos_modal,
  data = est_cond, cluster = ~office_id
)

est_cond <- est_cond %>%
  mutate(
    cf_pred_mean = predict(fs_pois_cond, newdata = est_cond, type = "response"),
    cf_resid_mean = exp_mean_cond - cf_pred_mean,
    cf_pred_count = predict(fs_pois_count_cond, newdata = est_cond, type = "response"),
    cf_resid_count = exp_count_cond - cf_pred_count
  )

cf_cond <- feols(
  n_female ~ exp_mean_cond + cf_resid_mean + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_cond, cluster = ~office_id
)

cf_count_cond <- feols(
  n_female ~ exp_count_cond + cf_resid_count + engineer_share + n_kakacho_ka + kakari_size |
    year_num + kyoku_modal,
  data = est_cond, cluster = ~office_id
)

cat("\n========== CONTROL FUNCTION (POISSON FIRST STAGE) ==========\n")
etable(
  fs_pois_cond,
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

etable(
  cf_cond,
  se.below = TRUE
)

cat("\n========== CONTROL FUNCTION (POISSON FS, COUNT EXPOSURE) ==========\n")
etable(
  fs_pois_count_cond,
  se.below = TRUE,
  fitstat = ~ n + r2 + wald
)

etable(
  cf_count_cond,
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

dict_table <- c(
  exp_mean_cond     = "Exposure (manager-linked)",
  fit_exp_mean_cond = "Exposure (manager-linked)",
  engineer_share    = "Engineer share",
  n_kakacho_ka      = "No. Kakari-cho",
  kakari_size       = "Kakari size",
  z_draft_mean_cond = "Draft shock (manager-linked)"
)

panel_a <- etable(
  ols_cond, iv_cond,
  headers = c("OLS", "IV"),
  order = c("Exposure", "Engineer", "Kakari-cho", "Kakari size"),
  dict = dict_table,
  se.below = TRUE,
  fitstat = ~ n + r2 + ivf,
  tex = TRUE,
  label = "tab:panel_a",
  title = "Panel A: Second stage (Dep. var.: N females)"
)

panel_b <- etable(
  fs_cond,
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
  "\\section*{Direct Draft Shock: OLS vs IV (Manager-Linked, Wartime Workers)}",
  "\\vspace{0.5em}",
  panel_a,
  "\\vspace{1em}",
  panel_b,
  "\\end{document}"
)

writeLines(tex_out, here("IV_ControlFunction_Results.tex"))
