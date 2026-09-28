################################################################################
# PreDraftWorkerPools1944 — v3
#
# Three fixes vs v2:
#  1. Cell unit = office_id × kakari × pos_norm  (matches ByDistance / Table 3)
#  2. Shi/Fu coverage flag (former-Shi vs. former-Fu/new offices)
#  3. Inference via fwildclusterboot::boottest, Webb 6-pt weights, B = 9,999
#
# Validation: before main results, replicate Table 3 n_total_internal estimate
#             on the 1944 cross-section with kyoku cluster + boottest.
################################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
    install.packages("fwildclusterboot", repos = "https://cloud.r-project.org")
  }
  library(tidyverse)
  library(fixest)
  library(fwildclusterboot)
})

set.seed(1944)
dqrng::dqset.seed(1944)

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
reg_dir  <- file.path(root, "Regressions")
out_dir  <- file.path(root, "MainResults", "PreDraftWorkerPools1944", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH    <- file.path(Sys.getenv("USERPROFILE"), "Box",
  "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv")
KA_GROUP_PATH <- file.path(reg_dir, "ka_group_map.csv")

# ---- Source normalize_kyoku / assign_kyoku_group from NewTable1c_TransferType.R ----
# Lines 55-126 define both functions; no side effects in that range.
helper_lines <- readLines(file.path(reg_dir, "NewTable1c_TransferType.R"), warn = FALSE)
eval(parse(text = helper_lines[55:126]))

# ---- Load master data ----
cat("Loading master data...\n")
df_names <- read_csv(DATA_PATH,
    locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num     = as.numeric(year),
    is_female    = gender_modern == "female",
    pos_norm     = str_replace_all(position, "\\s+", ""),
    kyoku_clean  = replace_na(kyoku, ""),
    ka_clean     = replace_na(ka, ""),
    norm_kyoku   = normalize_kyoku(kyoku),
    kyoku_group  = assign_kyoku_group(norm_kyoku)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH,
    locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names <- df_names %>%
  left_join(ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka"))

# ---- Shi/Fu flag ----
# office_id was renumbered at the 1943 merger: only 18 of 234 TokyoShi 1942
# office_ids appear in 1944 TokyoTo records (almost certainly coincidental).
# Fallback: staff-linkage definition.
#   Former-Shi: 1944 office where >50% of linked workers came from TokyoShi in 1942.
#   Former-Fu/new: remainder.
shi42_offices <- df_all %>%
  filter(year_num == 1942, gov_level == "TokyoShi", !is.na(office_id)) %>%
  pull(office_id) %>% unique()
cat("1942 TokyoShi office_ids:", length(shi42_offices), "\n")
cat("1944 TokyoTo office_ids:",
    df_all %>% filter(year_num==1944, gov_level=="TokyoTo", !is.na(office_id)) %>%
      pull(office_id) %>% n_distinct(), "\n")
cat("Direct office_id overlap:", length(intersect(shi42_offices,
    df_all %>% filter(year_num==1944, gov_level=="TokyoTo", !is.na(office_id)) %>%
      pull(office_id) %>% unique())), "\n")

# Staff-linkage Shi flag: for each 1944 office, share of linked workers from 1942 TokyoShi
office_origin <- df_all %>%
  filter(year_num == 1944, gov_level == "TokyoTo", !is.na(office_id)) %>%
  select(staff_id, office_id_44 = office_id) %>%
  left_join(
    df_all %>% filter(year_num == 1942, !is.na(office_id)) %>%
      select(staff_id, gov_42 = gov_level),
    by = "staff_id"
  ) %>%
  group_by(office_id_44) %>%
  summarise(
    n_workers   = n(),
    n_linked    = sum(!is.na(gov_42)),
    n_from_shi  = sum(gov_42 == "TokyoShi", na.rm = TRUE),
    share_shi   = if_else(n_linked > 0, n_from_shi / n_linked, NA_real_),
    .groups = "drop"
  )
# Former-Shi: majority linked workers from TokyoShi
shi_offices <- office_origin %>%
  filter(!is.na(share_shi), share_shi > 0.5) %>%
  pull(office_id_44)
cat("Former-Shi offices (staff-linkage >50% from TokyoShi):", length(shi_offices), "\n")
write_csv(office_origin, file.path(out_dir, "office_shi_fu_origin_v3.csv"))

# ---- Build staff transition table for 1944 ----
office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

# Lagged location: each worker's attributes in year_num - 1
staff_lag <- df_names %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
    lag_office_id   = office_id,
    lag_kyoku_group = kyoku_group,
    lag_ka          = ka,
    lag_ka_group    = ka_group) %>%
  mutate(year_num = year_num + 1L)

staff_transitions <- df_names %>%
  filter(year_num == 1944) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year,    by = "staff_id") %>%
  left_join(staff_lag,           by = c("staff_id", "year_num")) %>%
  mutate(
    # Arrival type — exact copy of ByDistance_DraftKakari_Regressions.R:177-186
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group)           ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) &
        ka_group == lag_ka_group                             ~ "same_section",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka          ~ "same_section",
      lag_kyoku_group == kyoku_group                         ~ "same_department",
      lag_kyoku_group != kyoku_group                         ~ "different_department",
      TRUE                                                   ~ NA_character_
    ),
    # is_new_hire — exact copy of ByDistance_DraftKakari_Regressions.R:188-191
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    )
  )

# ---- Position outcomes (cell = office_id × kakari × pos_norm) ----
# Exact copy of ByDistance_DraftKakari_Regressions.R:211-223
position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_section       = sum(arrival_type == "same_section",       na.rm = TRUE),
    n_same_department    = sum(arrival_type == "same_department",    na.rm = TRUE),
    n_different_department = sum(arrival_type == "different_department", na.rm = TRUE),
    n_new_hires          = sum(is_new_hire,                          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(n_total_internal =
    n_same_section + n_same_department + n_different_department)

# ---- Cumulative male stock up to (not including) 1944 ----
# Exact copy of ByDistance_DraftKakari_Regressions.R:225-231 restricted to yr=1944
cumul_male_stock <- df_names %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
  mutate(year_num = 1944L)

# ---- Drafted 1944 ----
# Exact copy of ByDistance_DraftKakari_Regressions.R:233-240 restricted to 1944
position_drafts <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_drafted      = n(),
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join panel ----
# Exact copy of ByDistance_DraftKakari_Regressions.R:242-249
panel <- position_outcomes %>%
  left_join(cumul_male_stock,
    by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts,
    by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(cumul_n_male, n_drafted, n_drafted_male), ~replace_na(.x, 0L)),
    ka_id = if_else(
      !is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    is_former_shi = office_id %in% shi_offices,
    # Clustering variable: normalized kyoku name
    kyoku_cl = normalize_kyoku(kyoku)
  ) %>%
  filter(!is.na(ka_id))

cat("Panel rows:", nrow(panel), "\n")
cat("Former-Shi cells:", sum(panel$is_former_shi), "/", nrow(panel), "\n")
cat("Kyoku clusters:", n_distinct(panel$kyoku_cl), "\n")

# ---- Shi/Fu coverage report ----
coverage <- panel %>%
  group_by(is_former_shi) %>%
  summarise(
    n_cells     = n(),
    n_offices   = n_distinct(office_id),
    n_kyoku     = n_distinct(kyoku_cl, na.rm = TRUE),
    mean_pool   = mean(cumul_n_male),
    mean_drafted = mean(n_drafted_male),
    mean_new_hires = mean(n_new_hires),
    n_ka_obs    = sum(!is.na(ka_id)),
    .groups = "drop"
  )
write_csv(coverage, file.path(out_dir, "shi_fu_coverage_v3.csv"))
cat("\n=== Shi/Fu Coverage ===\n"); print(coverage, n = Inf)

# ---- Singleton removal (required for boottest compatibility) ----
# boottest cannot handle observations dropped internally by feols.
# Pre-remove singletons iteratively across ka_id and pos_norm.
remove_fe_singletons <- function(data, fes) {
  repeat {
    n_before <- nrow(data)
    for (fe in fes) {
      counts <- table(data[[fe]])
      keep   <- names(counts[counts > 1])
      data   <- data[data[[fe]] %in% keep, ]
    }
    if (nrow(data) == n_before) break
  }
  data
}

# ---- Prepare integer IDs for boottest compatibility ----
# fwildclusterboot 0.14 cannot handle Japanese strings (encoding error).
# Use integer encodings for FE and cluster variables; all estimates are identical.
add_int_ids <- function(data) {
  data %>% filter(!is.na(kyoku_cl)) %>%
    mutate(
      ka_id_int      = as.integer(factor(ka_id)),
      pos_norm_int   = as.integer(factor(pos_norm)),
      kyoku_cl_int   = as.integer(factor(kyoku_cl))
    )
}

# ---- Validate: n_total_internal with office_id cluster ----
# ByDistance uses all years 1938-1945 + year FE; here 1944 only, no year FE.
# Validation: confirm feols + boottest run, estimate positive consistent with 0.4079.
cat("\n=== Validation: n_total_internal (1944, office_id cluster) ===\n")
panel_val <- remove_fe_singletons(add_int_ids(panel), c("ka_id_int", "pos_norm_int"))
cat("  Rows after singleton removal:", nrow(panel_val), "(from", nrow(panel), ")\n")

val_model <- feols(
  n_total_internal ~ n_drafted_male + log(cumul_n_male + 1) | ka_id_int + pos_norm_int,
  data    = panel_val,
  cluster = ~office_id
)
cat("  Coef  n_drafted_male:", coef(val_model)["n_drafted_male"], "\n")
cat("  SE    n_drafted_male:", se(val_model)["n_drafted_male"],   "\n")
cat("  p (office_id cluster):", pvalue(val_model)["n_drafted_male"], "\n")
cat("  ByDistance full-panel reference: 0.4079 (SE 0.0761, all years)\n")

boot_val <- tryCatch(
  boottest(val_model, param = "n_drafted_male", B = 9999L, type = "webb",
    clustid = ~kyoku_cl_int),
  error = function(e) { cat("  Boottest error:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(boot_val)) {
  cat("  Boottest p-value (kyoku cluster):", boot_val$p_val, "\n")
  cat("  Validation: boottest completed successfully.\n")
}

# ---- Helper: run model + boottest on one outcome ----
run_one <- function(y, data) {
  d <- remove_fe_singletons(add_int_ids(data), c("ka_id_int", "pos_norm_int"))
  G <- n_distinct(d$kyoku_cl_int)

  if (nrow(d) < 10 || G < 2) {
    return(tibble(outcome = y, estimate = NA_real_, std_error = NA_real_,
      p_feols = NA_real_, p_boot = NA_real_,
      n_cells = nrow(d), n_kyoku_clusters = G,
      diagnostic = "Insufficient data or clusters after singleton removal"))
  }

  m <- tryCatch(
    feols(
      as.formula(paste0(y,
        " ~ n_drafted_male + log(cumul_n_male+1) | ka_id_int + pos_norm_int")),
      data    = d,
      cluster = ~kyoku_cl_int
    ),
    error = function(e) e
  )
  if (inherits(m, "error")) {
    return(tibble(outcome = y, estimate = NA_real_, std_error = NA_real_,
      p_feols = NA_real_, p_boot = NA_real_,
      n_cells = nrow(d), n_kyoku_clusters = G,
      diagnostic = conditionMessage(m)))
  }
  # coeftable() works for all fixest objects; broom::tidy() does not
  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    filter(term == "n_drafted_male")

  bt <- tryCatch(
    boottest(m, param = "n_drafted_male", B = 9999L, type = "webb",
      clustid = ~kyoku_cl_int),
    error = function(e) list(p_val = NA_real_)
  )
  tibble(
    outcome          = y,
    estimate         = ct$Estimate,
    std_error        = ct[["Std. Error"]],
    p_feols          = ct[["Pr(>|t|)"]],
    p_boot           = bt$p_val,
    n_cells          = nobs(m),
    n_kyoku_clusters = G,
    diagnostic       = ""
  )
}

outcomes <- c("n_total_internal", "n_same_section", "n_same_department",
               "n_different_department", "n_new_hires")

# ---- Full sample ----
cat("\nRunning main spec — full sample...\n")
res_full <- map_dfr(outcomes, run_one, data = panel) %>%
  mutate(sample = "full", .before = 1)

# ---- Former-Shi subsample ----
panel_shi <- panel %>% filter(is_former_shi)
cat("Running main spec — former-Shi (", nrow(panel_shi), "cells)...\n")
res_shi <- map_dfr(outcomes, run_one, data = panel_shi) %>%
  mutate(sample = "former_shi", .before = 1)

results_v3 <- bind_rows(res_full, res_shi)
write_csv(results_v3, file.path(out_dir, "regression_v3_main.csv"))

cat("\n=== V3 Main Results ===\n")
print(results_v3 %>%
  select(sample, outcome, estimate, std_error, p_feols, p_boot,
         n_cells, n_kyoku_clusters),
  n = Inf)

cat("\nV3 complete.\n")
