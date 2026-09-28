################################################################################
# PreDraftWorkerPools1944 — v5
#
# Base spec (identical to v4):
#   y_i = b1*D_i + b2*D_i*log(S_i+1) + b3*log(S_i+1)
#         + g1*log(P_sK_i+1) + g2*log(P_oK_i+1)
#         + kyoku×ka FE + pos_norm FE
#
# Table 1: Confounds in S  — baseline vs. rank controls + own-cell incumbents
# Table 2: FE variants     — kyoku×ka FE (v4) vs. kyoku-only FE
# Table 3: New hires by rank — rank-1 / rank-2 / rank-3 subsamples
# Table 4: Functional form — (a) S=0/S>0 spline, (b) S terciles
#
# Inference: kyoku-clustered Webb boottest (G=6) AND ka-clustered boottest for b2.
# Check:     b2(same_ka) + b2(outside_own_ka) == b2(total_internal) each spec.
#
# Output: tables/regression_v5_table{1-4}_raw.csv + table_v5_{1-4}.{csv,tex}
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(fwildclusterboot)
})

set.seed(1944); dqrng::dqset.seed(1944)

root    <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
reg_dir <- file.path(root, "Regressions")
out_dir <- file.path(root, "MainResults", "PreDraftWorkerPools1944", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH     <- file.path(Sys.getenv("USERPROFILE"), "Box",
  "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv")
KA_GROUP_PATH <- file.path(reg_dir, "ka_group_map.csv")

helper_lines <- readLines(file.path(reg_dir, "NewTable1c_TransferType.R"), warn = FALSE)
eval(parse(text = helper_lines[55:126]))   # normalize_kyoku, assign_kyoku_group

rank_pos <- function(x) case_when(
  x %in% c("Unknown", "\u517c")                                       ~ NA_integer_,
  str_detect(x, "\u96c7|\u56d1\u8a17|\u81e8\u6642|\u898b\u7fd2")     ~ 1L,
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b"                      ~ 3L,
  TRUE                                                                 ~ 2L)

# ============================================================
# 1.  LOAD DATA  (identical to v4)
# ============================================================
cat("Loading master data...\n")
df_names <- read_csv(DATA_PATH,
    locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num    = as.numeric(year),
    is_female   = gender_modern == "female",
    pos_norm    = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean    = replace_na(ka, ""),
    norm_kyoku  = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku)
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

# ============================================================
# 2.  FORMER-SHI FLAG  (staff-linkage, identical to v4)
# ============================================================
shi_offices <- df_all %>%
  filter(year_num == 1944, gov_level == "TokyoTo", !is.na(office_id)) %>%
  select(staff_id, office_id_44 = office_id) %>%
  left_join(
    df_all %>% filter(year_num == 1942) %>% select(staff_id, gov_42 = gov_level),
    by = "staff_id"
  ) %>%
  group_by(office_id_44) %>%
  summarise(
    n_linked   = sum(!is.na(gov_42)),
    n_from_shi = sum(gov_42 == "TokyoShi", na.rm = TRUE),
    share_shi  = if_else(n_linked > 0, n_from_shi / n_linked, NA_real_),
    .groups    = "drop"
  ) %>%
  filter(!is.na(share_shi), share_shi > 0.5) %>%
  pull(office_id_44)
cat("Former-Shi offices:", length(shi_offices), "\n")

# ============================================================
# 3.  OUTCOME PANEL  (cell = office_id × kakari × pos_norm)
#     Adds n_new_hires_r1/r2/r3 for Table 3.
# ============================================================
office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
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
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group)               ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) &
        ka_group == lag_ka_group                                 ~ "same_section",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka              ~ "same_section",
      lag_kyoku_group == kyoku_group                             ~ "same_department",
      lag_kyoku_group != kyoku_group                             ~ "different_department",
      TRUE                                                       ~ NA_character_
    ),
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE                          ~ (year_num == first_year)
    ),
    hire_rank = rank_pos(pos_norm)   # rank of arriving worker
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_section         = sum(arrival_type == "same_section",         na.rm = TRUE),
    n_same_department      = sum(arrival_type == "same_department",      na.rm = TRUE),
    n_different_department = sum(arrival_type == "different_department", na.rm = TRUE),
    n_new_hires            = sum(is_new_hire,                            na.rm = TRUE),
    n_new_hires_r1         = sum(is_new_hire & hire_rank == 1L,          na.rm = TRUE),
    n_new_hires_r2         = sum(is_new_hire & hire_rank == 2L,          na.rm = TRUE),
    n_new_hires_r3         = sum(is_new_hire & hire_rank == 3L,          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_total_internal = n_same_section + n_same_department + n_different_department,
    n_outside_own_ka = n_same_department + n_different_department
  )

position_drafts <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

panel <- position_outcomes %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0L),
    ka_id          = if_else(!is.na(ka) & !is.na(kyoku),
                             paste(kyoku, ka, sep = "_"), NA_character_),
    is_former_shi  = office_id %in% shi_offices,
    kyoku_cl       = normalize_kyoku(kyoku)
  ) %>%
  filter(!is.na(ka_id))

# ============================================================
# 4.  POOL VARIABLES  (identical to v4)
# ============================================================
ids42_male <- df_names %>%
  filter(year_num == 1942, !is_female) %>% pull(staff_id) %>% unique()
linked_ids <- intersect(
  ids42_male,
  df_names %>% filter(year_num == 1944) %>% pull(staff_id) %>% unique()
)

men44_linked <- df_names %>%
  filter(year_num == 1944, staff_id %in% linked_ids, !is_female) %>%
  mutate(position_rank = rank_pos(pos_norm)) %>%
  filter(!is.na(position_rank))
cat("Linked men in 1944 with defined rank:", nrow(men44_linked), "\n")

ka_rank_totals    <- men44_linked %>% filter(!is.na(ka_group)) %>%
  count(ka_group, position_rank, name = "n")
kyoku_rank_totals <- men44_linked %>% filter(!is.na(kyoku_group)) %>%
  count(kyoku_group, position_rank, name = "n")
all_rank_totals   <- men44_linked %>% count(position_rank, name = "n")

cell_linked_cnt <- men44_linked %>%
  filter(!is.na(ka_group)) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(n_linked_in_cell = n(), .groups = "drop")

pool_lookup <- function(df, group_col, target_ranks = 1:3) {
  df %>%
    crossing(target_rank = target_ranks) %>%
    filter(position_rank >= pmax(1L, target_rank - 1L),
           position_rank <= target_rank) %>%
    group_by(across(all_of(c(group_col, "target_rank")))) %>%
    summarise(pool_n = sum(n, na.rm = TRUE), .groups = "drop")
}

pool_ka    <- pool_lookup(ka_rank_totals,    "ka_group")
pool_kyoku <- pool_lookup(kyoku_rank_totals, "kyoku_group")
pool_all   <- pool_lookup(all_rank_totals,   character(0)) %>% rename(all_n = pool_n)

panel_v4 <- panel %>%
  filter(is_former_shi) %>%
  mutate(
    cell_rank   = rank_pos(pos_norm),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean    = replace_na(ka, "")
  ) %>%
  filter(!is.na(cell_rank)) %>%
  left_join(
    ka_group_raw %>% filter(year == 1944) %>%
      select(kyoku, ka, cell_ka_group = ka_group),
    by = c("kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(cell_kyoku_group = assign_kyoku_group(normalize_kyoku(kyoku))) %>%
  left_join(
    pool_ka %>% rename(cell_ka_group = ka_group, S_i_total = pool_n),
    by = c("cell_ka_group", "cell_rank" = "target_rank")
  ) %>%
  left_join(cell_linked_cnt, by = c("office_id", "kakari", "pos_norm")) %>%
  mutate(S_i = pmax(0L,
    replace_na(S_i_total, 0L) - replace_na(n_linked_in_cell, 0L))) %>%
  left_join(
    pool_kyoku %>% rename(cell_kyoku_group = kyoku_group, kyoku_total = pool_n),
    by = c("cell_kyoku_group", "cell_rank" = "target_rank")
  ) %>%
  mutate(P_sameKyoku = pmax(0L,
    replace_na(kyoku_total, 0L) - replace_na(S_i_total, 0L))) %>%
  left_join(pool_all, by = c("cell_rank" = "target_rank")) %>%
  mutate(P_otherKyoku = pmax(0L,
    replace_na(all_n, 0L) - replace_na(kyoku_total, 0L))) %>%
  mutate(
    log_S_i          = log(S_i + 1),
    log_P_sameKyoku  = log(P_sameKyoku + 1),
    log_P_otherKyoku = log(P_otherKyoku + 1)
  )

cat("V4 base cells:", nrow(panel_v4),
    " | kyoku clusters:", n_distinct(panel_v4$kyoku_cl, na.rm=TRUE), "\n")

# ============================================================
# 5.  V5 ADDITIONAL VARIABLES
# ============================================================
panel_v5 <- panel_v4 %>%
  mutate(
    # Own-cell 1942 incumbents: linked men in this 1944 cell (proxy via staff-linkage)
    n_incumb42   = replace_na(n_linked_in_cell, 0L),
    log_incumb42 = log(n_incumb42 + 1),
    # Rank dummies (main effects absorbed by pos_norm FE; interactions identified)
    rank_2 = as.integer(cell_rank == 2L),
    rank_3 = as.integer(cell_rank == 3L),
    # Interactions: D × rank dummies
    D_rank2  = n_drafted_male * rank_2,
    D_rank3  = n_drafted_male * rank_3,
    # Interaction: D × log(own-cell incumbents+1)
    D_incumb = n_drafted_male * log_incumb42,
    # Table 4a: piecewise linear in S (zero vs. positive)
    S_zero        = as.integer(S_i == 0L),
    log_S_nonzero = if_else(S_i > 0L, log(S_i + 1), 0),
    D_S0    = n_drafted_male * S_zero,
    D_logS1 = n_drafted_male * log_S_nonzero,
    # Table 4b: S terciles
    S_tercile = ntile(S_i, 3L),
    T2    = as.integer(S_tercile == 2L),
    T3    = as.integer(S_tercile == 3L),
    D_T2  = n_drafted_male * T2,
    D_T3  = n_drafted_male * T3
  )

cat("S=0 cells:", sum(panel_v5$S_zero),
    " | S tercile dist:", paste(table(panel_v5$S_tercile), collapse="/"), "\n")

# ============================================================
# 6.  ESTIMATION HELPERS
# ============================================================
remove_fe_singletons <- function(data, fes) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      counts <- table(data[[fe]])
      data   <- data[data[[fe]] %in% names(counts[counts > 1]), ]
    }
    if (nrow(data) == n0) break
  }
  data
}

add_int_ids <- function(data) {
  data %>%
    filter(!is.na(kyoku_cl)) %>%
    mutate(
      ka_id_int    = as.integer(factor(ka_id)),
      pos_norm_int = as.integer(factor(pos_norm)),
      kyoku_cl_int = as.integer(factor(kyoku_cl))
    )
}

# Run one outcome. Returns long tibble (one row per coef term).
# b_params: terms for which to run boottest (kyoku + ka cluster).
run_spec <- function(y_name, data,
                     formula_rhs,
                     fe_str  = "ka_id_int + pos_norm_int",
                     fe_cols = c("ka_id_int", "pos_norm_int"),
                     b_params = c("n_drafted_male", "n_drafted_male:log_S_i")) {

  d <- remove_fe_singletons(add_int_ids(data), fe_cols)
  if (nrow(d) < 10) {
    warning("run_spec: too few obs for ", y_name, " (", nrow(d), ")")
    return(NULL)
  }

  G_kyoku <- n_distinct(d$kyoku_cl_int)
  G_ka    <- n_distinct(d$ka_id_int)

  f <- as.formula(paste0(y_name, " ~ ", formula_rhs, " | ", fe_str))
  m <- tryCatch(feols(f, data = d, cluster = ~kyoku_cl_int),
                error = function(e) { cat("feols error:", y_name, conditionMessage(e), "\n"); e })
  if (inherits(m, "error")) return(NULL)

  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    rename(estimate  = Estimate,
           std_error = `Std. Error`,
           p_feols   = `Pr(>|t|)`) %>%
    select(term, estimate, std_error, p_feols)

  ct$p_kyoku_boot <- NA_real_
  ct$p_ka_boot    <- NA_real_

  for (tm in intersect(b_params, ct$term)) {
    bt_k <- tryCatch(
      boottest(m, param = tm, B = 9999L, type = "webb", clustid = ~kyoku_cl_int),
      error = function(e) { cat("  boottest[kyoku] error:", tm, conditionMessage(e), "\n");
                            list(p_val = NA_real_) })
    ct$p_kyoku_boot[ct$term == tm] <- bt_k$p_val

    bt_ka <- tryCatch(
      boottest(m, param = tm, B = 9999L, type = "webb", clustid = ~ka_id_int),
      error = function(e) { cat("  boottest[ka] error:", tm, conditionMessage(e), "\n");
                            list(p_val = NA_real_) })
    ct$p_ka_boot[ct$term == tm] <- bt_ka$p_val
  }

  ct %>% mutate(outcome = y_name, n_cells = nobs(m),
                G_kyoku = G_kyoku, G_ka = G_ka, .before = everything())
}

run_outcomes <- function(outcomes, data, formula_rhs,
                         fe_str, fe_cols, b_params) {
  map_dfr(outcomes, run_spec,
          data = data, formula_rhs = formula_rhs,
          fe_str = fe_str, fe_cols = fe_cols, b_params = b_params)
}

# ============================================================
# 7.  FORMULA STRINGS
# ============================================================
FE_KA  <- "ka_id_int + pos_norm_int"      # kyoku×ka FE (v4 default)
FE_KYO <- "kyoku_cl_int + pos_norm_int"   # kyoku-only FE

FECOLS_KA  <- c("ka_id_int",   "pos_norm_int")
FECOLS_KYO <- c("kyoku_cl_int","pos_norm_int")

rhs_base <- paste(
  "n_drafted_male + n_drafted_male:log_S_i + log_S_i",
  "+ log_P_sameKyoku + log_P_otherKyoku"
)

rhs_conf <- paste(
  "n_drafted_male + n_drafted_male:log_S_i + log_S_i",
  "+ log_P_sameKyoku + log_P_otherKyoku",
  "+ D_rank2 + D_rank3 + D_incumb + log_incumb42"
)

# For rank subsamples: D_rank2/D_rank3 are constant within a rank, drop them
rhs_rank_sub <- paste(
  "n_drafted_male + n_drafted_male:log_S_i + log_S_i",
  "+ log_P_sameKyoku + log_P_otherKyoku",
  "+ D_incumb + log_incumb42"
)

rhs_spline <- paste(
  "D_S0 + D_logS1 + S_zero + log_S_nonzero",
  "+ log_P_sameKyoku + log_P_otherKyoku"
)

rhs_tercile <- paste(
  "n_drafted_male + D_T2 + D_T3 + T2 + T3",
  "+ log_P_sameKyoku + log_P_otherKyoku"
)

OUTCOMES_MAIN  <- c("n_same_section", "n_outside_own_ka",
                    "n_total_internal", "n_new_hires")
LABELS_MAIN    <- c("Same ka", "Outside own ka", "Total internal", "New hires")

B_MAIN <- c("n_drafted_male", "n_drafted_male:log_S_i")

# ============================================================
# 8.  TABLE 1: CONFOUNDS IN S
# ============================================================
cat("\n=== TABLE 1: CONFOUNDS ===\n")
res1a <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_base,
                      FE_KA, FECOLS_KA, B_MAIN) %>% mutate(panel = "A_baseline")
res1b <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_conf,
                      FE_KA, FECOLS_KA, B_MAIN) %>% mutate(panel = "B_confounds")
res1  <- bind_rows(res1a, res1b)
write_csv(res1, file.path(out_dir, "regression_v5_table1_raw.csv"))
cat("Table 1 done. Rows:", nrow(res1), "\n")

# ============================================================
# 9.  TABLE 2: FE COMPARISON
# ============================================================
cat("\n=== TABLE 2: FE COMPARISON ===\n")
res2a <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_conf,
                      FE_KA,  FECOLS_KA,  B_MAIN) %>% mutate(panel = "A_kyoku_ka_FE")
res2b <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_conf,
                      FE_KYO, FECOLS_KYO, B_MAIN) %>% mutate(panel = "B_kyoku_FE")
res2  <- bind_rows(res2a, res2b)
write_csv(res2, file.path(out_dir, "regression_v5_table2_raw.csv"))
cat("Table 2 done. Rows:", nrow(res2), "\n")

# ============================================================
# 10.  TABLE 3: NEW HIRES BY RANK
# ============================================================
cat("\n=== TABLE 3: NEW HIRES BY RANK ===\n")
cat("Rank distribution:", paste(table(panel_v5$cell_rank), collapse="/"),
    "(ranks 1/2/3)\n")

run_nh_rank <- function(rk, label) {
  d   <- if (is.na(rk)) panel_v5 else panel_v5 %>% filter(cell_rank == rk)
  rhs <- if (is.na(rk)) rhs_conf else rhs_rank_sub  # drop rank dummies in single-rank subsample
  run_outcomes("n_new_hires", d, rhs, FE_KA, FECOLS_KA, B_MAIN) %>%
    mutate(panel = label)
}

res3 <- bind_rows(
  run_nh_rank(1L, "rank1"),
  run_nh_rank(2L, "rank2"),
  run_nh_rank(3L, "rank3"),
  run_nh_rank(NA, "all")
)
write_csv(res3, file.path(out_dir, "regression_v5_table3_raw.csv"))
cat("Table 3 done. Rows:", nrow(res3), "\n")

# ============================================================
# 11.  TABLE 4: FUNCTIONAL FORM
# ============================================================
cat("\n=== TABLE 4: FUNCTIONAL FORM ===\n")

res4a <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_spline,
                      FE_KA, FECOLS_KA,
                      b_params = c("D_S0", "D_logS1")) %>%
  mutate(panel = "A_spline")

res4b <- run_outcomes(OUTCOMES_MAIN, panel_v5, rhs_tercile,
                      FE_KA, FECOLS_KA,
                      b_params = c("n_drafted_male", "D_T2", "D_T3")) %>%
  mutate(panel = "B_tercile")

res4  <- bind_rows(res4a, res4b)
write_csv(res4, file.path(out_dir, "regression_v5_table4_raw.csv"))
cat("Table 4 done. Rows:", nrow(res4), "\n")

# ============================================================
# 12.  CHECK: b2 additivity
# ============================================================
cat("\n=== CHECK: b2(same_ka) + b2(outside_own_ka) == b2(total_internal) ===\n")
check_add <- function(res, panel_nm,
                      b2_term = "n_drafted_male:log_S_i") {
  sub <- res %>% filter(panel == panel_nm, term == b2_term)
  get_b2 <- function(oc) {
    v <- sub %>% filter(outcome == oc) %>% pull(estimate)
    if (length(v) == 0) NA_real_ else v[1]
  }
  b2_ska  <- get_b2("n_same_section")
  b2_oka  <- get_b2("n_outside_own_ka")
  b2_tot  <- get_b2("n_total_internal")
  if (any(is.na(c(b2_ska, b2_oka, b2_tot)))) {
    cat(sprintf("  %-22s  cannot check (NA)\n", panel_nm))
    return(invisible(NULL))
  }
  diff <- (b2_ska + b2_oka) - b2_tot
  cat(sprintf("  %-22s  ska=%7.4f  oka=%7.4f  sum=%7.4f  tot=%7.4f  diff=%+.2e\n",
              panel_nm, b2_ska, b2_oka, b2_ska + b2_oka, b2_tot, diff))
}

for (pn in unique(res1$panel)) check_add(res1, pn)
for (pn in unique(res2$panel)) check_add(res2, pn)

# ============================================================
# 13.  TABLE FORMATTING HELPERS
# ============================================================
fmt   <- function(x, d = 3) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))
strs  <- function(p) case_when(
  is.na(p)  ~ "",
  p < 0.01  ~ "***",
  p < 0.05  ~ "**",
  p < 0.10  ~ "*",
  TRUE      ~ "")

# For one (outcome, term) pair, extract formatted cells
cell_est <- function(r, use_kyoku_boot = TRUE) {
  if (!nrow(r)) return("")
  p <- if (use_kyoku_boot) coalesce(r$p_kyoku_boot, r$p_feols) else r$p_feols
  paste0(fmt(r$estimate), strs(p))
}
cell_se  <- function(r) { if (!nrow(r)) "" else paste0("(", fmt(r$std_error), ")") }
cell_pk  <- function(r) { if (!nrow(r) || is.na(r$p_kyoku_boot)) ""
                          else paste0("[", fmt(r$p_kyoku_boot), "]") }
cell_pka <- function(r) { if (!nrow(r) || is.na(r$p_ka_boot)) ""
                          else paste0("{", fmt(r$p_ka_boot), "}") }

# Build one formatted wide-table data.frame from results.
# panels_specs = list of list(label, res_df, terms_list)
# terms_list = list of list(term, row_label, show_boot)
build_wide_tbl <- function(panels_specs, outcomes, outcome_labels) {
  rows <- list()
  for (sp in panels_specs) {
    rows[[length(rows)+1]] <- c(sp$label, rep("", length(outcomes)))
    res <- sp$res

    for (ti in sp$terms) {
      tm  <- ti$term
      lbl <- ti$label
      sub <- map(outcomes, function(oc) res %>% filter(outcome==oc, term==tm))

      rows[[length(rows)+1]] <- c(lbl,   map_chr(sub, cell_est))
      rows[[length(rows)+1]] <- c("",    map_chr(sub, cell_se))
      if (isTRUE(ti$show_boot)) {
        rows[[length(rows)+1]] <- c("[Webb p, kyoku]", map_chr(sub, cell_pk))
        rows[[length(rows)+1]] <- c("{Webb p, ka}",    map_chr(sub, cell_pka))
      }
    }

    # N, G_kyoku, G_ka
    for (nm in c("n_cells", "G_kyoku", "G_ka")) {
      lbl_nm <- switch(nm, n_cells="N", G_kyoku="G (kyoku)", G_ka="G (ka)")
      vals <- map_chr(outcomes, function(oc) {
        v <- res %>% filter(outcome==oc) %>% slice(1) %>% pull(all_of(nm))
        if (length(v)==0 || is.na(v)) "" else as.character(v)
      })
      rows[[length(rows)+1]] <- c(lbl_nm, vals)
    }
  }
  tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(tbl) <- c("", outcome_labels)
  tbl
}

save_wide_tbl <- function(tbl, base_name, caption, label, footnote = NULL) {
  write_csv(tbl, file.path(out_dir, paste0(base_name, ".csv")))

  nc  <- ncol(tbl) - 1
  cs  <- paste0("l", paste(rep("r", nc), collapse = ""))
  hdr <- paste(colnames(tbl), collapse = " & ")

  is_panel_hdr <- apply(tbl[, -1, drop = FALSE], 1, function(r) all(r == ""))
  is_N_row     <- tbl[[1]] == "N"

  tex <- c(
    "\\begin{table}[!htbp]",
    "  \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    paste0("  \\begin{tabular}{", cs, "}"),
    "    \\hline\\hline",
    paste0("    ", hdr, " \\\\"),
    "    \\hline"
  )

  prev_panel <- FALSE
  for (i in seq_len(nrow(tbl))) {
    row_str <- paste(as.character(tbl[i, ]), collapse = " & ")
    if (is_panel_hdr[i] && i > 1) tex <- c(tex, "    \\hline")
    if (is_N_row[i])               tex <- c(tex, "    \\hline")
    tex <- c(tex, paste0("    ", row_str, " \\\\"))
  }

  fn <- if (is.null(footnote))
    paste("Stars use Webb bootstrap p (kyoku cluster, B=9,999).",
          "[ ]: kyoku-cluster Webb p. \\{\\}: ka-cluster Webb p.")
  else footnote

  tex <- c(tex,
    "    \\hline\\hline",
    "  \\end{tabular}",
    "  \\\\[4pt]",
    paste0("  \\footnotesize ", fn),
    "\\end{table}")

  writeLines(tex, file.path(out_dir, paste0(base_name, ".tex")), useBytes = TRUE)
  invisible(tbl)
}

# ============================================================
# 14.  FORMATTED TABLE 1  (Confounds)
# ============================================================
b1_spec  <- list(term="n_drafted_male",         label="b1 (D_i)",                show_boot=TRUE)
b2_spec  <- list(term="n_drafted_male:log_S_i", label="b2 (D_i x log S_i+1)",    show_boot=TRUE)

tbl1_specs <- list(
  list(label = "Panel A: Baseline (v4 spec)",
       res   = res1 %>% filter(panel == "A_baseline"),
       terms = list(b1_spec, b2_spec)),
  list(label = "Panel B: With rank controls and own-cell incumbents",
       res   = res1 %>% filter(panel == "B_confounds"),
       terms = list(b1_spec, b2_spec))
)

tbl1 <- build_wide_tbl(tbl1_specs, OUTCOMES_MAIN, LABELS_MAIN)
save_wide_tbl(tbl1, "table_v5_1",
  "Confounds in S: baseline vs.~rank controls and own-cell incumbents (v5)",
  "tab:v5_confounds",
  paste("Panel A: v4 baseline. Panel B adds D$\\times$rank-2, D$\\times$rank-3",
        "(rank-1 base), D$\\times$log(linked incumbents+1), log(linked incumbents+1).",
        "Stars use Webb bootstrap p (kyoku cluster, B=9,999).",
        "[ ]: kyoku-cluster Webb p. \\{\\}: ka-cluster Webb p.",
        "FE: kyoku$\\times$ka and pos\\_norm."))
cat("Table 1 formatted.\n")

# ============================================================
# 15.  FORMATTED TABLE 2  (FE comparison)
# ============================================================
tbl2_specs <- list(
  list(label = "Panel A: kyoku x ka FE (v4 default)",
       res   = res2 %>% filter(panel == "A_kyoku_ka_FE"),
       terms = list(b1_spec, b2_spec)),
  list(label = "Panel B: kyoku FE only",
       res   = res2 %>% filter(panel == "B_kyoku_FE"),
       terms = list(b1_spec, b2_spec))
)

tbl2 <- build_wide_tbl(tbl2_specs, OUTCOMES_MAIN, LABELS_MAIN)
save_wide_tbl(tbl2, "table_v5_2",
  "FE comparison: kyoku$\\times$ka FE vs.~kyoku-only FE (v5 with confounds)",
  "tab:v5_fe_comparison",
  paste("Both panels use v5 confound controls.",
        "Panel A: kyoku$\\times$ka FE + pos\\_norm FE.",
        "Panel B: kyoku FE + pos\\_norm FE.",
        "Stars use Webb bootstrap p (kyoku cluster, B=9,999).",
        "[ ]: kyoku-cluster Webb p. \\{\\}: ka-cluster Webb p.",
        "Note: Panel B kyoku-cluster bootstrap omitted when kyoku is the FE."))
cat("Table 2 formatted.\n")

# ============================================================
# 16.  FORMATTED TABLE 3  (New hires by rank)
# ============================================================
OUTCOMES_T3 <- c("rank1", "rank2", "rank3", "all")
LABELS_T3   <- c("New hires rank 1", "New hires rank 2", "New hires rank 3", "All new hires")

# reshape: panel -> pseudo-outcome column
res3_wide <- res3 %>% mutate(outcome_col = panel)

tbl3_specs <- list(
  list(label = "New hires by cell rank (v5, former-Shi)",
       res   = res3 %>% select(-outcome) %>% rename(outcome = panel),
       terms = list(b1_spec, b2_spec))
)

tbl3 <- build_wide_tbl(tbl3_specs, OUTCOMES_T3, LABELS_T3)
save_wide_tbl(tbl3, "table_v5_3",
  "New hires by rank subsample (v5 with confounds)",
  "tab:v5_new_hires_rank",
  paste("Each column is a separate regression restricted to cells of the indicated rank.",
        "Outcome: n\\_new\\_hires.",
        "Stars use Webb bootstrap p (kyoku cluster, B=9,999).",
        "[ ]: kyoku-cluster Webb p. \\{\\}: ka-cluster Webb p.",
        "FE: kyoku$\\times$ka and pos\\_norm."))
cat("Table 3 formatted.\n")

# ============================================================
# 17.  FORMATTED TABLE 4  (Functional form)
# ============================================================
b_s0_spec   <- list(term="D_S0",    label="b(D x 1[S=0])",          show_boot=TRUE)
b_s1_spec   <- list(term="D_logS1", label="b(D x log(S+1) x 1[S>0])", show_boot=TRUE)
b_t1_spec   <- list(term="n_drafted_male", label="b(D, T1 base)",    show_boot=TRUE)
b_dt2_spec  <- list(term="D_T2",    label="b(D x 1[T2])",            show_boot=TRUE)
b_dt3_spec  <- list(term="D_T3",    label="b(D x 1[T3])",            show_boot=TRUE)

tbl4_specs <- list(
  list(label = "Panel A: S=0 / S>0 spline",
       res   = res4 %>% filter(panel == "A_spline"),
       terms = list(b_s0_spec, b_s1_spec)),
  list(label = "Panel B: S terciles (T1 base)",
       res   = res4 %>% filter(panel == "B_tercile"),
       terms = list(b_t1_spec, b_dt2_spec, b_dt3_spec))
)

tbl4 <- build_wide_tbl(tbl4_specs, OUTCOMES_MAIN, LABELS_MAIN)
save_wide_tbl(tbl4, "table_v5_4",
  "Functional form robustness (v5)",
  "tab:v5_functional_form",
  paste("Panel A: D$\\times 1[S=0]$ and D$\\times\\log(S+1)\\times 1[S>0]$",
        "with main effects $1[S=0]$, $\\log(S+1)\\times 1[S>0]$, pool controls, and FE.",
        "Panel B: D$\\times 1[T_2]$, D$\\times 1[T_3]$ (T1 base), with tercile dummies,",
        "pool controls, and FE; terciles computed on S\\_i distribution.",
        "Stars use Webb bootstrap p (kyoku cluster, B=9,999).",
        "[ ]: kyoku-cluster Webb p. \\{\\}: ka-cluster Webb p.",
        "FE: kyoku$\\times$ka and pos\\_norm."))
cat("Table 4 formatted.\n")

cat("\n=== V5 COMPLETE ===\n")
cat("Files in", out_dir, ":\n")
cat(paste(" ", grep("v5", list.files(out_dir), value=TRUE), collapse="\n"), "\n")
