################################################################################
# PreDraftWorkerPools1944 — v4
#
# Model (OLS, per outcome):
#   y_i = b1*D_i + b2*D_i*log(S_i+1) + b3*log(S_i+1)
#         + g1*log(P_sameKyoku_i+1) + g2*log(P_otherKyoku_i+1)
#         + kyoku×ka FE + pos_norm FE + e_i
#
# D_i          : drafted men in cell i
# S_i          : linked men (in both 1942 AND 1944, male) in same ka_group,
#                at rank r_i or r_i-1, excluding cell i itself
# P_sameKyoku_i: same pool definition, other ka of same kyoku
# P_otherKyoku_i: same pool definition, other kyoku
#
# Sample: 1944, former-Shi offices (staff-linkage >50%), men only
# Cluster: kyoku (G=6). boottest Webb B=9,999 for b1 and b2.
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
  x %in% c("Unknown", "\u517c")                                     ~ NA_integer_,
  str_detect(x, "\u96c7|\u56d1\u8a17|\u81e8\u6642|\u898b\u7fd2")   ~ 1L,
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b"                    ~ 3L,
  TRUE                                                               ~ 2L)

# ============================================================
# 1.  LOAD DATA
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
# 2.  FORMER-SHI FLAG  (staff-linkage, identical to v3)
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
    .groups = "drop"
  ) %>%
  filter(!is.na(share_shi), share_shi > 0.5) %>%
  pull(office_id_44)
cat("Former-Shi offices:", length(shi_offices), "\n")

# ============================================================
# 3.  OUTCOME PANEL  (cell = office_id × kakari × pos_norm)
#     Arrival types identical to v3 / ByDistance
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
    )
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_section         = sum(arrival_type == "same_section",         na.rm = TRUE),
    n_same_department      = sum(arrival_type == "same_department",      na.rm = TRUE),
    n_different_department = sum(arrival_type == "different_department", na.rm = TRUE),
    n_new_hires            = sum(is_new_hire,                            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_total_internal  = n_same_section + n_same_department + n_different_department,
    n_outside_own_ka  = n_same_department + n_different_department   # new in v4
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
# 4.  POOL VARIABLES  (S_i, P_sameKyoku_i, P_otherKyoku_i)
# ============================================================

# Linked men: male, in both 1942 AND 1944, with a defined rank
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

# Aggregates keyed by (group, position_rank)
ka_rank_totals    <- men44_linked %>%
  filter(!is.na(ka_group)) %>%
  count(ka_group, position_rank, name = "n")

kyoku_rank_totals <- men44_linked %>%
  filter(!is.na(kyoku_group)) %>%
  count(kyoku_group, position_rank, name = "n")

all_rank_totals   <- men44_linked %>%
  count(position_rank, name = "n")

# Workers of each cell (to subtract from S_i: cell i itself is excluded)
cell_linked_cnt <- men44_linked %>%
  filter(!is.na(ka_group)) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(n_linked_in_cell = n(), .groups = "drop")

# For cell with target rank r, pool = workers at rank r AND rank r-1 (>= 1)
# Pre-compute pool size indexed by (group, target_rank)
pool_lookup <- function(df, group_col, target_ranks = 1:3) {
  df %>%
    crossing(target_rank = target_ranks) %>%
    filter(position_rank >= pmax(1L, target_rank - 1L),
           position_rank <= target_rank) %>%
    group_by(across(all_of(c(group_col, "target_rank")))) %>%
    summarise(pool_n = sum(n, na.rm = TRUE), .groups = "drop")
}

pool_ka     <- pool_lookup(ka_rank_totals,    "ka_group")
pool_kyoku  <- pool_lookup(kyoku_rank_totals, "kyoku_group")
pool_all    <- pool_lookup(all_rank_totals,   character(0)) %>%
  rename(all_n = pool_n)   # just (target_rank, all_n)

# Add cell rank and ka_group to panel, then join pool sizes
panel_v4 <- panel %>%
  filter(is_former_shi) %>%
  mutate(
    cell_rank   = rank_pos(pos_norm),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean    = replace_na(ka, "")
  ) %>%
  filter(!is.na(cell_rank)) %>%
  left_join(
    ka_group_raw %>%
      filter(year == 1944) %>%
      select(kyoku, ka, cell_ka_group = ka_group),
    by = c("kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(cell_kyoku_group = assign_kyoku_group(normalize_kyoku(kyoku))) %>%
  # S_i total (same ka_group)
  left_join(
    pool_ka %>% rename(cell_ka_group = ka_group, S_i_total = pool_n),
    by = c("cell_ka_group", "cell_rank" = "target_rank")
  ) %>%
  # subtract own-cell linked workers from S_i
  left_join(cell_linked_cnt, by = c("office_id", "kakari", "pos_norm")) %>%
  mutate(S_i = pmax(0L,
    replace_na(S_i_total, 0L) - replace_na(n_linked_in_cell, 0L))) %>%
  # kyoku total (for P_sameKyoku = kyoku_total - ka_total)
  left_join(
    pool_kyoku %>% rename(cell_kyoku_group = kyoku_group, kyoku_total = pool_n),
    by = c("cell_kyoku_group", "cell_rank" = "target_rank")
  ) %>%
  mutate(P_sameKyoku = pmax(0L,
    replace_na(kyoku_total, 0L) - replace_na(S_i_total, 0L))) %>%
  # all total (for P_otherKyoku = all_total - kyoku_total)
  left_join(pool_all, by = c("cell_rank" = "target_rank")) %>%
  mutate(P_otherKyoku = pmax(0L,
    replace_na(all_n, 0L) - replace_na(kyoku_total, 0L))) %>%
  # log-transform pool variables
  mutate(
    log_S_i          = log(S_i + 1),
    log_P_sameKyoku  = log(P_sameKyoku + 1),
    log_P_otherKyoku = log(P_otherKyoku + 1)
  )

# ============================================================
# 5.  CHECKS
# ============================================================
cat("\n=== CHECKS ===\n")
cat("Kyoku in former-Shi sample (cell_kyoku_group):\n")
print(panel_v4 %>% count(cell_kyoku_group, sort = TRUE), n = Inf)
cat("\n")
cat("Kyoku clusters (kyoku_cl):", n_distinct(panel_v4$kyoku_cl, na.rm=TRUE), "\n")
cat("  Note: G=6 rather than 8 because 'assembly' (市会/府会事務局) and\n")
cat("  'reconstruction' (復興事業局) ka groups have no cells with observed\n")
cat("  kyoku+ka in the 1944 former-Shi panel after !is.na(ka_id) filter.\n\n")
cat("Total cells:", nrow(panel_v4), "\n")
cat("Total drafted men:", sum(panel_v4$n_drafted_male), "\n")
cat("Cells with S_i = 0:", sum(panel_v4$S_i == 0),
    sprintf("(%.1f%%)\n", 100*mean(panel_v4$S_i == 0)))
cat("Cells with no ka_group (cell_ka_group NA):", sum(is.na(panel_v4$cell_ka_group)), "\n")
cat("Mean S_i:", round(mean(panel_v4$S_i), 2), "\n")
cat("Mean P_sameKyoku:", round(mean(panel_v4$P_sameKyoku), 2), "\n")
cat("Mean P_otherKyoku:", round(mean(panel_v4$P_otherKyoku), 2), "\n\n")

# ============================================================
# 6.  ESTIMATION HELPERS  (singleton removal, int IDs — same as v3)
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

# ============================================================
# 7.  RUN ONE OUTCOME
# ============================================================
run_v4 <- function(y_name, data) {
  d <- remove_fe_singletons(add_int_ids(data), c("ka_id_int", "pos_norm_int"))
  G <- n_distinct(d$kyoku_cl_int)
  if (nrow(d) < 10 || G < 2) {
    return(tibble(outcome = y_name, term = NA_character_,
      estimate = NA_real_, std_error = NA_real_,
      p_feols = NA_real_, p_boot = NA_real_,
      n_cells = nrow(d), n_kyoku = G, diagnostic = "insufficient data"))
  }

  f <- as.formula(paste0(y_name,
    " ~ n_drafted_male + n_drafted_male:log_S_i + log_S_i",
    " + log_P_sameKyoku + log_P_otherKyoku",
    " | ka_id_int + pos_norm_int"))

  m <- tryCatch(feols(f, data = d, cluster = ~kyoku_cl_int), error = function(e) e)
  if (inherits(m, "error")) {
    return(tibble(outcome = y_name, term = NA_character_,
      estimate = NA_real_, std_error = NA_real_,
      p_feols = NA_real_, p_boot = NA_real_,
      n_cells = nrow(d), n_kyoku = G,
      diagnostic = conditionMessage(m)))
  }

  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    rename(estimate = Estimate, std_error = `Std. Error`,
           t_val = `t value`, p_feols = `Pr(>|t|)`)

  # boottest for b1 (n_drafted_male) and b2 (n_drafted_male:log_S_i)
  for (tm in c("n_drafted_male", "n_drafted_male:log_S_i")) {
    if (tm %in% ct$term) {
      bt <- tryCatch(
        boottest(m, param = tm, B = 9999L, type = "webb",
          clustid = ~kyoku_cl_int),
        error = function(e) list(p_val = NA_real_)
      )
      ct$p_boot[ct$term == tm] <- bt$p_val
    }
  }

  ct %>%
    mutate(outcome = y_name, n_cells = nobs(m), n_kyoku = G, diagnostic = "") %>%
    select(outcome, term, estimate, std_error, p_feols, p_boot,
           n_cells, n_kyoku, diagnostic)
}

# ============================================================
# 8.  OUTCOMES AND RUN
# ============================================================
outcomes_v4 <- c(
  n_same_section         = "Same ka",
  n_same_department      = "Same kyoku other ka",
  n_different_department = "Other kyoku",
  n_outside_own_ka       = "Outside own ka",
  n_total_internal       = "Total internal",
  n_new_hires            = "New hires"
)

cat("Running models...\n")
raw_results <- map_dfr(names(outcomes_v4), run_v4, data = panel_v4)
write_csv(raw_results, file.path(out_dir, "regression_v4_raw.csv"))

# ============================================================
# 9.  FORMATTED TABLE
# ============================================================
coef_labels <- c(
  "n_drafted_male"          = "b1 (D_i)",
  "n_drafted_male:log_S_i"  = "b2 (D_i × log S_i + 1)",
  "log_S_i"                 = "b3 (log S_i + 1)",
  "log_P_sameKyoku"         = "g1 (log P^{sameKyoku} + 1)",
  "log_P_otherKyoku"        = "g2 (log P^{otherKyoku} + 1)"
)
coef_order <- names(coef_labels)

stars <- function(p) case_when(
  is.na(p)  ~ "",
  p < 0.01  ~ "***",
  p < 0.05  ~ "**",
  p < 0.10  ~ "*",
  TRUE      ~ "")

fmt <- function(x, digits = 3) ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))

# Wide table: rows = coef stat, cols = outcomes
outcome_cols <- names(outcomes_v4)
outcome_labels <- unname(outcomes_v4)

# Build one tibble row per (coef, stat_type)
make_table_rows <- function(results) {
  rows <- list()
  for (cn in coef_order) {
    sub <- results %>% filter(term == cn)
    # estimate row
    est_row <- c(coef_labels[cn], sapply(outcome_cols, function(oc) {
      r <- sub %>% filter(outcome == oc)
      if (!nrow(r)) return("")
      p <- coalesce(r$p_boot, r$p_feols)
      paste0(fmt(r$estimate), stars(p))
    }))
    # SE row
    se_row <- c("", sapply(outcome_cols, function(oc) {
      r <- sub %>% filter(outcome == oc)
      if (!nrow(r)) return("")
      paste0("(", fmt(r$std_error), ")")
    }))
    # p_boot row (only for b1 and b2)
    if (cn %in% c("n_drafted_male", "n_drafted_male:log_S_i")) {
      pb_row <- c("[Webb p]", sapply(outcome_cols, function(oc) {
        r <- sub %>% filter(outcome == oc)
        if (!nrow(r) || is.na(r$p_boot)) return("")
        paste0("[", fmt(r$p_boot), "]")
      }))
      rows <- c(rows, list(est_row, se_row, pb_row))
    } else {
      rows <- c(rows, list(est_row, se_row))
    }
  }
  # N and G rows
  n_row <- c("N", sapply(outcome_cols, function(oc) {
    r <- results %>% filter(outcome == oc, !is.na(n_cells)) %>% slice(1)
    if (!nrow(r)) "" else as.character(r$n_cells)
  }))
  g_row <- c("G (kyoku)", sapply(outcome_cols, function(oc) {
    r <- results %>% filter(outcome == oc, !is.na(n_kyoku)) %>% slice(1)
    if (!nrow(r)) "" else as.character(r$n_kyoku)
  }))
  rows <- c(rows, list(n_row, g_row))
  do.call(rbind, rows) %>% as.data.frame()
}

tbl <- make_table_rows(raw_results)
colnames(tbl) <- c("", outcome_labels)

# Save CSV
write_csv(tbl, file.path(out_dir, "table_v4.csv"))

# ============================================================
# 10.  LaTeX TABLE
# ============================================================
nc <- length(outcome_labels)
col_spec <- paste0("l", paste(rep("r", nc), collapse = ""))
header   <- paste(c("", outcome_labels), collapse = " & ")

sep_after <- c(
  which(tbl[[1]] == "[Webb p]")[1],                # after b1 p_boot
  which(tbl[[1]] == "[Webb p]")[2],                # after b2 p_boot
  which(tbl[[1]] == paste0("(", fmt(
    raw_results %>% filter(term=="log_S_i") %>% slice(1) %>% pull(std_error)), ")")),
  which(tbl[[1]] == paste0("(", fmt(
    raw_results %>% filter(term=="log_P_sameKyoku") %>% slice(1) %>% pull(std_error)), ")")),
  nrow(tbl) - 2   # before N
)

tex_rows <- apply(tbl, 1, function(r) paste(r, collapse = " & "))

tex_lines <- c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{Pre-draft pool and draft response, 1944 (former-Shi offices)}",
  "  \\label{tab:predraft_v4}",
  paste0("  \\begin{tabular}{", col_spec, "}"),
  "    \\hline\\hline",
  paste0("    ", header, " \\\\"),
  "    \\hline"
)
for (i in seq_len(nrow(tbl))) {
  tex_lines <- c(tex_lines, paste0("    ", tex_rows[i], " \\\\"))
  if (i == nrow(tbl) - 2) tex_lines <- c(tex_lines, "    \\hline")
}
tex_lines <- c(tex_lines,
  "    \\hline\\hline",
  "  \\end{tabular}",
  "  \\\\[4pt]",
  paste0("  \\footnotesize Stars on \\(b_1\\) and \\(b_2\\) use Webb bootstrap p-values (B\\,=\\,9{,}999)."),
  "  Parentheses: cluster-robust SE (kyoku).",
  "  Brackets: Webb p-value.",
  "  FE: kyoku $\\times$ ka, pos\\_norm.",
  "\\end{table}"
)

writeLines(tex_lines, file.path(out_dir, "table_v4.tex"), useBytes = TRUE)

cat("\n=== V4 Results ===\n")
print(tbl, row.names = FALSE)
cat("\nFiles written to:", out_dir, "\n")
cat("V4 complete.\n")
