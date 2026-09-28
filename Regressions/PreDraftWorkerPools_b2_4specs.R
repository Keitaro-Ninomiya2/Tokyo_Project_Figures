################################################################################
# PreDraftWorkerPools1944 — b2 across four specifications
#
# b_2 = coefficient on D_i × log(S_i+1) for five outcomes:
#   same ka, other ka same kyoku, other kyoku, outside own ka, total internal
#
# Spec 1: OLS, kyoku-clustered (v4 main spec)
# Spec 2: PPML (fepois), same regressors + FE, kyoku-clustered
# Spec 3: OLS, occupation-level pool — S = same pos_norm × ka_group linked men
# Spec 4: OLS as (1), ka-clustered instead of kyoku-clustered
#
# For each cell: b2, SE, Webb boottest p (B=9,999), N, n_clusters
# Spec 4 bootstrap uses ka clusters; all others use kyoku clusters.
#
# Output: tables/b2_4specs_raw.csv, b2_4specs.csv, b2_4specs.tex
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
eval(parse(text = helper_lines[55:126]))

rank_pos <- function(x) case_when(
  x %in% c("Unknown", "\u517c")                                       ~ NA_integer_,
  str_detect(x, "\u96c7|\u56d1\u8a17|\u81e8\u6642|\u898b\u7fd2")     ~ 1L,
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b"                      ~ 3L,
  TRUE                                                                 ~ 2L)

# ============================================================
# 1.  LOAD DATA
# ============================================================
cat("Loading data...\n")
df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE) %>%
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

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE) %>%
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
# 2.  FORMER-SHI FLAG
# ============================================================
shi_offices <- df_all %>%
  filter(year_num == 1944, gov_level == "TokyoTo", !is.na(office_id)) %>%
  select(staff_id, office_id_44 = office_id) %>%
  left_join(df_all %>% filter(year_num == 1942) %>%
              select(staff_id, gov_42 = gov_level), by = "staff_id") %>%
  group_by(office_id_44) %>%
  summarise(n_linked   = sum(!is.na(gov_42)),
            n_from_shi = sum(gov_42 == "TokyoShi", na.rm = TRUE),
            share_shi  = if_else(n_linked > 0, n_from_shi / n_linked, NA_real_),
            .groups = "drop") %>%
  filter(!is.na(share_shi), share_shi > 0.5) %>%
  pull(office_id_44)

# ============================================================
# 3.  OUTCOME PANEL  (cell = office_id × kakari × pos_norm)
# ============================================================
office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num, lag_kyoku_group = kyoku_group,
         lag_ka = ka, lag_ka_group = ka_group) %>%
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
    )
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_section         = sum(arrival_type == "same_section",         na.rm = TRUE),
    n_same_department      = sum(arrival_type == "same_department",      na.rm = TRUE),
    n_different_department = sum(arrival_type == "different_department", na.rm = TRUE),
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
# 4.  POOL VARIABLES — v4 spec (rank-based S_i)
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
cat("Linked men (1944, defined rank):", nrow(men44_linked), "\n")

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
  mutate(cell_rank   = rank_pos(pos_norm),
         kyoku_clean = replace_na(kyoku, ""),
         ka_clean    = replace_na(ka, "")) %>%
  filter(!is.na(cell_rank)) %>%
  left_join(
    ka_group_raw %>% filter(year == 1944) %>%
      select(kyoku, ka, cell_ka_group = ka_group),
    by = c("kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(cell_kyoku_group = assign_kyoku_group(normalize_kyoku(kyoku))) %>%
  left_join(pool_ka %>% rename(cell_ka_group = ka_group, S_i_total = pool_n),
            by = c("cell_ka_group", "cell_rank" = "target_rank")) %>%
  left_join(cell_linked_cnt, by = c("office_id", "kakari", "pos_norm")) %>%
  mutate(S_i = pmax(0L, replace_na(S_i_total, 0L) - replace_na(n_linked_in_cell, 0L))) %>%
  left_join(pool_kyoku %>% rename(cell_kyoku_group = kyoku_group, kyoku_total = pool_n),
            by = c("cell_kyoku_group", "cell_rank" = "target_rank")) %>%
  mutate(P_sameKyoku = pmax(0L, replace_na(kyoku_total, 0L) - replace_na(S_i_total, 0L))) %>%
  left_join(pool_all, by = c("cell_rank" = "target_rank")) %>%
  mutate(P_otherKyoku = pmax(0L, replace_na(all_n, 0L) - replace_na(kyoku_total, 0L))) %>%
  mutate(log_S_i          = log(S_i + 1),
         log_P_sameKyoku  = log(P_sameKyoku + 1),
         log_P_otherKyoku = log(P_otherKyoku + 1))

cat("V4 panel:", nrow(panel_v4), "cells |",
    n_distinct(panel_v4$kyoku_cl, na.rm=TRUE), "kyoku clusters\n")

# ============================================================
# 5.  POOL VARIABLES — Spec 3: occupation-level (pos_norm × ka_group)
#
# S_pos_i  = linked men in same ka_group AND same pos_norm, excluding cell i
# P_sameKyoku_pos = same in other ka of same kyoku
# P_otherKyoku_pos = same in other kyoku
# ============================================================
ka_posn_totals <- men44_linked %>%
  filter(!is.na(ka_group)) %>%
  count(ka_group, pos_norm, name = "n")

kyoku_posn_totals <- men44_linked %>%
  filter(!is.na(kyoku_group)) %>%
  count(kyoku_group, pos_norm, name = "n")

all_posn_totals <- men44_linked %>%
  count(pos_norm, name = "n")

panel_spec3 <- panel_v4 %>%
  # ka-level pool by pos_norm
  left_join(ka_posn_totals %>% rename(cell_ka_group = ka_group, S_pos_total = n),
            by = c("cell_ka_group", "pos_norm")) %>%
  left_join(cell_linked_cnt, by = c("office_id", "kakari", "pos_norm"),
            suffix = c("", "_dup")) %>%
  mutate(S_pos = pmax(0L,
    replace_na(S_pos_total, 0L) - replace_na(n_linked_in_cell, 0L))) %>%
  # kyoku-level pool by pos_norm
  left_join(kyoku_posn_totals %>%
              rename(cell_kyoku_group = kyoku_group, kyoku_pos_total = n),
            by = c("cell_kyoku_group", "pos_norm")) %>%
  mutate(P_sameKyoku_pos = pmax(0L,
    replace_na(kyoku_pos_total, 0L) - replace_na(S_pos_total, 0L))) %>%
  # all-level pool by pos_norm
  left_join(all_posn_totals %>% rename(all_pos_n = n),
            by = "pos_norm") %>%
  mutate(P_otherKyoku_pos = pmax(0L,
    replace_na(all_pos_n, 0L) - replace_na(kyoku_pos_total, 0L))) %>%
  mutate(log_S_pos           = log(S_pos + 1),
         log_P_sameKyoku_pos = log(P_sameKyoku_pos + 1),
         log_P_otherKyoku_pos = log(P_otherKyoku_pos + 1))

cat("Spec 3: mean S_pos =", round(mean(panel_spec3$S_pos), 2),
    "| S_pos=0:", sum(panel_spec3$S_pos == 0), "\n")

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

# PPML extra: also drop FE groups where the outcome sums to zero
# (fepois drops these internally; boottest requires they be removed beforehand)
remove_ppml_zero_fes <- function(data, fes, y_var) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      grp_sums <- tapply(data[[y_var]], data[[fe]], sum, na.rm = TRUE)
      keep     <- names(grp_sums[grp_sums > 0])
      data     <- data[data[[fe]] %in% keep, ]
    }
    # Re-run OLS singletons after zero removal
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
    mutate(ka_id_int    = as.integer(factor(ka_id)),
           pos_norm_int = as.integer(factor(pos_norm)),
           kyoku_cl_int = as.integer(factor(kyoku_cl)))
}

# ============================================================
# 7.  run_b2: fit one model, return b2 row
# ============================================================
run_b2 <- function(y_name, data, rhs, b2_term,
                   model_type   = "ols",    # "ols" or "ppml"
                   cluster_var  = "kyoku_cl_int") {

  d <- remove_fe_singletons(add_int_ids(data), c("ka_id_int", "pos_norm_int"))
  if (nrow(d) < 10) return(NULL)

  # PPML: additionally drop FE groups with zero outcome sum (fepois drops them
  # internally; boottest requires they be absent before fitting)
  if (model_type == "ppml")
    d <- remove_ppml_zero_fes(d, c("ka_id_int", "pos_norm_int"), y_name)
  if (nrow(d) < 10) return(NULL)

  G <- n_distinct(d[[cluster_var]])
  cl_form <- as.formula(paste0("~", cluster_var))
  f <- as.formula(paste0(y_name, " ~ ", rhs, " | ka_id_int + pos_norm_int"))

  m <- tryCatch(
    if (model_type == "ppml") fepois(f, data = d, cluster = cl_form)
    else                      feols( f, data = d, cluster = cl_form),
    error = function(e) { cat("  fit error [", y_name, "]:", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)

  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    rename(estimate = Estimate, std_error = `Std. Error`) %>%
    rename_with(~ "p_feols", any_of(c("Pr(>|t|)", "Pr(>|z|)")))

  r <- ct %>% filter(term == b2_term)
  if (!nrow(r)) {
    cat("  b2 term not found [", y_name, "]:", b2_term, "| available:",
        paste(ct$term, collapse=", "), "\n")
    return(NULL)
  }

  bt <- tryCatch(
    boottest(m, param = b2_term, B = 9999L, type = "webb", clustid = cl_form),
    error = function(e) { cat("  boot error [", y_name, "]:", conditionMessage(e), "\n");
                          list(p_val = NA_real_) })

  tibble(outcome = y_name, model = model_type, cluster = cluster_var,
         b2 = r$estimate, se = r$std_error,
         p_feols = r$p_feols, p_boot = bt$p_val,
         n_cells = nobs(m), n_clusters = G)
}

# ============================================================
# 8.  RUN FOUR SPECS × FIVE OUTCOMES
# ============================================================
OUTCOMES <- c("n_same_section", "n_same_department",
              "n_different_department", "n_outside_own_ka", "n_total_internal")
LABELS   <- c("Same ka", "Other ka, same kyoku",
              "Other kyoku", "Outside own ka", "Total internal")

RHS_V4   <- paste("n_drafted_male + n_drafted_male:log_S_i + log_S_i",
                  "+ log_P_sameKyoku + log_P_otherKyoku")
RHS_S3   <- paste("n_drafted_male + n_drafted_male:log_S_pos + log_S_pos",
                  "+ log_P_sameKyoku_pos + log_P_otherKyoku_pos")

specs <- list(
  list(tag="spec1", rhs=RHS_V4, b2="n_drafted_male:log_S_i",
       type="ols",  cl="kyoku_cl_int", data=panel_v4),
  list(tag="spec2", rhs=RHS_V4, b2="n_drafted_male:log_S_i",
       type="ppml", cl="kyoku_cl_int", data=panel_v4),
  list(tag="spec3", rhs=RHS_S3, b2="n_drafted_male:log_S_pos",
       type="ols",  cl="kyoku_cl_int", data=panel_spec3),
  list(tag="spec4", rhs=RHS_V4, b2="n_drafted_male:log_S_i",
       type="ols",  cl="ka_id_int",   data=panel_v4)
)

cat("\nRunning 4 specs × 5 outcomes...\n")
raw <- map_dfr(specs, function(sp) {
  cat("  ", sp$tag, "(", sp$type, "cluster =", sp$cl, ")\n")
  map_dfr(OUTCOMES, run_b2,
          data        = sp$data,
          rhs         = sp$rhs,
          b2_term     = sp$b2,
          model_type  = sp$type,
          cluster_var = sp$cl) %>%
    mutate(spec = sp$tag)
})

write_csv(raw, file.path(out_dir, "b2_4specs_raw.csv"))
cat("Raw results saved. Rows:", nrow(raw), "\n")

# ============================================================
# 9.  FORMATTED TABLE
# ============================================================
fmt  <- function(x, d = 3) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))
strs <- function(p) case_when(
  is.na(p)  ~ "",
  p < 0.01  ~ "***",
  p < 0.05  ~ "**",
  p < 0.10  ~ "*",
  TRUE      ~ "")

# Wide CSV / LaTeX layout
# Row 1: outcome label, b2 + stars in spec columns
# Row 2: blank label, (se) in spec columns
# Footer: Cells, Clusters

SPEC_TAGS   <- c("spec1", "spec2", "spec3", "spec4")
SPEC_LABELS <- c("(1) OLS, kyoku", "(2) PPML, kyoku",
                 "(3) OLS, pos\\_norm pool", "(4) OLS, ka cluster")

rows <- list()

for (i in seq_along(OUTCOMES)) {
  oc  <- OUTCOMES[i]
  lbl <- LABELS[i]

  b2_cells <- map_chr(SPEC_TAGS, function(sp) {
    r <- raw %>% filter(outcome == oc, spec == sp)
    if (!nrow(r)) return("")
    # PPML: p_boot NA → fall back to cluster-robust Wald p; OLS: use bootstrap p
    p_star <- coalesce(r$p_boot, r$p_feols)
    paste0(fmt(r$b2), strs(p_star))
  })
  se_cells <- map_chr(SPEC_TAGS, function(sp) {
    r <- raw %>% filter(outcome == oc, spec == sp)
    if (!nrow(r)) return("")
    paste0("(", fmt(r$se), ")")
  })

  rows[[length(rows)+1]] <- c(lbl, b2_cells)
  rows[[length(rows)+1]] <- c("",  se_cells)
}

# Footer: use n_total_internal row for N (most cells, fewest PPML zero removals)
N_cells_row <- c("Cells", map_chr(SPEC_TAGS, function(sp) {
  r <- raw %>% filter(spec == sp, outcome == "n_total_internal")
  if (!nrow(r)) "" else as.character(r$n_cells)
}))
G_row <- c("Clusters", map_chr(SPEC_TAGS, function(sp) {
  r <- raw %>% filter(spec == sp, outcome == "n_total_internal")
  if (!nrow(r)) "" else as.character(r$n_clusters)
}))
rows[[length(rows)+1]] <- N_cells_row
rows[[length(rows)+1]] <- G_row

tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(tbl) <- c("Outcome", SPEC_LABELS)
write_csv(tbl, file.path(out_dir, "b2_4specs.csv"))

# ============================================================
# 10.  LaTeX TABLE
# ============================================================
nc  <- ncol(tbl) - 1
cs  <- paste0("l", paste(rep("r", nc), collapse = ""))
hdr <- paste(colnames(tbl), collapse = " & ")

is_footer <- tbl[[1]] %in% c("Cells", "Clusters")

tex <- c(
  "\\begin{table}[!htbp]",
  "  \\centering",
  "  \\caption{$\\hat{\\beta}_2 = D_i \\times \\log(S_i+1)$ across specifications (1944, former-Shi)}",
  "  \\label{tab:b2_4specs}",
  paste0("  \\begin{tabular}{", cs, "}"),
  "    \\hline\\hline",
  paste0("    ", hdr, " \\\\"),
  "    \\hline"
)

for (i in seq_len(nrow(tbl))) {
  row_str <- paste(as.character(tbl[i, ]), collapse = " & ")
  if (tbl[[1]][i] == "Cells") tex <- c(tex, "    \\hline")
  tex <- c(tex, paste0("    ", row_str, " \\\\"))
}

tex <- c(tex,
  "    \\hline\\hline",
  "  \\end{tabular}",
  "  \\\\[4pt]",
  "  \\footnotesize",
  "  Outcome: number of 1944 arrivals of each type.",
  "  $S_i$ in (1)--(2)--(4): linked men in same ka group at cell rank or rank below, excl.~cell $i$.",
  "  $S_i$ in (3): linked men in same ka group \\emph{and} same pos\\_norm, excl.~cell $i$.",
  "  $P$ controls analogous.",
  "  FE: kyoku$\\times$ka and pos\\_norm.",
  "  Stars: (1)(3)(4) Webb bootstrap p; (2) cluster-robust Wald p (fwildclusterboot does not support PPML).",
  "  (* 0.10, ** 0.05, *** 0.01). (1)--(3) cluster by kyoku ($G=6$); (4) clusters by ka.",
  "  Cells for (2) shows $n$ used in the total-internal regression;",
  "  other PPML outcomes drop additional cells with all-zero outcomes in an FE group.",
  "\\end{table}"
)

writeLines(tex, file.path(out_dir, "b2_4specs.tex"), useBytes = TRUE)

cat("\n=== RESULTS ===\n")
print(tbl, row.names = FALSE)
cat("\nFiles written to:", out_dir, "\n")
cat("Done.\n")
