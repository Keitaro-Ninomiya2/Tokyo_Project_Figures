################################################################################
# PreDraftWorkerPools_v7.R
#
# Baseline pool switched to 1942 ka assignment.
#
# S_i = linked men (1942 ∩ 1944) placed by their 1942 ka_group,
#        rank = cell_rank or cell_rank-1.
#        Cell exclusion: men whose 1942 ka_group = cell_ka_group
#        AND who end up in cell i (office × kakari × pos_norm) in 1944.
#
# Columns (all five outcomes, kyoku × ka + pos_norm FE):
#  (1) OLS, ka cluster
#  (2) PPML, ka cluster  — OLS-scale APE derivative in main table
#  (3) OLS, occupation-level pool (same pos_norm × 1942 ka), ka cluster
#  (4) OLS spline: D×1[S=0] + D×log(S+1)×1[S>0] + main effects, ka cluster
#  (5) OLS, kyoku cluster
#
# Table layout:
#   Panel A: b2 [= D×log(S+1) for cols 1,2,3,5; slope for col 4], coef + SE rows
#   Panel B: col 4 D×1[S=0] only, coef + SE rows
#   Footer: Cells, Clusters
#
# Outputs:
#   tables/v7_main_raw.csv
#   tables/v7_main.{csv,tex}
#   tables/v7_ppml_diag.csv
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
  x %in% c("Unknown", "\u517c")                                        ~ NA_integer_,
  str_detect(x, "\u96c7|\u56d1\u8a17|\u81e8\u6642|\u898b\u7fd2")      ~ 1L,
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b"                       ~ 3L,
  TRUE                                                                  ~ 2L)

# ============================================================
# 1.  LOAD DATA
# ============================================================
cat("Loading data...\n")
df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num    = as.numeric(year), is_female = gender_modern == "female",
         pos_norm    = str_replace_all(position, "\\s+", ""),
         kyoku_clean = replace_na(kyoku, ""), ka_clean = replace_na(ka, ""),
         norm_kyoku  = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE) %>%
  mutate(year_num  = as.numeric(year), is_female = gender_modern == "female",
         pos_norm  = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names <- df_names %>%
  left_join(ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka"))

# ============================================================
# 2.  OUTCOME PANEL  (same as v4/v6)
# ============================================================
shi_offices <- df_all %>%
  filter(year_num == 1944, gov_level == "TokyoTo", !is.na(office_id)) %>%
  select(staff_id, office_id_44 = office_id) %>%
  left_join(df_all %>% filter(year_num == 1942) %>%
              select(staff_id, gov_42 = gov_level), by = "staff_id") %>%
  group_by(office_id_44) %>%
  summarise(n_linked=sum(!is.na(gov_42)), n_from_shi=sum(gov_42=="TokyoShi",na.rm=TRUE),
            share_shi=if_else(n_linked>0, n_from_shi/n_linked, NA_real_), .groups="drop") %>%
  filter(!is.na(share_shi), share_shi > 0.5) %>% pull(office_id_44)

office_initial_year <- df_names %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df_names %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")
staff_lag <- df_names %>% distinct(staff_id, year_num, .keep_all=TRUE) %>%
  select(staff_id, year_num, lag_kyoku_group=kyoku_group,
         lag_ka=ka, lag_ka_group=ka_group) %>%
  mutate(year_num = year_num + 1L)

staff_transitions <- df_names %>% filter(year_num == 1944) %>%
  left_join(office_initial_year, by="office_id") %>%
  left_join(staff_first_year,    by="staff_id") %>%
  left_join(staff_lag,           by=c("staff_id","year_num")) %>%
  mutate(arrival_type = case_when(
    is.na(lag_kyoku_group) | is.na(kyoku_group)              ~ NA_character_,
    !is.na(ka_group) & !is.na(lag_ka_group) & ka_group==lag_ka_group ~ "same_section",
    (is.na(ka_group)|is.na(lag_ka_group)) & lag_kyoku_group==kyoku_group &
      !is.na(lag_ka)&!is.na(ka)&lag_ka==ka                   ~ "same_section",
    lag_kyoku_group == kyoku_group                            ~ "same_department",
    lag_kyoku_group != kyoku_group                            ~ "different_department",
    TRUE                                                      ~ NA_character_))

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(n_same_section         = sum(arrival_type=="same_section",  na.rm=TRUE),
            n_same_department      = sum(arrival_type=="same_department",na.rm=TRUE),
            n_different_department = sum(arrival_type=="different_department",na.rm=TRUE),
            .groups="drop") %>%
  mutate(n_total_internal = n_same_section+n_same_department+n_different_department,
         n_outside_own_ka = n_same_department + n_different_department)

position_drafts <- df_all %>% filter(year_num==1944, drafted==TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted_male=sum(!is_female,na.rm=TRUE), .groups="drop")

panel_base <- position_outcomes %>%
  left_join(position_drafts, by=c("office_id","kakari","pos_norm","year_num")) %>%
  mutate(n_drafted_male=replace_na(n_drafted_male,0L),
         ka_id        = if_else(!is.na(ka)&!is.na(kyoku),paste(kyoku,ka,sep="_"),NA_character_),
         is_former_shi= office_id %in% shi_offices,
         kyoku_cl     = normalize_kyoku(kyoku)) %>%
  filter(!is.na(ka_id))

# ============================================================
# 3.  POOL — 1942 ka assignment (v7 baseline)
# ============================================================
ids42_male <- df_names %>% filter(year_num==1942, !is_female) %>% pull(staff_id) %>% unique()
linked_ids <- intersect(ids42_male,
  df_names %>% filter(year_num==1944) %>% pull(staff_id) %>% unique())

# 1942 records for linked men
men42_linked <- df_names %>%
  filter(year_num==1942, staff_id %in% linked_ids, !is_female) %>%
  mutate(position_rank = rank_pos(pos_norm)) %>%
  filter(!is.na(position_rank))

# 1944 cell memberships (for cell exclusion)
men44_cells <- df_names %>%
  filter(year_num==1944, staff_id %in% linked_ids, !is_female) %>%
  select(staff_id, office_id_44=office_id, kakari_44=kakari, pos_norm_44=pos_norm)

cat("Linked men in 1942 (defined rank):", nrow(men42_linked), "\n")

# ---- Pool totals from 1942 positions ----
ka_rank42    <- men42_linked %>% filter(!is.na(ka_group))    %>% count(ka_group,    position_rank, name="n")
kyoku_rank42 <- men42_linked %>% filter(!is.na(kyoku_group)) %>% count(kyoku_group, position_rank, name="n")
all_rank42   <- men42_linked %>% count(position_rank, name="n")
ka_posn42    <- men42_linked %>% filter(!is.na(ka_group))    %>% count(ka_group, pos_norm, name="n")
kyoku_posn42 <- men42_linked %>% filter(!is.na(kyoku_group)) %>% count(kyoku_group, pos_norm, name="n")
all_posn42   <- men42_linked %>% count(pos_norm, name="n")

pool_lookup <- function(df, group_col, target_ranks=1:3) {
  df %>% crossing(target_rank=target_ranks) %>%
    filter(position_rank >= pmax(1L,target_rank-1L), position_rank <= target_rank) %>%
    group_by(across(all_of(c(group_col,"target_rank")))) %>%
    summarise(pool_n=sum(n,na.rm=TRUE), .groups="drop")
}
pool_ka42    <- pool_lookup(ka_rank42,    "ka_group")
pool_kyoku42 <- pool_lookup(kyoku_rank42, "kyoku_group")
pool_all42   <- pool_lookup(all_rank42,   character(0)) %>% rename(all_n=pool_n)

# ---- Cell exclusion: linked men whose 1942 ka_group matches cell_ka_group
#      AND who land in that cell in 1944  ----
# Join 1942 ka_group onto 1944 cell membership
men_cross <- men42_linked %>%
  select(staff_id, ka_group_42=ka_group, pos_norm_42=pos_norm) %>%
  left_join(men44_cells, by="staff_id") %>%
  filter(!is.na(office_id_44))

# Rank-pool exclusion: count by (1944 cell, 1942 ka_group)
cell_excl_rank42 <- men_cross %>%
  filter(!is.na(ka_group_42)) %>%
  count(office_id_44, kakari_44, pos_norm_44, ka_group_42, name="n_excl_rank")

# Pos_norm-pool exclusion: men whose 1942 ka_group = cell_ka_group AND
# 1942 pos_norm = cell's 1944 pos_norm (only those entries count against the pool).
# Since pool definition requires pos_norm_42 = cell_pos_norm = pos_norm_44,
# filter to pos_norm_42 == pos_norm_44 before counting.
cell_excl_posn42 <- men_cross %>%
  filter(!is.na(ka_group_42), !is.na(pos_norm_42),
         pos_norm_42 == pos_norm_44) %>%
  count(office_id_44, kakari_44, pos_norm_44, ka_group_42, name="n_excl_posn")

# ---- Build v7 panel ----
build_v7_panel <- function(panel_in) {
  p <- panel_in %>% filter(is_former_shi) %>%
    mutate(cell_rank   = rank_pos(pos_norm),
           kyoku_clean = replace_na(kyoku,""), ka_clean = replace_na(ka,"")) %>%
    filter(!is.na(cell_rank)) %>%
    # Cell's 1944 ka_group (used as join key against 1942 pools)
    left_join(ka_group_raw %>% filter(year==1944) %>%
                select(kyoku, ka, cell_ka_group=ka_group),
              by=c("kyoku_clean"="kyoku","ka_clean"="ka")) %>%
    mutate(cell_kyoku_group = assign_kyoku_group(normalize_kyoku(kyoku))) %>%
    # Rank pool total (from 1942 ka)
    left_join(pool_ka42    %>% rename(cell_ka_group=ka_group,    S_i_total=pool_n),
              by=c("cell_ka_group","cell_rank"="target_rank")) %>%
    # Rank cell exclusion (1942 ka_group × 1944 cell)
    left_join(cell_excl_rank42 %>%
                rename(cell_ka_group=ka_group_42),
              by=c("office_id"="office_id_44","kakari"="kakari_44",
                   "pos_norm"="pos_norm_44","cell_ka_group")) %>%
    mutate(S_i = pmax(0L, replace_na(S_i_total,0L) - replace_na(n_excl_rank,0L))) %>%
    # Kyoku pool
    left_join(pool_kyoku42 %>% rename(cell_kyoku_group=kyoku_group, kyoku_total=pool_n),
              by=c("cell_kyoku_group","cell_rank"="target_rank")) %>%
    # Global pool
    left_join(pool_all42, by=c("cell_rank"="target_rank")) %>%
    mutate(
      P_sameKyoku  = pmax(0L, replace_na(kyoku_total,0L)-replace_na(S_i_total,0L)),
      P_otherKyoku = pmax(0L, replace_na(all_n,0L)     -replace_na(kyoku_total,0L)),
      log_S_i          = log(S_i+1),
      log_P_sameKyoku  = log(P_sameKyoku+1),
      log_P_otherKyoku = log(P_otherKyoku+1),
      # Spline
      S_zero        = as.integer(S_i == 0),
      log_S_nonzero = if_else(S_i>0, log(S_i+1), 0),
      D_S0          = n_drafted_male * S_zero,
      D_logS1       = n_drafted_male * log_S_nonzero)
}

# Spec-3 pos_norm pool (1942 ka × pos_norm)
add_posn_pool <- function(panel_in) {
  panel_in %>%
    left_join(ka_posn42    %>% rename(cell_ka_group=ka_group,    S_pos_total=n),
              by=c("cell_ka_group","pos_norm")) %>%
    left_join(cell_excl_posn42 %>%
                rename(cell_ka_group=ka_group_42),
              by=c("office_id"="office_id_44","kakari"="kakari_44",
                   "pos_norm"="pos_norm_44","cell_ka_group")) %>%
    mutate(S_pos = pmax(0L, replace_na(S_pos_total,0L) - replace_na(n_excl_posn,0L))) %>%
    left_join(kyoku_posn42 %>% rename(cell_kyoku_group=kyoku_group, kyoku_pos=n),
              by=c("cell_kyoku_group","pos_norm")) %>%
    left_join(all_posn42 %>% rename(all_pos=n), by="pos_norm") %>%
    mutate(
      P_sameKyoku_pos  = pmax(0L, replace_na(kyoku_pos,0L)-replace_na(S_pos_total,0L)),
      P_otherKyoku_pos = pmax(0L, replace_na(all_pos,0L)  -replace_na(kyoku_pos,0L)),
      log_S_pos           = log(S_pos+1),
      log_P_sameKyoku_pos = log(P_sameKyoku_pos+1),
      log_P_otherKyoku_pos= log(P_otherKyoku_pos+1))
}

panel_v7   <- build_v7_panel(panel_base)
panel_v7_3 <- add_posn_pool(panel_v7)

cat("V7 panel:", nrow(panel_v7), "cells\n")

# Pool diagnostics
excl_total <- sum(cell_excl_rank42$n_excl_rank, na.rm=TRUE)
cat("\nPool diagnostics (v7, 1942 ka):\n")
cat("  mean S_i:", round(mean(panel_v7$S_i, na.rm=TRUE),2), "\n")
cat("  pct S_i=0:", round(100*mean(panel_v7$S_i==0, na.rm=TRUE),1), "%\n")
cat("  total cell exclusions removed:", excl_total, "\n")
cat("  mean cell exclusion (conditional on excl>0):",
    round(mean(cell_excl_rank42$n_excl_rank),2), "\n")
cat("  mean S_pos (spec 3):", round(mean(panel_v7_3$S_pos, na.rm=TRUE),2), "\n")
cat("  pct S_pos=0 (spec 3):", round(100*mean(panel_v7_3$S_pos==0, na.rm=TRUE),1), "%\n")

# ============================================================
# 4.  HELPERS
# ============================================================
remove_fe_singletons <- function(data, fes) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      cnt  <- table(data[[fe]])
      data <- data[data[[fe]] %in% names(cnt[cnt>1]), ]
    }
    if (nrow(data)==n0) break
  }
  data
}

remove_ppml_zero_fes <- function(data, fes, y_var) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      grp  <- tapply(data[[y_var]], data[[fe]], sum, na.rm=TRUE)
      data <- data[data[[fe]] %in% names(grp[grp>0]), ]
    }
    for (fe in fes) {
      cnt  <- table(data[[fe]])
      data <- data[data[[fe]] %in% names(cnt[cnt>1]), ]
    }
    if (nrow(data)==n0) break
  }
  data
}

add_int_ids <- function(data) {
  data %>% filter(!is.na(kyoku_cl)) %>%
    mutate(ka_id_int    = as.integer(factor(ka_id)),
           pos_norm_int = as.integer(factor(pos_norm)),
           kyoku_cl_int = as.integer(factor(kyoku_cl)))
}

FES <- c("ka_id_int","pos_norm_int")

get_coef_row <- function(m, term) {
  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    rename_with(~"p_feols", any_of(c("Pr(>|t|)","Pr(>|z|)")))
  ct %>% filter(.data$term == !!term) %>%
    select(term, estimate=Estimate, std_error=`Std. Error`, p_feols)
}

run_ols_boot <- function(y, data, rhs, b_term, cluster_var) {
  d  <- remove_fe_singletons(add_int_ids(data), FES)
  cl <- as.formula(paste0("~",cluster_var))
  f  <- as.formula(paste0(y," ~ ",rhs," | ka_id_int + pos_norm_int"))
  m  <- tryCatch(feols(f,data=d,cluster=cl),
                 error=function(e){cat("  feols error [",y,"]:",conditionMessage(e),"\n");NULL})
  if (is.null(m)) return(NULL)
  r  <- get_coef_row(m, b_term)
  if (!nrow(r)) return(NULL)
  bt <- tryCatch(boottest(m,param=b_term,B=9999L,type="webb",clustid=cl),
                 error=function(e) list(p_val=NA_real_))
  tibble(outcome=y, term=b_term,
         b=r$estimate, se=r$std_error, p_feols=r$p_feols, p_boot=bt$p_val,
         n_cells=nobs(m), n_clusters=n_distinct(d[[cluster_var]]))
}

run_ppml_diag <- function(y, data, rhs, b_term) {
  d_base <- remove_fe_singletons(add_int_ids(data), FES)
  d_ppml <- remove_ppml_zero_fes(d_base, FES, y)
  f      <- as.formula(paste0(y," ~ ",rhs," | ka_id_int + pos_norm_int"))
  m_ppml <- tryCatch(fepois(f, data=d_ppml, cluster=~ka_id_int),
                     error=function(e){cat("  fepois error [",y,"]:",conditionMessage(e),"\n");NULL})
  if (is.null(m_ppml)) return(NULL)
  n_iter    <- { v <- tryCatch(m_ppml$iterations, error=function(e) NULL); if(is.null(v)) NA_integer_ else as.integer(v) }
  converged <- { v <- tryCatch(m_ppml$convStatus,  error=function(e) NULL); if(is.null(v)) NA else isTRUE(v) }
  n_ka      <- n_distinct(d_ppml[["ka_id_int"]])
  r_ppml    <- get_coef_row(m_ppml, b_term)
  b2_nat    <- r_ppml$estimate; se_nat <- r_ppml$std_error; p_wald <- r_ppml$p_feols
  mu_bar    <- mean(fitted(m_ppml), na.rm=TRUE)
  tibble(outcome=y,
         N_base=nrow(d_base), N_ppml=nrow(d_ppml), N_dropped=nrow(d_base)-nrow(d_ppml),
         converged=converged, n_iter=n_iter, n_ka_clusters=n_ka,
         mu_bar=mu_bar,
         b2_native=b2_nat, se_native=se_nat, p_wald=p_wald,
         b2_ols_scale=b2_nat*mu_bar, se_ols_scale=se_nat*mu_bar)
}

OUTCOMES <- c("n_same_section","n_same_department","n_different_department",
              "n_outside_own_ka","n_total_internal")
LABELS   <- c("Same ka","Other ka same kyoku","Other kyoku",
              "Outside own ka","Total internal")

RHS1  <- "n_drafted_male + n_drafted_male:log_S_i + log_S_i + log_P_sameKyoku + log_P_otherKyoku"
B2_1  <- "n_drafted_male:log_S_i"
RHS3  <- "n_drafted_male + n_drafted_male:log_S_pos + log_S_pos + log_P_sameKyoku_pos + log_P_otherKyoku_pos"
B2_3  <- "n_drafted_male:log_S_pos"
RHS4  <- "D_S0 + D_logS1 + S_zero + log_S_nonzero + log_P_sameKyoku + log_P_otherKyoku"

# ============================================================
# 5.  RUN SPECS
# ============================================================
cat("\nCol 1 (OLS, ka cluster)...\n")
col1 <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v7,
                rhs=RHS1, b_term=B2_1, cluster_var="ka_id_int") %>%
  mutate(spec="col1")

cat("Col 2 (PPML, ka cluster)...\n")
ppml_diag <- map_dfr(OUTCOMES, run_ppml_diag, data=panel_v7,
                     rhs=RHS1, b_term=B2_1)
write_csv(ppml_diag, file.path(out_dir,"v7_ppml_diag.csv"))
cat("PPML diagnostics:\n")
print(ppml_diag %>% select(outcome,N_ppml,N_dropped,converged,n_iter,b2_native,b2_ols_scale))

col2 <- ppml_diag %>%
  transmute(outcome, spec="col2", term=B2_1,
            b=b2_ols_scale, se=se_ols_scale,
            p_feols=p_wald, p_boot=NA_real_,
            n_cells=N_ppml, n_clusters=n_ka_clusters)

cat("Col 3 (OLS pos_norm pool, ka cluster)...\n")
col3 <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v7_3,
                rhs=RHS3, b_term=B2_3, cluster_var="ka_id_int") %>%
  mutate(spec="col3")

cat("Col 4 (OLS spline, ka cluster)...\n")
col4_slope <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v7,
                      rhs=RHS4, b_term="D_logS1", cluster_var="ka_id_int") %>%
  mutate(spec="col4_slope")
col4_level <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v7,
                      rhs=RHS4, b_term="D_S0", cluster_var="ka_id_int") %>%
  mutate(spec="col4_level")

cat("Col 5 (OLS, kyoku cluster)...\n")
col5 <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v7,
                rhs=RHS1, b_term=B2_1, cluster_var="kyoku_cl_int") %>%
  mutate(spec="col5")

raw_v7 <- bind_rows(col1, col2, col3, col4_slope, col4_level, col5)
write_csv(raw_v7, file.path(out_dir,"v7_main_raw.csv"))

# ============================================================
# 6.  FORMAT TABLE
# ============================================================
fmt  <- function(x, d=3) ifelse(is.na(x),"",sprintf(paste0("%.",d,"f"),x))
strs <- function(p) case_when(is.na(p)~"", p<0.01~"***", p<0.05~"**", p<0.10~"*", TRUE~"")
p_star <- function(r, col) {
  if (col=="col2") coalesce(r$p_feols, NA_real_) else coalesce(r$p_boot, r$p_feols)
}

COL_ORDER  <- c("col1","col2","col3","col4_slope","col5")
COL_LABELS <- c("(1) OLS, ka", "(2) PPML, ka\\textsuperscript{a}",
                "(3) OLS pos\\_norm, ka", "(4) OLS spline, ka", "(5) OLS, kyoku")

make_rows <- function(spec_vec, term_suffix="") {
  rows <- list()
  for (i in seq_along(OUTCOMES)) {
    oc  <- OUTCOMES[i]; lbl <- LABELS[i]
    b_cells  <- map_chr(spec_vec, function(sp) {
      r <- raw_v7 %>% filter(outcome==oc, spec==sp)
      if (!nrow(r)||is.na(r$b)) return("")
      p <- p_star(r, sp)
      paste0(fmt(r$b), strs(p))
    })
    se_cells <- map_chr(spec_vec, function(sp) {
      r <- raw_v7 %>% filter(outcome==oc, spec==sp)
      if (!nrow(r)||is.na(r$se)) return("")
      paste0("(", fmt(r$se), ")")
    })
    rows[[length(rows)+1]] <- c(lbl, b_cells)
    rows[[length(rows)+1]] <- c("",  se_cells)
  }
  rows
}

rows_A <- make_rows(COL_ORDER)
rows_B <- make_rows(c("","","","col4_level",""))   # only col4 populated

# Footer
N_row <- c("Cells", map_chr(COL_ORDER, function(sp) {
  r <- raw_v7 %>% filter(spec==sp, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_cells)) "" else as.character(r$n_cells) }))
G_row <- c("Clusters", map_chr(COL_ORDER, function(sp) {
  r <- raw_v7 %>% filter(spec==sp, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_clusters)) "" else as.character(r$n_clusters) }))

all_rows <- c(rows_A, rows_B,
              list(N_row), list(G_row))

max_len  <- max(sapply(all_rows, length))
all_rows <- lapply(all_rows, function(r){ length(r)<-max_len; replace(r,is.na(r),"") })
tbl      <- as.data.frame(do.call(rbind,all_rows), stringsAsFactors=FALSE)
colnames(tbl) <- c("Outcome", COL_LABELS)
write_csv(tbl, file.path(out_dir,"v7_main.csv"))

# LaTeX
nc  <- length(COL_ORDER)
cs  <- paste0("l", paste(rep("r",nc), collapse=""))
hdr <- paste(c("", COL_LABELS), collapse=" & ")
panel_A_end <- 2 * length(OUTCOMES)   # row index of last Panel A row
panel_B_end <- panel_A_end + 2*length(OUTCOMES)

tex <- c(
  "\\begin{table}[!htbp]","  \\centering",
  "  \\caption{$\\hat{b}_2$ across specifications (1944, former-Shi, v7 --- 1942 pool)}",
  "  \\label{tab:v7_main}",
  paste0("  \\begin{tabular}{",cs,"}"),
  "    \\hline\\hline",
  paste0("    ",hdr," \\\\"),
  "    \\midrule",
  "    \\multicolumn{6}{l}{\\textit{Panel A: $D_i \\times \\log(S_i{+}1)$ [cols 1--3, 5]; slope $D_i \\times \\log(S_i{+}1) \\times 1[S_i{>}0]$ [col 4]}} \\\\",
  "    \\addlinespace"
)

for (i in seq_len(nrow(tbl))) {
  rs <- paste(as.character(tbl[i,]), collapse=" & ")
  if (i == panel_A_end + 1) {
    tex <- c(tex,
      "    \\midrule",
      "    \\multicolumn{6}{l}{\\textit{Panel B: col.\\ (4) $D_i \\times 1[S_i{=}0]$}} \\\\",
      "    \\addlinespace")
  }
  if (tbl[[1]][i]=="Cells") tex <- c(tex,"    \\midrule")
  tex <- c(tex, paste0("    ",rs," \\\\"))
}

tex <- c(tex,
  "    \\hline\\hline",
  "  \\end{tabular}",
  "  \\\\[4pt]",
  "  \\footnotesize",
  "  FE: kyoku$\\times$ka and pos\\_norm. Pool $S_i$: linked men (1942$\\cap$1944) by \\textbf{1942} ka group, rank $r$ or $r{-}1$;",
  "  cell exclusion = men whose 1942 ka matches cell$_i$ and who land in cell$_i$ in 1944.",
  "  Col.\\ (2) shows $\\bar{\\mu}\\times\\hat{b}_2^{\\text{PPML}}$ (delta-method SE); native coef in \\texttt{v7\\_ppml\\_diag.csv}.",
  "  Col.\\ (3) pool: same pos\\_norm $\\times$ 1942 ka group.",
  "  Stars: (1)(3)(4)(5) Webb bootstrap $p$ ($B{=}9{,}999$); (2) cluster-robust Wald $p$.",
  "  (* 0.10, ** 0.05, *** 0.01). Clusters: (1)--(4) ka; (5) kyoku.",
  "\\end{table}")
writeLines(tex, file.path(out_dir,"v7_main.tex"), useBytes=TRUE)

cat("\n=== V7 MAIN TABLE ===\n")
print(tbl, row.names=FALSE)
cat("\nAll files written to:", out_dir, "\n")
cat("Done.\n")
