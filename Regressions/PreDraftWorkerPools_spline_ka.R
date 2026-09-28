################################################################################
# PreDraftWorkerPools_spline_ka.R
#
# (A) Spline Panel A: D×1[S=0] + D×log(S+1)×1[S>0] with main effects
#     All five outcomes. Ka cluster, Webb boottest (B=9,999).
#     Uses v6 baseline sample, FE, and pool controls (1944-ka pool).
#
# (B) Ka-assignment year check:
#     S currently built from men44_linked — 1944 ka_group.
#     Rebuild S from men42_linked — 1942 ka_group.
#     Re-run cols 1 (OLS, kyoku) and 4 (OLS, ka) with both pools.
#     No cell-exclusion for 1942 pool (1944 cell definitions don't
#     map cleanly to 1942 offices).
#
# Outputs:
#   tables/spline_ka_raw.csv         long raw results (A)
#   tables/spline_ka.csv/.tex        formatted table (A)
#   tables/ka_year_check_raw.csv     long raw: cols 1+4, 1944 vs 1942 pool (B)
#   tables/ka_year_check.csv/.tex    formatted comparison table (B)
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
# 2.  OUTCOME PANEL  (same as v6)
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
    is.na(lag_kyoku_group) | is.na(kyoku_group)               ~ NA_character_,
    !is.na(ka_group) & !is.na(lag_ka_group) & ka_group==lag_ka_group  ~ "same_section",
    (is.na(ka_group)|is.na(lag_ka_group)) & lag_kyoku_group==kyoku_group &
      !is.na(lag_ka)&!is.na(ka)&lag_ka==ka                    ~ "same_section",
    lag_kyoku_group == kyoku_group                             ~ "same_department",
    lag_kyoku_group != kyoku_group                             ~ "different_department",
    TRUE                                                       ~ NA_character_))

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(n_same_section         = sum(arrival_type=="same_section",na.rm=TRUE),
            n_same_department      = sum(arrival_type=="same_department",na.rm=TRUE),
            n_different_department = sum(arrival_type=="different_department",na.rm=TRUE),
            .groups="drop") %>%
  mutate(n_total_internal = n_same_section+n_same_department+n_different_department,
         n_outside_own_ka = n_same_department + n_different_department)

position_drafts <- df_all %>% filter(year_num==1944, drafted==TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted_male=sum(!is_female,na.rm=TRUE), .groups="drop")

panel <- position_outcomes %>%
  left_join(position_drafts, by=c("office_id","kakari","pos_norm","year_num")) %>%
  mutate(n_drafted_male=replace_na(n_drafted_male,0L),
         ka_id         = if_else(!is.na(ka)&!is.na(kyoku),paste(kyoku,ka,sep="_"),NA_character_),
         is_former_shi = office_id %in% shi_offices,
         kyoku_cl      = normalize_kyoku(kyoku)) %>%
  filter(!is.na(ka_id))

# ============================================================
# 3.  LINKED MEN — 1944 ka (v4/v6 default) AND 1942 ka (new)
# ============================================================
ids42_male <- df_names %>% filter(year_num==1942, !is_female) %>% pull(staff_id) %>% unique()
linked_ids <- intersect(ids42_male,
  df_names %>% filter(year_num==1944) %>% pull(staff_id) %>% unique())

# — 1944 ka pool (v4/v6 default) —
men44_linked <- df_names %>%
  filter(year_num==1944, staff_id %in% linked_ids, !is_female) %>%
  mutate(position_rank=rank_pos(pos_norm)) %>% filter(!is.na(position_rank))
cat("Linked men 1944 (defined rank):", nrow(men44_linked), "\n")
cat("S pool year: 1944 ka_group (confirmed — men44_linked filtered at year_num==1944)\n")

# — 1942 ka pool (alternative) —
men42_linked <- df_names %>%
  filter(year_num==1942, staff_id %in% linked_ids, !is_female) %>%
  mutate(position_rank=rank_pos(pos_norm)) %>% filter(!is.na(position_rank))
cat("Linked men 1942 (defined rank):", nrow(men42_linked), "\n")
cat("Note: 1942 pool uses no cell exclusion (1944 cell IDs absent in 1942 data)\n")

pool_lookup <- function(df, group_col, target_ranks=1:3) {
  df %>% crossing(target_rank=target_ranks) %>%
    filter(position_rank >= pmax(1L, target_rank-1L), position_rank <= target_rank) %>%
    group_by(across(all_of(c(group_col,"target_rank")))) %>%
    summarise(pool_n=sum(n,na.rm=TRUE), .groups="drop")
}

make_pools <- function(men_linked) {
  ka_rt    <- men_linked %>% filter(!is.na(ka_group))  %>% count(ka_group,    position_rank, name="n")
  ky_rt    <- men_linked %>% filter(!is.na(kyoku_group)) %>% count(kyoku_group, position_rank, name="n")
  all_rt   <- men_linked %>% count(position_rank, name="n")
  list(ka    = pool_lookup(ka_rt,  "ka_group"),
       kyoku = pool_lookup(ky_rt,  "kyoku_group"),
       all   = pool_lookup(all_rt, character(0)) %>% rename(all_n=pool_n))
}

pools44 <- make_pools(men44_linked)
pools42 <- make_pools(men42_linked)

cell_linked_cnt <- men44_linked %>% filter(!is.na(ka_group)) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(n_linked_in_cell=n(), .groups="drop")

build_panel <- function(panel_in, pools, subtract_cell = TRUE) {
  p <- panel_in %>% filter(is_former_shi) %>%
    mutate(cell_rank=rank_pos(pos_norm), kyoku_clean=replace_na(kyoku,""),
           ka_clean=replace_na(ka,"")) %>%
    filter(!is.na(cell_rank)) %>%
    left_join(ka_group_raw %>% filter(year==1944) %>%
                select(kyoku, ka, cell_ka_group=ka_group),
              by=c("kyoku_clean"="kyoku","ka_clean"="ka")) %>%
    mutate(cell_kyoku_group=assign_kyoku_group(normalize_kyoku(kyoku))) %>%
    left_join(pools$ka    %>% rename(cell_ka_group=ka_group,    S_i_total=pool_n),
              by=c("cell_ka_group","cell_rank"="target_rank")) %>%
    left_join(pools$kyoku %>% rename(cell_kyoku_group=kyoku_group, kyoku_total=pool_n),
              by=c("cell_kyoku_group","cell_rank"="target_rank")) %>%
    left_join(pools$all,  by=c("cell_rank"="target_rank"))
  if (subtract_cell) {
    p <- p %>%
      left_join(cell_linked_cnt, by=c("office_id","kakari","pos_norm")) %>%
      mutate(S_i=pmax(0L, replace_na(S_i_total,0L)-replace_na(n_linked_in_cell,0L)))
  } else {
    p <- p %>% mutate(S_i=pmax(0L, replace_na(S_i_total,0L)))
  }
  p %>% mutate(
    P_sameKyoku  = pmax(0L, replace_na(kyoku_total,0L)-replace_na(S_i_total,0L)),
    P_otherKyoku = pmax(0L, replace_na(all_n,0L)-replace_na(kyoku_total,0L)),
    log_S_i          = log(S_i+1),
    log_P_sameKyoku  = log(P_sameKyoku+1),
    log_P_otherKyoku = log(P_otherKyoku+1),
    # Spline terms
    S_zero       = as.integer(S_i == 0),
    log_S_nonzero= if_else(S_i > 0, log(S_i+1), 0),
    D_S0         = n_drafted_male * S_zero,
    D_logS1      = n_drafted_male * log_S_nonzero)
}

panel_44 <- build_panel(panel, pools44, subtract_cell=TRUE)
panel_42 <- build_panel(panel, pools42, subtract_cell=FALSE)
cat("V4 panel (1944 ka):", nrow(panel_44), "cells\n")
cat("Panel (1942 ka):", nrow(panel_42), "cells\n")

cat("\nPool summary — 1944 ka:\n")
cat("  mean S_i:", round(mean(panel_44$S_i, na.rm=TRUE),2),
    "  pct S_i=0:", round(100*mean(panel_44$S_i==0, na.rm=TRUE),1), "%\n")
cat("Pool summary — 1942 ka:\n")
cat("  mean S_i:", round(mean(panel_42$S_i, na.rm=TRUE),2),
    "  pct S_i=0:", round(100*mean(panel_42$S_i==0, na.rm=TRUE),1), "%\n")

# ============================================================
# 4.  HELPERS
# ============================================================
remove_fe_singletons <- function(data, fes) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      counts <- table(data[[fe]])
      data   <- data[data[[fe]] %in% names(counts[counts>1]), ]
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

run_spec <- function(y, data, rhs, b_terms, cluster_var) {
  d  <- remove_fe_singletons(add_int_ids(data), FES)
  cl <- as.formula(paste0("~", cluster_var))
  f  <- as.formula(paste0(y, " ~ ", rhs, " | ka_id_int + pos_norm_int"))
  m  <- tryCatch(feols(f, data=d, cluster=cl),
                 error=function(e){ cat("  feols error [",y,"]:",conditionMessage(e),"\n"); NULL })
  if (is.null(m)) return(NULL)
  G <- n_distinct(d[[cluster_var]])
  map_dfr(b_terms, function(bt) {
    r  <- get_coef_row(m, bt)
    if (!nrow(r)) {
      return(tibble(outcome=y, term=bt, b=NA_real_, se=NA_real_,
                    p_feols=NA_real_, p_boot=NA_real_, n_cells=nobs(m), n_clusters=G))
    }
    boot <- tryCatch(boottest(m, param=bt, B=9999L, type="webb", clustid=cl),
                     error=function(e) list(p_val=NA_real_))
    tibble(outcome=y, term=bt,
           b=r$estimate, se=r$std_error, p_feols=r$p_feols,
           p_boot=boot$p_val, n_cells=nobs(m), n_clusters=G)
  })
}

OUTCOMES <- c("n_same_section","n_same_department","n_different_department",
              "n_outside_own_ka","n_total_internal")
LABELS   <- c("Same ka","Other ka same kyoku","Other kyoku",
              "Outside own ka","Total internal")

# ============================================================
# 5.  (A) SPLINE PANEL A — ka cluster
# ============================================================
cat("\n=== SPLINE PANEL A (ka cluster) ===\n")

RHS_SPLINE <- "D_S0 + D_logS1 + S_zero + log_S_nonzero + log_P_sameKyoku + log_P_otherKyoku"
B_SPLINE   <- c("D_S0","D_logS1")

spline_raw <- map_dfr(OUTCOMES, run_spec,
                      data=panel_44, rhs=RHS_SPLINE,
                      b_terms=B_SPLINE, cluster_var="ka_id_int") %>%
  mutate(spec="spline_ka44")

write_csv(spline_raw, file.path(out_dir, "spline_ka_raw.csv"))
cat("Spline raw saved.\n")
print(spline_raw %>% select(outcome, term, b, se, p_feols, p_boot))

# Format table: rows = outcome (coef + SE), cols = [D_S0 coef SE p | D_logS1 coef SE p]
fmt  <- function(x, d=3) ifelse(is.na(x),"",sprintf(paste0("%.",d,"f"),x))
strs <- function(p) case_when(is.na(p)~"", p<0.01~"***", p<0.05~"**", p<0.10~"*", TRUE~"")

rows <- list()
for (i in seq_along(OUTCOMES)) {
  oc <- OUTCOMES[i]; lbl <- LABELS[i]
  r0 <- spline_raw %>% filter(outcome==oc, term=="D_S0")
  r1 <- spline_raw %>% filter(outcome==oc, term=="D_logS1")
  p0 <- coalesce(r0$p_boot, r0$p_feols)
  p1 <- coalesce(r1$p_boot, r1$p_feols)
  rows[[length(rows)+1]] <- c(lbl,
    paste0(fmt(r0$b), strs(p0)), fmt(r0$se), fmt(p0),
    paste0(fmt(r1$b), strs(p1)), fmt(r1$se), fmt(p1))
  rows[[length(rows)+1]] <- c("",
    paste0("(", fmt(r0$se), ")"), "", "",
    paste0("(", fmt(r1$se), ")"), "", "")
}
N_row <- c("Cells",
  as.character(spline_raw %>% filter(outcome=="n_total_internal",term=="D_S0") %>% pull(n_cells)),
  "","","","","")
G_row <- c("Clusters",
  as.character(spline_raw %>% filter(outcome=="n_total_internal",term=="D_S0") %>% pull(n_clusters)),
  "","","","","")
rows[[length(rows)+1]] <- N_row
rows[[length(rows)+1]] <- G_row

max_len <- max(sapply(rows, length))
rows    <- lapply(rows, function(r){ length(r)<-max_len; replace(r,is.na(r),"") })
tbl     <- as.data.frame(do.call(rbind,rows), stringsAsFactors=FALSE)
COL_NAMES <- c("Outcome",
               "D×1[S=0]", "SE", "p",
               "D×log(S+1)×1[S>0]", "SE", "p")
colnames(tbl) <- COL_NAMES
write_csv(tbl, file.path(out_dir,"spline_ka.csv"))

# LaTeX
cs  <- paste0("l", paste(rep("r",ncol(tbl)-1), collapse=""))
hdr <- paste(COL_NAMES, collapse=" & ")
tex <- c(
  "\\begin{table}[!htbp]","  \\centering",
  "  \\caption{Spline Panel A: $D \\times 1[S=0]$ and $D \\times \\log(S+1) \\times 1[S>0]$ (1944, former-Shi, ka cluster)}",
  "  \\label{tab:spline_ka}",
  paste0("  \\begin{tabular}{",cs,"}"),
  "    \\hline\\hline",
  paste0("    ",hdr," \\\\"),
  "    \\hline")
for (i in seq_len(nrow(tbl))) {
  rs <- paste(as.character(tbl[i,]), collapse=" & ")
  if (tbl[[1]][i]=="Cells") tex <- c(tex,"    \\hline")
  tex <- c(tex, paste0("    ",rs," \\\\"))
}
tex <- c(tex,
  "    \\hline\\hline","  \\end{tabular}","  \\\\[4pt]","  \\footnotesize",
  "  FE: ka\\,id $\\times$ pos\\_norm. Pool controls: $\\log(P_{\\text{sameKyoku}}+1)$, $\\log(P_{\\text{otherKyoku}}+1)$.",
  "  $S$ pool: linked men (1942 \\& 1944) in same 1944 ka, same rank $\\pm 1$, excluding own cell.",
  "  Stars from Webb boottest $p$ (ka cluster, $G \\approx 97$, $B=9{,}999$). (* 0.10, ** 0.05, *** 0.01).",
  "\\end{table}")
writeLines(tex, file.path(out_dir,"spline_ka.tex"), useBytes=TRUE)

# ============================================================
# 6.  (B) KA-ASSIGNMENT YEAR CHECK — cols 1 and 4, linear spec
# ============================================================
cat("\n=== KA-ASSIGNMENT YEAR CHECK (cols 1 and 4) ===\n")

RHS_LINEAR <- "n_drafted_male + n_drafted_male:log_S_i + log_S_i + log_P_sameKyoku + log_P_otherKyoku"
B2_TERM    <- "n_drafted_male:log_S_i"

cat("Col 1 (OLS, kyoku), 1944-ka pool...\n")
c1_44 <- map_dfr(OUTCOMES, run_spec, data=panel_44, rhs=RHS_LINEAR,
                 b_terms=B2_TERM, cluster_var="kyoku_cl_int") %>%
  mutate(spec="col1_ka44")

cat("Col 1 (OLS, kyoku), 1942-ka pool...\n")
c1_42 <- map_dfr(OUTCOMES, run_spec, data=panel_42, rhs=RHS_LINEAR,
                 b_terms=B2_TERM, cluster_var="kyoku_cl_int") %>%
  mutate(spec="col1_ka42")

cat("Col 4 (OLS, ka), 1944-ka pool...\n")
c4_44 <- map_dfr(OUTCOMES, run_spec, data=panel_44, rhs=RHS_LINEAR,
                 b_terms=B2_TERM, cluster_var="ka_id_int") %>%
  mutate(spec="col4_ka44")

cat("Col 4 (OLS, ka), 1942-ka pool...\n")
c4_42 <- map_dfr(OUTCOMES, run_spec, data=panel_42, rhs=RHS_LINEAR,
                 b_terms=B2_TERM, cluster_var="ka_id_int") %>%
  mutate(spec="col4_ka42")

ka_check_raw <- bind_rows(c1_44, c1_42, c4_44, c4_42)
write_csv(ka_check_raw, file.path(out_dir,"ka_year_check_raw.csv"))

# Format: rows = outcome, cols = [col1 1944ka b SE p | col1 1942ka b SE p |
#                                  col4 1944ka b SE p | col4 1942ka b SE p]
SPEC_MAP <- list(
  list(spec="col1_ka44", label="(1) OLS kyoku, S=1944ka"),
  list(spec="col1_ka42", label="(1) OLS kyoku, S=1942ka"),
  list(spec="col4_ka44", label="(4) OLS ka, S=1944ka"),
  list(spec="col4_ka42", label="(4) OLS ka, S=1942ka"))

rows2 <- list()
for (i in seq_along(OUTCOMES)) {
  oc <- OUTCOMES[i]; lbl <- LABELS[i]
  b_cells  <- map_chr(SPEC_MAP, function(sm) {
    r <- ka_check_raw %>% filter(outcome==oc, spec==sm$spec)
    if (!nrow(r)) return("")
    p_star <- coalesce(r$p_boot, r$p_feols)
    paste0(fmt(r$b), strs(p_star))
  })
  se_cells <- map_chr(SPEC_MAP, function(sm) {
    r <- ka_check_raw %>% filter(outcome==oc, spec==sm$spec)
    if (!nrow(r)) return("") else paste0("(", fmt(r$se), ")")
  })
  p_cells  <- map_chr(SPEC_MAP, function(sm) {
    r <- ka_check_raw %>% filter(outcome==oc, spec==sm$spec)
    if (!nrow(r)) return("") else fmt(coalesce(r$p_boot, r$p_feols))
  })
  rows2[[length(rows2)+1]] <- c(lbl, b_cells)
  rows2[[length(rows2)+1]] <- c("",  se_cells)
}
N_row2 <- c("Cells", map_chr(SPEC_MAP, function(sm) {
  r <- ka_check_raw %>% filter(spec==sm$spec, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_cells)) "" else as.character(r$n_cells) }))
G_row2 <- c("Clusters", map_chr(SPEC_MAP, function(sm) {
  r <- ka_check_raw %>% filter(spec==sm$spec, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_clusters)) "" else as.character(r$n_clusters) }))
rows2[[length(rows2)+1]] <- N_row2
rows2[[length(rows2)+1]] <- G_row2

max_len2 <- max(sapply(rows2, length))
rows2    <- lapply(rows2, function(r){ length(r)<-max_len2; replace(r,is.na(r),"") })
tbl2     <- as.data.frame(do.call(rbind,rows2), stringsAsFactors=FALSE)
COL2     <- c("Outcome", map_chr(SPEC_MAP, ~.x$label))
colnames(tbl2) <- COL2
write_csv(tbl2, file.path(out_dir,"ka_year_check.csv"))

cs2  <- paste0("l", paste(rep("r",length(SPEC_MAP)), collapse=""))
hdr2 <- paste(COL2, collapse=" & ")
tex2 <- c(
  "\\begin{table}[!htbp]","  \\centering",
  "  \\caption{Ka-assignment year check: $\\hat{b}_2$ with 1944 vs.\\ 1942 $S_i$ pool (cols 1 \\& 4)}",
  "  \\label{tab:ka_year_check}",
  paste0("  \\begin{tabular}{",cs2,"}"),
  "    \\hline\\hline",
  paste0("    ",hdr2," \\\\"),
  "    \\hline",
  "    & \\multicolumn{2}{c}{Col.\\ 1 (kyoku cluster)} & \\multicolumn{2}{c}{Col.\\ 4 (ka cluster)} \\\\",
  "    \\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "    & S=1944\\,ka & S=1942\\,ka & S=1944\\,ka & S=1942\\,ka \\\\"
)
for (i in seq_len(nrow(tbl2))) {
  rs <- paste(as.character(tbl2[i,]), collapse=" & ")
  if (tbl2[[1]][i]=="Cells") tex2 <- c(tex2,"    \\hline")
  tex2 <- c(tex2, paste0("    ",rs," \\\\"))
}
tex2 <- c(tex2,
  "    \\hline\\hline","  \\end{tabular}","  \\\\[4pt]","  \\footnotesize",
  "  S=1944\\,ka: pool built from linked men at their 1944 ka position (v4/v6 default).",
  "  S=1942\\,ka: pool built from linked men at their 1942 ka position; no cell exclusion.",
  "  Stars from Webb boottest $p$ (B\\,=\\,9{,}999). (* 0.10, ** 0.05, *** 0.01).",
  "\\end{table}")
writeLines(tex2, file.path(out_dir,"ka_year_check.tex"), useBytes=TRUE)

cat("\n=== SUMMARY ===\n")
cat("Pool year: S uses 1944 ka_group (men44_linked at year_num==1944)\n")
cat("1942-ka pool: no cell exclusion applied\n\n")
cat("=== SPLINE TABLE ===\n"); print(tbl, row.names=FALSE)
cat("\n=== KA YEAR CHECK TABLE ===\n"); print(tbl2, row.names=FALSE)
cat("\nAll files written to:", out_dir, "\n")
cat("Done.\n")
