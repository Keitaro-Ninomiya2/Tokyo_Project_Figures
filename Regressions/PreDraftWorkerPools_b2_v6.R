################################################################################
# b2_4specs v6
#
# Col 1: OLS, kyoku cluster (unchanged from v5)
# Col 2: PPML, ka cluster  — main table shows OLS-scale APE derivative
#                             (b2_ppml × mean fitted values); native coef in
#                             appendix table.
# Col 3: OLS, pos_norm pool, ka cluster
# Col 4: OLS, ka cluster (unchanged from v5)
#
# Additional outputs:
#   b2_col3_check_v6.csv      Col 3 per-outcome: b2, SE, p_feols, p_boot, t_boot
#   ppml_diagnostics_v6.csv   N/convergence/APE/OLS-on-same-sample per outcome
#   b2_ppml_native_v6.{csv,tex}  Appendix: native PPML b2
#
# Method note: "v6 changes" section added.
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
# 2.  FORMER-SHI + OUTCOME PANEL  (abbreviated; same as v4/v5)
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
# 3.  POOL VARIABLES  (rank-based v4 pools + spec-3 pos_norm pools)
# ============================================================
ids42_male  <- df_names %>% filter(year_num==1942,!is_female) %>% pull(staff_id) %>% unique()
linked_ids  <- intersect(ids42_male,
  df_names %>% filter(year_num==1944) %>% pull(staff_id) %>% unique())
men44_linked <- df_names %>%
  filter(year_num==1944, staff_id %in% linked_ids, !is_female) %>%
  mutate(position_rank=rank_pos(pos_norm)) %>% filter(!is.na(position_rank))
cat("Linked men (1944, defined rank):", nrow(men44_linked), "\n")

ka_rank_totals    <- men44_linked %>% filter(!is.na(ka_group)) %>%
  count(ka_group, position_rank, name="n")
kyoku_rank_totals <- men44_linked %>% filter(!is.na(kyoku_group)) %>%
  count(kyoku_group, position_rank, name="n")
all_rank_totals   <- men44_linked %>% count(position_rank, name="n")
cell_linked_cnt   <- men44_linked %>% filter(!is.na(ka_group)) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(n_linked_in_cell=n(), .groups="drop")

pool_lookup <- function(df, group_col, target_ranks=1:3) {
  df %>% crossing(target_rank=target_ranks) %>%
    filter(position_rank >= pmax(1L, target_rank-1L), position_rank <= target_rank) %>%
    group_by(across(all_of(c(group_col,"target_rank")))) %>%
    summarise(pool_n=sum(n,na.rm=TRUE), .groups="drop")
}
pool_ka    <- pool_lookup(ka_rank_totals,    "ka_group")
pool_kyoku <- pool_lookup(kyoku_rank_totals, "kyoku_group")
pool_all   <- pool_lookup(all_rank_totals,   character(0)) %>% rename(all_n=pool_n)

build_v4_panel <- function(panel_in) {
  panel_in %>% filter(is_former_shi) %>%
    mutate(cell_rank=rank_pos(pos_norm), kyoku_clean=replace_na(kyoku,""),
           ka_clean=replace_na(ka,"")) %>%
    filter(!is.na(cell_rank)) %>%
    left_join(ka_group_raw %>% filter(year==1944) %>%
                select(kyoku,ka,cell_ka_group=ka_group),
              by=c("kyoku_clean"="kyoku","ka_clean"="ka")) %>%
    mutate(cell_kyoku_group=assign_kyoku_group(normalize_kyoku(kyoku))) %>%
    left_join(pool_ka    %>% rename(cell_ka_group=ka_group,S_i_total=pool_n),
              by=c("cell_ka_group","cell_rank"="target_rank")) %>%
    left_join(cell_linked_cnt, by=c("office_id","kakari","pos_norm")) %>%
    mutate(S_i=pmax(0L,replace_na(S_i_total,0L)-replace_na(n_linked_in_cell,0L))) %>%
    left_join(pool_kyoku %>% rename(cell_kyoku_group=kyoku_group,kyoku_total=pool_n),
              by=c("cell_kyoku_group","cell_rank"="target_rank")) %>%
    mutate(P_sameKyoku=pmax(0L,replace_na(kyoku_total,0L)-replace_na(S_i_total,0L))) %>%
    left_join(pool_all, by=c("cell_rank"="target_rank")) %>%
    mutate(P_otherKyoku=pmax(0L,replace_na(all_n,0L)-replace_na(kyoku_total,0L)),
           log_S_i=log(S_i+1),
           log_P_sameKyoku=log(P_sameKyoku+1),
           log_P_otherKyoku=log(P_otherKyoku+1))
}
panel_v4 <- build_v4_panel(panel)
cat("V4 panel:", nrow(panel_v4), "cells\n")

# Spec-3 pos_norm pools
ka_posn    <- men44_linked %>% filter(!is.na(ka_group)) %>%
  count(ka_group, pos_norm, name="n")
kyoku_posn <- men44_linked %>% filter(!is.na(kyoku_group)) %>%
  count(kyoku_group, pos_norm, name="n")
all_posn   <- men44_linked %>% count(pos_norm, name="n")

panel_spec3 <- panel_v4 %>%
  left_join(ka_posn    %>% rename(cell_ka_group=ka_group,S_pos_total=n),
            by=c("cell_ka_group","pos_norm")) %>%
  left_join(cell_linked_cnt, by=c("office_id","kakari","pos_norm"), suffix=c("","_2")) %>%
  mutate(S_pos=pmax(0L,replace_na(S_pos_total,0L)-replace_na(n_linked_in_cell,0L))) %>%
  left_join(kyoku_posn %>% rename(cell_kyoku_group=kyoku_group,kyoku_pos=n),
            by=c("cell_kyoku_group","pos_norm")) %>%
  mutate(P_sameKyoku_pos=pmax(0L,replace_na(kyoku_pos,0L)-replace_na(S_pos_total,0L))) %>%
  left_join(all_posn %>% rename(all_pos=n), by="pos_norm") %>%
  mutate(P_otherKyoku_pos=pmax(0L,replace_na(all_pos,0L)-replace_na(kyoku_pos,0L)),
         log_S_pos           =log(S_pos+1),
         log_P_sameKyoku_pos =log(P_sameKyoku_pos+1),
         log_P_otherKyoku_pos=log(P_otherKyoku_pos+1))

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

remove_ppml_zero_fes <- function(data, fes, y_var) {
  repeat {
    n0 <- nrow(data)
    for (fe in fes) {
      grp <- tapply(data[[y_var]], data[[fe]], sum, na.rm=TRUE)
      data <- data[data[[fe]] %in% names(grp[grp>0]), ]
    }
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

get_b2_row <- function(m, b2_term) {
  ct <- as.data.frame(coeftable(m)) %>%
    tibble::rownames_to_column("term") %>%
    rename_with(~"p_feols", any_of(c("Pr(>|t|)","Pr(>|z|)")))
  ct %>% filter(term==b2_term) %>%
    select(term, estimate=Estimate, std_error=`Std. Error`, p_feols)
}

# ============================================================
# 5.  COL 1 (OLS, kyoku) and COL 4 (OLS, ka) — identical to v5
# ============================================================
RHS_V4 <- paste("n_drafted_male + n_drafted_male:log_S_i + log_S_i",
                "+ log_P_sameKyoku + log_P_otherKyoku")
RHS_S3 <- paste("n_drafted_male + n_drafted_male:log_S_pos + log_S_pos",
                "+ log_P_sameKyoku_pos + log_P_otherKyoku_pos")
B2_V4  <- "n_drafted_male:log_S_i"
B2_S3  <- "n_drafted_male:log_S_pos"

OUTCOMES <- c("n_same_section","n_same_department","n_different_department",
              "n_outside_own_ka","n_total_internal")
LABELS   <- c("Same ka","Other ka same kyoku","Other kyoku",
              "Outside own ka","Total internal")
FES      <- c("ka_id_int","pos_norm_int")

run_ols_boot <- function(y, data, rhs, b2_term, cluster_var) {
  d  <- remove_fe_singletons(add_int_ids(data), FES)
  cl <- as.formula(paste0("~",cluster_var))
  f  <- as.formula(paste0(y," ~ ",rhs," | ka_id_int + pos_norm_int"))
  m  <- tryCatch(feols(f,data=d,cluster=cl), error=function(e){
    cat("  OLS error [",y,"]:",conditionMessage(e),"\n"); NULL})
  if (is.null(m)) return(NULL)
  r  <- get_b2_row(m, b2_term)
  bt <- tryCatch(boottest(m,param=b2_term,B=9999L,type="webb",clustid=cl),
                 error=function(e) list(p_val=NA_real_, t_stat=NA_real_))
  tibble(outcome=y, b2=r$estimate, se=r$std_error,
         p_feols=r$p_feols, p_boot=bt$p_val, t_boot=bt$t_stat,
         n_cells=nobs(m), n_clusters=n_distinct(d[[cluster_var]]))
}

cat("\nCol 1 (OLS, kyoku)...\n")
col1 <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v4, rhs=RHS_V4,
                b2_term=B2_V4, cluster_var="kyoku_cl_int") %>% mutate(spec="col1")

cat("Col 4 (OLS, ka)...\n")
col4 <- map_dfr(OUTCOMES, run_ols_boot, data=panel_v4, rhs=RHS_V4,
                b2_term=B2_V4, cluster_var="ka_id_int") %>% mutate(spec="col4")

# ============================================================
# 6.  COL 2 (PPML, ka cluster) — with full diagnostics
# ============================================================
cat("\nCol 2 (PPML, ka cluster) + diagnostics...\n")

run_ppml_diag <- function(y, data, rhs, b2_term) {
  d_base <- remove_fe_singletons(add_int_ids(data), FES)
  d_ppml <- remove_ppml_zero_fes(d_base, FES, y)
  N_base    <- nrow(d_base); N_ppml <- nrow(d_ppml)

  f <- as.formula(paste0(y," ~ ",rhs," | ka_id_int + pos_norm_int"))

  m_ppml <- tryCatch(fepois(f, data=d_ppml, cluster=~ka_id_int),
                     error=function(e){cat("  fepois error [",y,"]:",conditionMessage(e),"\n"); NULL})
  if (is.null(m_ppml)) return(NULL)

  # Convergence — fepois uses $convStatus (logical) and $iterations (int)
  n_iter <- {
    val <- tryCatch(m_ppml$iterations, error = function(e) NULL)
    if (is.null(val)) NA_integer_ else as.integer(val)
  }
  converged <- {
    val <- tryCatch(m_ppml$convStatus, error = function(e) NULL)
    if (is.null(val)) NA else isTRUE(val)
  }
  n_ka_clusters <- n_distinct(d_ppml[["ka_id_int"]])

  r_ppml <- get_b2_row(m_ppml, b2_term)
  b2_nat <- r_ppml$estimate
  se_nat <- r_ppml$std_error
  p_wald <- r_ppml$p_feols

  # APE derivative: b2_ppml × mean(fitted values)
  mu_bar   <- mean(fitted(m_ppml), na.rm=TRUE)
  b2_ols   <- b2_nat * mu_bar
  se_ols   <- se_nat * mu_bar    # delta method, treating mu_bar as fixed

  # OLS on same (PPML-filtered) sample
  m_ols_s <- tryCatch(feols(f, data=d_ppml, cluster=~ka_id_int), error=function(e) NULL)
  r_ols_s  <- if (!is.null(m_ols_s)) get_b2_row(m_ols_s, b2_term) else
    tibble(estimate=NA_real_, std_error=NA_real_, p_feols=NA_real_)

  tibble(outcome=y, N_base=N_base, N_ppml=N_ppml, N_dropped=N_base-N_ppml,
         converged=converged, n_iter=n_iter, n_ka_clusters=n_ka_clusters,
         mean_y_ppml_sample = mean(d_ppml[[y]], na.rm=TRUE),
         mu_bar=mu_bar,
         b2_native=b2_nat, se_native=se_nat, p_wald=p_wald,
         b2_ols_scale=b2_ols, se_ols_scale=se_ols,
         b2_ols_on_ppml=r_ols_s$estimate,
         se_ols_on_ppml=r_ols_s$std_error,
         p_ols_on_ppml=r_ols_s$p_feols)
}

ppml_diag <- map_dfr(OUTCOMES, run_ppml_diag, data=panel_v4,
                     rhs=RHS_V4, b2_term=B2_V4)
write_csv(ppml_diag, file.path(out_dir, "ppml_diagnostics_v6.csv"))
cat("PPML diagnostics saved.\n"); print(ppml_diag %>% select(outcome,N_ppml,N_dropped,converged,n_iter,b2_native,b2_ols_scale,b2_ols_on_ppml))

col2 <- ppml_diag %>%
  transmute(outcome, spec="col2",
            b2=b2_ols_scale, se=se_ols_scale,
            p_feols=p_wald, p_boot=NA_real_, t_boot=NA_real_,
            n_cells=N_ppml, n_clusters=n_ka_clusters)  # ka clusters (G varies by outcome after sep. removal)

# ============================================================
# 7.  COL 3 (OLS pos_norm pool, ka cluster) — with check table
# ============================================================
cat("\nCol 3 (OLS pos_norm pool, ka cluster)...\n")

col3_raw <- map_dfr(OUTCOMES, run_ols_boot, data=panel_spec3,
                    rhs=RHS_S3, b2_term=B2_S3,
                    cluster_var="ka_id_int") %>% mutate(spec="col3")

# Check table: flag |p_feols - p_boot| > 0.10
col3_check <- col3_raw %>%
  mutate(
    diff_p     = round(abs(p_feols - p_boot), 4),
    flag       = diff_p > 0.10,
    note = case_when(
      is.na(p_boot)  ~ "boottest returned NA",
      flag           ~ paste0("p_feols=", round(p_feols,3),
                              " p_boot=", round(p_boot,3),
                              " diff=", round(diff_p,3),
                              "; G=97 ka clusters — large G, WCR should track t closely; check if t_boot differs from analytical t"),
      TRUE           ~ "ok"
    )
  ) %>%
  select(outcome, b2, se, t_analytical=t_boot, p_feols, p_boot, diff_p, flag, note)

# Overwrite t_analytical with the actual analytical t-stat from feols
col3_check$t_analytical <- col3_raw$b2 / col3_raw$se

write_csv(col3_check, file.path(out_dir, "b2_col3_check_v6.csv"))
cat("\nCol 3 check:\n")
print(col3_check %>% select(outcome, b2, se, t_analytical, p_feols, p_boot, diff_p, flag, note))

col3 <- col3_raw

# ============================================================
# 8.  COMBINE AND SAVE RAW
# ============================================================
raw_v6 <- bind_rows(col1, col2, col3, col4)
write_csv(raw_v6, file.path(out_dir, "b2_4specs_v6_raw.csv"))

# ============================================================
# 9.  MAIN FORMATTED TABLE (col 2 = OLS-scale APE derivative)
# ============================================================
fmt  <- function(x, d=3) ifelse(is.na(x), "", sprintf(paste0("%.",d,"f"), x))
strs <- function(p) case_when(is.na(p)~"", p<0.01~"***", p<0.05~"**", p<0.10~"*", TRUE~"")

SPEC_ORDER  <- c("col1","col2","col3","col4")
SPEC_LABELS <- c("(1) OLS, kyoku",
                 "(2) PPML, ka (APE deriv.)",
                 "(3) OLS, pos\\_norm pool, ka",
                 "(4) OLS, ka")

rows <- list()
for (i in seq_along(OUTCOMES)) {
  oc  <- OUTCOMES[i]; lbl <- LABELS[i]

  b2_cells <- map_chr(SPEC_ORDER, function(sp) {
    r <- raw_v6 %>% filter(outcome==oc, spec==sp)
    if (!nrow(r)) return("")
    # col1/3/4: stars from p_boot; col2: stars from p_feols (Wald)
    p_star <- if (sp == "col2") r$p_feols else coalesce(r$p_boot, r$p_feols)
    paste0(fmt(r$b2), strs(p_star))
  })
  se_cells <- map_chr(SPEC_ORDER, function(sp) {
    r <- raw_v6 %>% filter(outcome==oc, spec==sp)
    if (!nrow(r)) return("")
    paste0("(", fmt(r$se), ")")
  })
  rows[[length(rows)+1]] <- c(lbl, b2_cells)
  rows[[length(rows)+1]] <- c("",   se_cells)
}

# Footer
N_row <- c("Cells", map_chr(SPEC_ORDER, function(sp) {
  r <- raw_v6 %>% filter(spec==sp, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_cells)) "" else as.character(r$n_cells)
}))
G_row <- c("Clusters", map_chr(SPEC_ORDER, function(sp) {
  r <- raw_v6 %>% filter(spec==sp, outcome=="n_total_internal")
  if (!nrow(r)||is.na(r$n_clusters)) "" else as.character(r$n_clusters)
}))
rows[[length(rows)+1]] <- N_row
rows[[length(rows)+1]] <- G_row

tbl <- as.data.frame(do.call(rbind,rows), stringsAsFactors=FALSE)
colnames(tbl) <- c("Outcome", SPEC_LABELS)
write_csv(tbl, file.path(out_dir, "b2_4specs_v6.csv"))

# LaTeX
nc  <- ncol(tbl)-1
cs  <- paste0("l", paste(rep("r",nc), collapse=""))
hdr <- paste(colnames(tbl), collapse=" & ")
tex <- c(
  "\\begin{table}[!htbp]", "  \\centering",
  "  \\caption{$\\hat{\\beta}_2$ across specifications (1944, former-Shi, v6)}",
  "  \\label{tab:b2_4specs_v6}",
  paste0("  \\begin{tabular}{",cs,"}"),
  "    \\hline\\hline",
  paste0("    ",hdr," \\\\"),
  "    \\hline"
)
for (i in seq_len(nrow(tbl))) {
  row_str <- paste(as.character(tbl[i,]), collapse=" & ")
  if (tbl[[1]][i]=="Cells") tex <- c(tex, "    \\hline")
  tex <- c(tex, paste0("    ",row_str," \\\\"))
}
tex <- c(tex,
  "    \\hline\\hline",
  "  \\end{tabular}",
  "  \\\\[4pt]",
  "  \\footnotesize",
  "  Cols.~(1) and (4): OLS; (3): OLS with occupation-level pool ($S =$ same pos\\_norm $\\times$ ka group).",
  "  Col.~(2) PPML: shows $\\bar{\\mu} \\times \\hat{b}_2^{\\text{PPML}}$ where $\\bar{\\mu} =$ mean fitted outcome;",
  "  native PPML $\\hat{b}_2$ in Appendix Table.",
  "  SE for col.~(2): $\\bar{\\mu} \\times$ SE$(\\hat{b}_2^{\\text{PPML}})$ (delta method, $\\bar{\\mu}$ treated as fixed).",
  "  Stars: (1)(3)(4) Webb bootstrap p (B\\,=\\,9{,}999); (2) cluster-robust Wald p.",
  "  (* 0.10, ** 0.05, *** 0.01).",
  "  (1) kyoku clusters ($G=6$); (2)--(4) ka clusters ($G\\approx 97$).",
  "\\end{table}")
writeLines(tex, file.path(out_dir,"b2_4specs_v6.tex"), useBytes=TRUE)

# ============================================================
# 10.  APPENDIX TABLE — native PPML b2 (rows = outcomes, cols = statistics)
# ============================================================
APP_COLS <- c("Outcome",
              "$\\hat{b}_2^{\\text{PPML}}$",
              "SE",
              "$p$ (Wald)",
              "$\\bar{\\mu}$",
              "OLS-scale",
              "OLS same samp.")

app_rows <- list()
for (i in seq_along(OUTCOMES)) {
  oc  <- OUTCOMES[i]; lbl <- LABELS[i]
  r   <- ppml_diag %>% filter(outcome==oc)
  if (!nrow(r)) next
  p_star <- strs(r$p_wald)
  # coefficient row
  app_rows[[length(app_rows)+1]] <- c(lbl,
    paste0(fmt(r$b2_native), p_star),
    "",
    fmt(r$p_wald),
    fmt(r$mu_bar, 2),
    fmt(r$b2_ols_scale),
    fmt(r$b2_ols_on_ppml))
  # SE row
  app_rows[[length(app_rows)+1]] <- c("",
    paste0("(", fmt(r$se_native), ")"),
    "", "", "", "", "")
}
app_rows[[length(app_rows)+1]] <- c("N (PPML sample)",
  map_chr(OUTCOMES, function(oc) {
    r <- ppml_diag %>% filter(outcome==oc); if (!nrow(r)) "" else as.character(r$N_ppml)
  })[1], "", "", "", "", "")   # N varies by outcome; printed in diag CSV

# Footer: N / dropped / converged / iterations per outcome
footer_stats <- c("N_ppml","N_dropped","converged","n_iter")
footer_labels <- c("N (PPML sample)","N dropped","Converged","Iterations")
for (fi in seq_along(footer_stats)) {
  app_rows[[length(app_rows)+1]] <- c(footer_labels[fi],
    map_chr(OUTCOMES, function(oc) {
      r <- ppml_diag %>% filter(outcome==oc)
      if (!nrow(r)) "" else as.character(r[[footer_stats[fi]]])
    }))
}

# footer rows have 6 elements (label + 5 outcomes); data rows have 7
# Rebuild with consistent 7-col structure for data, 7-col footer using blank extras
app_rows_data <- app_rows[seq_len(2 * length(OUTCOMES))]   # coef + SE rows
app_rows_foot <- app_rows[seq(2*length(OUTCOMES)+1, length(app_rows))]

# Trim to 7 columns: Outcome + 6 stat columns (ignore single-value N row added above)
app_rows_clean <- c(app_rows_data,
  lapply(app_rows_foot[seq_along(footer_stats)], function(r) {
    # r = c(label, val1, val2, val3, val4, val5)  → 6 elements
    # pad to 7 with empty string in col 7
    c(r, "")
  }))

# Collapse to data frame
max_len <- max(sapply(app_rows_clean, length))
app_rows_pad <- lapply(app_rows_clean, function(r) {
  length(r) <- max_len; replace(r, is.na(r), "")
})
app_tbl <- as.data.frame(do.call(rbind, app_rows_pad), stringsAsFactors=FALSE)
colnames(app_tbl) <- APP_COLS
write_csv(app_tbl, file.path(out_dir,"b2_ppml_native_v6.csv"))

app_nc  <- length(APP_COLS) - 1
app_cs  <- paste0("l", paste(rep("r", app_nc), collapse=""))
app_hdr <- paste(APP_COLS, collapse=" & ")
app_tex <- c(
  "\\begin{table}[!htbp]", "  \\centering",
  "  \\caption{PPML: native $\\hat{b}_2$ and diagnostics (appendix)}",
  "  \\label{tab:ppml_native_v6}",
  paste0("  \\begin{tabular}{",app_cs,"}"),
  "    \\hline\\hline",
  paste0("    ",app_hdr," \\\\"),
  "    \\hline"
)
for (i in seq_len(nrow(app_tbl))) {
  rs <- paste(as.character(app_tbl[i,]), collapse=" & ")
  if (app_tbl[[1]][i] %in% footer_labels[1]) app_tex <- c(app_tex,"    \\hline")
  app_tex <- c(app_tex, paste0("    ",rs," \\\\"))
}
app_tex <- c(app_tex,
  "    \\hline\\hline",
  "  \\end{tabular}",
  "  \\\\[4pt]",
  "  \\footnotesize",
  "  $\\hat{b}_2^{\\text{PPML}}$: native coefficient on $D_i \\times \\log(S_i+1)$ in fepois.",
  "  Stars from cluster-robust Wald p (ka cluster); boottest not supported for PPML.",
  "  $\\bar{\\mu}$: mean fitted value. OLS-scale: $\\bar{\\mu} \\times \\hat{b}_2^{\\text{PPML}}$.",
  "  OLS same sample: OLS estimated on the same observations used by fepois.",
  "  N dropped: cells removed because all observations in an FE group have zero outcome (separation).",
  "\\end{table}")
writeLines(app_tex, file.path(out_dir,"b2_ppml_native_v6.tex"), useBytes=TRUE)

cat("\n=== MAIN TABLE ===\n")
print(tbl, row.names=FALSE)
cat("\nAll files written to:", out_dir, "\n")
cat("Done.\n")
