################################################################################
# Table 1 Robustness: 1944-only cross-section
# Same specs as NewTable1_Reallocation.R but restricted to year_num == 1944
################################################################################

library(tidyverse)
library(fixest)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))

# --- Cumulative baseline (kakari level) for 1944 only ---
cumul_male_stock <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
  mutate(year_num = 1944L)

# --- Transitions for 1944 ---
office_initial_year <- df %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  select(staff_id, year_num, lag_office = office_id, lag_ka = ka,
         lag_kyoku = kyoku, lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num == 1944) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num"),
            relationship = "many-to-many") %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    transfer_distance = case_when(
      !is_transfer_in ~ NA_integer_,
      !is.na(lag_ka) & !is.na(ka) & lag_ka == ka & lag_kyoku == kyoku ~ 1L,
      !is.na(lag_kyoku) & !is.na(kyoku) & lag_kyoku == kyoku ~ 2L,
      TRUE ~ 3L
    )
  )

# --- Position outcomes (kakari level) ---
position_outcomes <- staff_transitions %>%
  group_by(kyoku, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires       = sum(is_new_hire, na.rm = TRUE),
    n_transfers_in    = sum(is_transfer_in, na.rm = TRUE),
    n_transfers_same_ka = sum(transfer_distance == 1, na.rm = TRUE),
    n_transfers_diff_ka = sum(transfer_distance >= 2, na.rm = TRUE),
    n_transfers_dist2 = sum(transfer_distance == 2, na.rm = TRUE),
    n_transfers_dist3 = sum(transfer_distance == 3, na.rm = TRUE),
    .groups = "drop"
  )

# --- Draft counts (kakari level) ---
position_drafts <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted = n(), n_drafted_male = sum(!is_female, na.rm = TRUE),
            .groups = "drop")

# --- Occupation classification ---
position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    occupation = case_when(
      str_detect(pos_norm, "技") ~ "engineer",
      str_detect(pos_norm, "雇|傭") ~ "yato",
      TRUE ~ "non_engineer"
    )
  )

# --- Neighbor draft measures ---
kakari_totals <- position_panel %>%
  group_by(office_id, kakari) %>%
  summarise(kakari_total_drafts = sum(n_drafted), .groups = "drop")

kakari_occ_totals <- position_panel %>%
  group_by(office_id, kakari, occupation) %>%
  summarise(kakari_occ_drafts = sum(n_drafted), .groups = "drop")

office_occ_totals <- position_panel %>%
  group_by(office_id, occupation) %>%
  summarise(office_occ_drafts = sum(n_drafted), .groups = "drop")

position_panel <- position_panel %>%
  left_join(kakari_totals, by = c("office_id", "kakari")) %>%
  left_join(kakari_occ_totals, by = c("office_id", "kakari", "occupation")) %>%
  left_join(office_occ_totals, by = c("office_id", "occupation")) %>%
  mutate(
    adj_kakari_across_occ = kakari_total_drafts - kakari_occ_drafts,
    adj_ka_within_occ     = office_occ_drafts - n_drafted
  )

cat("1944-only panel:", nrow(position_panel), "obs\n")
cat("  With drafts > 0:", sum(position_panel$n_drafted_male > 0), "\n")

# ============================================================
# REGRESSIONS (cross-section, no year FE)
# ============================================================

cat("\n===== TABLE 1 (1944 ONLY): WORKER REALLOCATION =====\n\n")

# C1: Total transfers in
c1 <- fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
               kyoku + pos_norm,
             data = position_panel, cluster = ~office_id)

# C2: Same-ka transfers
c2a <- fepois(n_transfers_same_ka ~ n_drafted_male + log(cumul_n_male + 1) |
                kyoku + pos_norm,
              data = position_panel, cluster = ~office_id)

# C3: Diff-ka transfers
c2b <- fepois(n_transfers_diff_ka ~ n_drafted_male + log(cumul_n_male + 1) |
                kyoku + pos_norm,
              data = position_panel, cluster = ~office_id)

# C4: Same-kyoku transfers
c3a <- fepois(n_transfers_dist2 ~ n_drafted_male + log(cumul_n_male + 1) |
                kyoku + pos_norm,
              data = position_panel, cluster = ~office_id)

# C5: Diff-kyoku transfers
c3b <- fepois(n_transfers_dist3 ~ n_drafted_male + log(cumul_n_male + 1) |
                kyoku + pos_norm,
              data = position_panel, cluster = ~office_id)

# C6: New hires
c4 <- fepois(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
               kyoku + pos_norm,
             data = position_panel, cluster = ~office_id)

# C7: Adjacent drafts (diff kakari, same occ)
c5 <- fepois(n_new_hires ~ adj_ka_within_occ + log(cumul_n_male + 1) |
               kyoku + pos_norm,
             data = position_panel, cluster = ~office_id)

# C8: Adjacent drafts (diff occ, same kakari)
c6 <- fepois(n_new_hires ~ adj_kakari_across_occ + log(cumul_n_male + 1) |
               kyoku + pos_norm,
             data = position_panel, cluster = ~office_id)

etable(c1, c2a, c2b, c3a, c3b, c4, c5, c6,
       headers = c("Transfers In", "Same Ka", "Diff Ka",
                    "Dist 2", "Dist 3", "New Hires",
                    "Adj (diff kakari)", "Adj (diff occ)"),
       se.below = TRUE, fitstat = ~n)

# ============================================================
# TABLE 2: GENDER COMPOSITION (1944 only)
# ============================================================

cat("\n\n===== TABLE 2 (1944 ONLY): GENDER COMPOSITION =====\n\n")

# Need position-level panel with gender counts
gender_outcomes <- staff_transitions %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_female = sum(is_new_hire & is_female, na.rm = TRUE),
    n_new_male   = sum(is_new_hire & !is_female, na.rm = TRUE),
    n_total      = n(),
    n_female_all = sum(is_female, na.rm = TRUE),
    female_share_all = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

# New hire female share
new_hires_only <- staff_transitions %>%
  filter(is_new_hire == TRUE) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    female_share_new = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

gender_panel <- gender_outcomes %>%
  left_join(new_hires_only, by = c("office_id", "ka", "kakari", "pos_norm", "year_num"))

# Rank classification
gender_panel <- gender_panel %>%
  mutate(
    rank_cat = case_when(
      str_detect(pos_norm, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
      TRUE ~ 2L
    )
  )

# Draft info
pos_drafts <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  group_by(office_id, pos_norm) %>%
  summarise(n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

cumul_male_pos <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop")

gender_panel <- gender_panel %>%
  left_join(pos_drafts, by = c("office_id", "pos_norm")) %>%
  left_join(cumul_male_pos, by = c("office_id", "pos_norm")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    any_drafted = as.integer(n_drafted_male > 0)
  )

# C1-2: Female vs male new hires (Poisson)
g1 <- fepois(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) |
               ka + pos_norm,
             data = gender_panel, cluster = ~office_id)

g2 <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) |
               ka + pos_norm,
             data = gender_panel, cluster = ~office_id)

# C3-4: With rank interaction
gender_panel <- gender_panel %>% mutate(is_rank1 = as.integer(rank_cat == 1))

g3 <- tryCatch(
  fepois(n_new_female ~ n_drafted_male + n_drafted_male:is_rank1 + log(cumul_n_male + 1) |
           ka + pos_norm,
         data = gender_panel, cluster = ~office_id),
  error = function(e) { cat("  g3 error:", e$message, "\n"); NULL }
)

g4 <- tryCatch(
  fepois(n_new_male ~ n_drafted_male + n_drafted_male:is_rank1 + log(cumul_n_male + 1) |
           ka + pos_norm,
         data = gender_panel, cluster = ~office_id),
  error = function(e) { cat("  g4 error:", e$message, "\n"); NULL }
)

# C5-8: Female share OLS by rank
g5 <- feols(female_share_all ~ any_drafted + cumul_n_male |
              ka + pos_norm,
            data = gender_panel %>% filter(rank_cat == 1), cluster = ~office_id)

g6 <- feols(female_share_all ~ any_drafted + cumul_n_male |
              ka + pos_norm,
            data = gender_panel %>% filter(rank_cat == 2), cluster = ~office_id)

g7 <- feols(female_share_new ~ any_drafted + cumul_n_male |
              ka + pos_norm,
            data = gender_panel %>% filter(rank_cat == 1), cluster = ~office_id)

g8 <- feols(female_share_new ~ any_drafted + cumul_n_male |
              ka + pos_norm,
            data = gender_panel %>% filter(rank_cat == 2), cluster = ~office_id)

models_g <- list(g1, g2, g3, g4, g5, g6, g7, g8)
models_g <- models_g[!sapply(models_g, is.null)]

etable(models_g, se.below = TRUE, fitstat = ~n)

# ============================================================
# EXPORT LaTeX TABLES
# ============================================================

# --- Appendix Table A1: Reallocation (1944 only) ---
dict1 <- c(
  n_drafted_male        = "Own drafts (male)",
  adj_ka_within_occ     = "Adj. drafts (diff kakari, same occ)",
  adj_kakari_across_occ = "Adj. drafts (diff occ, same kakari)",
  "log(cumul_n_male + 1)" = "log(Male baseline + 1)"
)

tex1 <- etable(c1, c2a, c2b, c3a, c3b, c4, c5, c6,
               dict = dict1,
               headers = c("Transfers\nIn",
                            "Same Ka\nTransfers",
                            "Diff Ka\nTransfers",
                            "Same Kyoku\nTransfers",
                            "Diff Kyoku\nTransfers",
                            "New\nHires",
                            "New Hires\n(adj kakari)",
                            "New Hires\n(adj occ)"),
               se.below = TRUE, fitstat = ~n,
               tex = TRUE)

tex1_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Worker Reallocation in Response to Military Drafting (1944 Only)}",
  "\\label{tab:reallocation-1944}",
  "\\small",
  "\\begin{threeparttable}",
  tex1,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} Poisson regressions restricted to 1944. Unit of observation: position $\\times$ kakari. Column specifications mirror Table~\\ref{tab:reallocation}. All specifications include kyoku and position fixed effects. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex1_out, "../AppendixTable_Reallocation1944.tex")
cat("\nAppendix Table A1 exported.\n")

# --- Appendix Table A2: Gender (1944 only) ---
dict2 <- c(
  n_drafted_male = "Own drafts (male)",
  "log(cumul_n_male + 1)" = "log(Male baseline + 1)",
  "n_drafted_male:is_rank1" = "Own drafts $\\times$ Rank 1",
  any_drafted = "Any drafted (=1)",
  cumul_n_male = "Male baseline"
)

# Build model list for export (handle NULLs)
g_models <- list(g1, g2, g3, g4, g5, g6, g7, g8)
g_valid  <- !sapply(g_models, is.null)

# Only export valid models
g_export <- g_models[g_valid]

tex2 <- etable(g_export,
               dict = dict2,
               se.below = TRUE, fitstat = ~n,
               tex = TRUE)

# Determine header based on which models survived
header_full <- c("Female\nHires", "Male\nHires",
                 "Female\nHires", "Male\nHires",
                 "Fem. Share\n(All, R1)", "Fem. Share\n(All, R2)",
                 "Fem. Share\n(New, R1)", "Fem. Share\n(New, R2)")
headers_used <- header_full[g_valid]

tex2 <- etable(g_export,
               dict = dict2,
               headers = headers_used,
               se.below = TRUE, fitstat = ~n,
               tex = TRUE)

tex2_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Gender Composition of Hiring and Workforce (1944 Only)}",
  "\\label{tab:gender-1944}",
  "\\small",
  "\\begin{threeparttable}",
  tex2,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} Restricted to 1944. Column specifications mirror Table~\\ref{tab:gender}. Poisson regressions for hire counts; OLS for female shares. All specifications include ka and position fixed effects. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex2_out, "../AppendixTable_Gender1944.tex")
cat("Appendix Table A2 exported.\n")
