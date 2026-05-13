################################################################################
# Combined Table: Transfer Outcomes
#
# Final layout:
# Panel B: Transferred vs donor-office peers
# Panel C: Draft-induced vs non-draft destination transfers
# Panel A: Draft-vacancy vs non-draft-vacancy transferees
#
# Rows = panels; Columns = outcomes
################################################################################

library(tidyverse)
library(fixest)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945
postwar_years     <- 1947:1955

# ============================================================
# HELPERS
# ============================================================

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
    str_detect(k, "社会")       ~ "社会局",
    str_detect(k, "保健")       ~ "保健局",
    str_detect(k, "上木")       ~ "土木局",
    str_detect(k, "土木")       ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務")   ~ "府会事務局",
    str_detect(k, "發育")       ~ "教育局",
    str_detect(k, "教育")       ~ "教育局",
    str_detect(k, "水違")       ~ "水道局",
    str_detect(k, "水道")       ~ "水道局",
    str_detect(k, "電気")       ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務")       ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",
    str_detect(k, "経済")       ~ "経済局",
    str_detect(k, "厚生")       ~ "厚生局",
    str_detect(k, "市民")       ~ "市民局",
    str_detect(k, "中央卸売")   ~ "中央卸売市場",
    str_detect(k, "港準")       ~ "港湾局",
    str_detect(k, "港湾")       ~ "港湾局",
    str_detect(k, "監査")       ~ "監査局",
    str_detect(k, "産業")       ~ "産業局",
    str_detect(k, "城東病院")   ~ "健民局",
    str_detect(k, "健民")       ~ "健民局",
    str_detect(k, "戰時生活")   ~ "戰時生活局",
    str_detect(k, "経〓")       ~ "経理局",
    str_detect(k, "経理")       ~ "経理局",
    str_detect(k, "労働")       ~ "労働局",
    str_detect(k, "建築|御築")  ~ "建築局",
    str_detect(k, "道路")       ~ "道路局",
    str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通")       ~ "交通局",
    str_detect(k, "計[畫画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醉院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

parse_court_rank <- function(x) {
  case_when(
    is.na(x) ~ NA_real_,
    str_detect(x, "正一") ~ 1,
    str_detect(x, "正二") ~ 2,
    str_detect(x, "正三") ~ 3,
    str_detect(x, "正四") ~ 4,
    str_detect(x, "正五") ~ 5,
    str_detect(x, "正六") ~ 6,
    str_detect(x, "正七") ~ 7,
    str_detect(x, "正八") ~ 8,
    str_detect(x, "正九") ~ 9,
    str_detect(x, "正十") ~ 10,
    TRUE ~ NA_real_
  )
}

parse_decoration <- function(x) {
  case_when(
    is.na(x) ~ NA_real_,
    str_detect(x, "勳一|勲一|功一") ~ 1,
    str_detect(x, "勳二|勲二|功二") ~ 2,
    str_detect(x, "勳三|勲三|功三") ~ 3,
    str_detect(x, "勳四|勲四|功四") ~ 4,
    str_detect(x, "勳五|勲五|功五") ~ 5,
    str_detect(x, "勳六|勲六|功六") ~ 6,
    str_detect(x, "勳七|勲七|功七") ~ 7,
    str_detect(x, "勳八|勲八|功八") ~ 8,
    str_detect(x, "勳九|勲九|功九") ~ 9,
    str_detect(x, "勳十|勲十|功十") ~ 10,
    TRUE ~ NA_real_
  )
}

rank_to_prestige_score <- function(x) {
  if_else(is.na(x), 0, pmax(0, 9 - as.numeric(x)))
}

df <- df %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         pos_rank   = assign_rank(pos_norm, year_num),
         court_rank = parse_court_rank(rank),
         decor_rank = parse_decoration(decoration))

df_all <- df_all %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         pos_rank   = assign_rank(pos_norm, year_num),
         court_rank = parse_court_rank(rank),
         decor_rank = parse_decoration(decoration))

# ============================================================
# SHARED OUTCOMES
# ============================================================

worker_outcomes <- df %>%
  group_by(staff_id) %>%
  summarise(max_rank = max(pos_rank, na.rm = TRUE),
            last_year = max(year_num), first_year = min(year_num), .groups = "drop")

postwar_outcomes <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(postwar_years = n_distinct(year_num), .groups = "drop")

# ============================================================
# PANELS A & B: TRANSFERRED vs PEERS
# ============================================================

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, norm_kyoku, pos_norm, pos_rank) %>%
  rename_with(~paste0("lag_", .), -c(staff_id, year_num)) %>%
  mutate(year_num = year_num + 1)

worker_panel <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office_id), !is.na(lag_ka)) %>%
  mutate(transferred = as.integer(lag_ka != ka | lag_norm_kyoku != norm_kyoku))

transferred_workers <- worker_panel %>%
  filter(transferred == 1) %>%
  select(staff_id, transfer_year = year_num,
         donor_office = lag_office_id, donor_ka = lag_ka,
         dest_office = office_id, dest_ka = ka,
         dest_pos = pos_norm, dest_rank = pos_rank,
         donor_pos = lag_pos_norm, donor_rank = lag_pos_rank,
         is_female, norm_kyoku, lag_norm_kyoku)

tenure_df <- df %>%
  group_by(staff_id) %>%
  summarise(first_yr = min(year_num), .groups = "drop")

office_composition <- df %>%
  filter(year_num %in% years_of_interest, !is.na(ka)) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(peer_headcount = n(), peer_avg_rank = mean(pos_rank, na.rm = TRUE),
            peer_female_share = mean(is_female, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    df %>% filter(year_num %in% years_of_interest, !is.na(ka)) %>%
      left_join(tenure_df, by = "staff_id") %>%
      mutate(tenure = year_num - first_yr) %>%
      group_by(office_id, ka, year_num) %>%
      summarise(peer_avg_tenure = mean(tenure, na.rm = TRUE), .groups = "drop"),
    by = c("office_id", "ka", "year_num")
  )

# Panel A: vs destination peers
dest_cells <- transferred_workers %>% distinct(dest_office, dest_ka, transfer_year)
dest_all_workers <- dest_cells %>%
  inner_join(df %>% filter(year_num %in% years_of_interest, !is.na(ka)) %>%
               select(staff_id, office_id, ka, year_num, pos_norm, pos_rank, is_female),
             by = c("dest_office" = "office_id", "dest_ka" = "ka", "transfer_year" = "year_num"))
transferred_ids <- transferred_workers %>%
  select(staff_id, transfer_year, dest_office, dest_ka) %>% mutate(is_transferred = 1L)

dest_individual <- dest_all_workers %>%
  left_join(transferred_ids, by = c("staff_id", "transfer_year", "dest_office", "dest_ka")) %>%
  mutate(is_transferred = replace_na(is_transferred, 0L)) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  left_join(office_composition,
            by = c("dest_office" = "office_id", "dest_ka" = "ka", "transfer_year" = "year_num")) %>%
  mutate(years_after = last_year - transfer_year,
         postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
         postwar_years = replace_na(postwar_years, 0),
         rank_gain = max_rank - pos_rank,
         dest_ka_id = paste(dest_office, dest_ka, sep = "_"))

dest_common <- dest_individual %>%
  filter(!is.na(dest_ka_id), !is.na(pos_norm), !is.na(rank_gain))

# Panel B: vs donor peers
donor_cells <- transferred_workers %>%
  distinct(donor_office, donor_ka, transfer_year) %>% mutate(donor_year = transfer_year - 1)
donor_all_workers <- donor_cells %>%
  inner_join(df %>% filter(year_num %in% (min(years_of_interest) - 1):max(years_of_interest), !is.na(ka)) %>%
               select(staff_id, office_id, ka, year_num, pos_norm, pos_rank, is_female, court_rank, decor_rank),
             by = c("donor_office" = "office_id", "donor_ka" = "ka", "donor_year" = "year_num"))
donor_transferred_ids <- transferred_workers %>%
  select(staff_id, transfer_year, donor_office, donor_ka) %>% mutate(is_transferred = 1L)

donor_individual <- donor_all_workers %>%
  left_join(donor_transferred_ids, by = c("staff_id", "transfer_year", "donor_office", "donor_ka")) %>%
  mutate(is_transferred = replace_na(is_transferred, 0L)) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  left_join(office_composition,
            by = c("donor_office" = "office_id", "donor_ka" = "ka", "donor_year" = "year_num")) %>%
  mutate(years_after = last_year - transfer_year,
         postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
         postwar_years = replace_na(postwar_years, 0),
         rank_gain = max_rank - pos_rank,
         donor_ka_id = paste(donor_office, donor_ka, sep = "_"),
         productivity_score = rowSums(
           cbind(
             rank_to_prestige_score(decor_rank),
             rank_to_prestige_score(court_rank)
           ),
           na.rm = TRUE
         ),
         productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
         productive = replace_na(productive, 0L),
         donor_cell = paste(donor_office, donor_ka, pos_norm, transfer_year, sep = "_"))

donor_common <- donor_individual %>%
  filter(!is.na(donor_ka_id), !is.na(pos_norm), !is.na(rank_gain))

# Regressions A & B
fml_ctrl <- "peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1)"

d1 <- feols(years_after     ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | dest_ka_id + transfer_year + pos_norm, data = dest_common, cluster = ~dest_ka_id)
d2 <- feols(postwar_survived ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | dest_ka_id + transfer_year + pos_norm, data = dest_common, cluster = ~dest_ka_id)
d3 <- feols(postwar_years    ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | dest_ka_id + transfer_year + pos_norm, data = dest_common, cluster = ~dest_ka_id)
d4 <- feols(rank_gain        ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | dest_ka_id + transfer_year + pos_norm, data = dest_common, cluster = ~dest_ka_id)

s1 <- feols(years_after      ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | donor_ka_id + transfer_year + pos_norm, data = donor_common, cluster = ~donor_ka_id)
s2 <- feols(postwar_survived  ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | donor_ka_id + transfer_year + pos_norm, data = donor_common, cluster = ~donor_ka_id)
s3 <- feols(postwar_years     ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | donor_ka_id + transfer_year + pos_norm, data = donor_common, cluster = ~donor_ka_id)
s4 <- feols(rank_gain         ~ is_transferred + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1) | donor_ka_id + transfer_year + pos_norm, data = donor_common, cluster = ~donor_ka_id)

# ============================================================
# PANEL C: DRAFT-INDUCED vs NATURAL-EXIT DESTINATION
# ============================================================

draft_dest <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  distinct(office_id, ka, year_num) %>%
  mutate(draft_induced_vacancy = 1L)

staff_lag2 <- df %>%
  select(staff_id, year_num, office_id, ka, pos_norm, pos_rank, court_rank, decor_rank) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_pos = pos_norm, lag_rank = pos_rank,
         lag_court_rank = court_rank, lag_decor_rank = decor_rank) %>%
  mutate(year_num = year_num + 1)

transfers <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag2, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office), !is.na(lag_ka)) %>%
  filter(lag_office != office_id | lag_ka != ka) %>%
  left_join(draft_dest, by = c("office_id", "ka", "year_num")) %>%
  mutate(draft_induced_vacancy = replace_na(draft_induced_vacancy, 0L),
         donor_cell = paste(lag_office, lag_ka, lag_pos, year_num, sep = "_"))

analysis_c <- transfers %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  mutate(years_after      = last_year - year_num,
         postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
         postwar_years    = replace_na(postwar_years, 0L),
         rank_gain        = max_rank - lag_rank,
         tenure_at_transfer = year_num - first_year,
         productivity_score = rowSums(
           cbind(
             rank_to_prestige_score(lag_decor_rank),
             rank_to_prestige_score(lag_court_rank)
           ),
           na.rm = TRUE
         ),
         productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
         productive = replace_na(productive, 0L)) %>%
  filter(!is.na(donor_cell), !is.na(lag_rank), !is.na(lag_pos))

analysis_c_common <- analysis_c %>%
  filter(!is.na(rank_gain))

mixed_donor_cells <- analysis_c_common %>%
  group_by(donor_cell) %>%
  summarise(
    n_draft = sum(draft_induced_vacancy == 1L),
    n_nondraft = sum(draft_induced_vacancy == 0L),
    .groups = "drop"
  ) %>%
  filter(n_draft > 0, n_nondraft > 0) %>%
  select(donor_cell)

analysis_c_common <- analysis_c_common %>%
  semi_join(mixed_donor_cells, by = "donor_cell")

donor_common_mixed <- donor_common %>%
  semi_join(mixed_donor_cells, by = "donor_cell")

panel_a_common <- analysis_c_common

r1 <- feols(years_after      ~ draft_induced_vacancy + productive + tenure_at_transfer + is_female | donor_cell, data = analysis_c_common, cluster = ~donor_cell)
r2 <- feols(postwar_survived  ~ draft_induced_vacancy + productive + tenure_at_transfer + is_female | donor_cell, data = analysis_c_common, cluster = ~donor_cell)
r3 <- feols(postwar_years     ~ draft_induced_vacancy + productive + tenure_at_transfer + is_female | donor_cell, data = analysis_c_common, cluster = ~donor_cell)
r4 <- feols(rank_gain         ~ draft_induced_vacancy + productive + tenure_at_transfer + is_female | donor_cell, data = analysis_c_common, cluster = ~donor_cell)

a1 <- feols(years_after      ~ draft_induced_vacancy + productive | donor_cell, data = panel_a_common, cluster = ~donor_cell)
a2 <- feols(postwar_survived ~ draft_induced_vacancy + productive | donor_cell, data = panel_a_common, cluster = ~donor_cell)
a3 <- feols(postwar_years    ~ draft_induced_vacancy + productive | donor_cell, data = panel_a_common, cluster = ~donor_cell)
a4 <- feols(rank_gain        ~ draft_induced_vacancy + productive | donor_cell, data = panel_a_common, cluster = ~donor_cell)

s1 <- feols(years_after      ~ is_transferred | donor_cell, data = donor_common_mixed, cluster = ~donor_cell)
s2 <- feols(postwar_survived  ~ is_transferred | donor_cell, data = donor_common_mixed, cluster = ~donor_cell)
s3 <- feols(postwar_years     ~ is_transferred | donor_cell, data = donor_common_mixed, cluster = ~donor_cell)
s4 <- feols(rank_gain         ~ is_transferred | donor_cell, data = donor_common_mixed, cluster = ~donor_cell)

# ============================================================
# BUILD COMBINED TABLE
# Outcomes = columns, Panels (models) = rows
# Order: Panel B, Panel C, Panel A
# ============================================================

fmt_coef <- function(x) formatC(x, digits = 4, format = "f")
fmt_se   <- function(x) formatC(x, digits = 4, format = "f")

stars <- function(p) {
  case_when(p < 0.01 ~ "$^{***}$", p < 0.05 ~ "$^{**}$", p < 0.1 ~ "$^{*}$", TRUE ~ "")
}

extract_est <- function(mod, var) {
  list(coef = coef(mod)[var], se = se(mod)[var], p = pvalue(mod)[var])
}

# Each panel: label, models (one per outcome), variable name, N
panels <- list(
  list(label = "Panel B: Donor peers",   treatment = "is_transferred",        n = nobs(s1), mods = list(s1, s2, s3, s4)),
  list(label = "Panel C: Draft vacancy", treatment = "draft_induced_vacancy",  n = nobs(r1), mods = list(r1, r2, r3, r4)),
  list(label = "Panel A: Draft vs non-draft transferees", treatment = "draft_induced_vacancy", n = nobs(a1), mods = list(a1, a2, a3, a4))
)

outcome_labels <- c("Tenure after", "Postwar survival", "Postwar years", "Rank gain")

# Build coefficient rows with panels as rows
body_rows <- c()
for (pan in panels) {
  ests <- lapply(pan$mods, function(m) extract_est(m, pan$treatment))
  coef_cells <- sapply(ests, function(e) sprintf("%s%s", fmt_coef(e$coef), stars(e$p)))
  se_cells   <- sapply(ests, function(e) sprintf("(%s)", fmt_se(e$se)))
  n_cells <- rep(formatC(pan$n, format = "d", big.mark = ","), length(outcome_labels))
  fe_cells <- rep("Donor cell FE", length(outcome_labels))
  support_cells <- rep(formatC(nrow(mixed_donor_cells), format = "d", big.mark = ","), length(outcome_labels))
  body_rows <- c(body_rows,
    paste0(pan$label, " & ", paste(coef_cells, collapse = " & "), " \\\\"),
    paste0(" & ", paste(se_cells, collapse = " & "), " \\\\"),
    paste0("Fixed effects & ", paste(fe_cells, collapse = " & "), " \\\\"),
    paste0("Support cells & ", paste(support_cells, collapse = " & "), " \\\\"),
    paste0("Observations & ", paste(n_cells, collapse = " & "), " \\\\[4pt]")
  )
}

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Transfer Outcomes}",
  "\\label{tab:transfer-outcomes-combined}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  paste0(" & ", paste(outcome_labels, collapse = " & "), " \\\\"),
  "\\midrule",
  body_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} ",
    "Panel~B compares transferred workers to peers who remained at the donor office. ",
    "Panel~A compares draft-vacancy transferees to non-draft-vacancy transferees, controlling for worker productivity. ",
    "Panel~C compares transferees within the same donor occupational unit $\\times$ year cell ",
    "by whether the destination had a draft-induced vacancy, controlling for worker productivity, tenure at transfer, and gender; ",
    "productivity is a prior-year indicator built from court rank and decoration class. ",
    "Tenure after is years observed after the transfer year. ",
    "Postwar survival is an indicator for being observed at least once during 1947--1955. ",
    "Postwar years counts the number of distinct observed years during 1947--1955. ",
    "Rank gain is the worker's maximum observed rank minus pre-transfer rank. ",
    "Donor cell is the worker's pre-transfer office $\\times$ section $\\times$ position $\\times$ transfer year. ",
    "All three panels use donor-cell fixed effects and the same donor-cell support: cells with at least one draft-vacancy transfer and at least one non-draft-vacancy transfer. ",
    "Within each panel, all four outcomes are estimated on the same sample. ",
    "Clustered standard errors at the donor-cell level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("TransferOutcomes_Combined.tex"))
cat("\nTable exported to TransferOutcomes_Combined.tex\n\n")

# Console preview
header <- sprintf("%-40s  %14s  %14s  %14s  %14s", "", outcome_labels[1], outcome_labels[2], outcome_labels[3], outcome_labels[4])
cat(header, "\n", strrep("-", 105), "\n")
for (pan in panels) {
  ests <- lapply(pan$mods, function(m) extract_est(m, pan$treatment))
  coefs <- sapply(ests, function(e) sprintf("%.4f%s", e$coef, gsub("\\$|\\^|\\{|\\}", "", stars(e$p))))
  ses   <- sapply(ests, function(e) sprintf("(%.4f)", e$se))
  cat(sprintf("%-40s  %14s  %14s  %14s  %14s\n", pan$label, coefs[1], coefs[2], coefs[3], coefs[4]))
  cat(sprintf("%-40s  %14s  %14s  %14s  %14s\n", "",         ses[1],   ses[2],   ses[3],   ses[4]))
}
cat(strrep("-", 105), "\n")
