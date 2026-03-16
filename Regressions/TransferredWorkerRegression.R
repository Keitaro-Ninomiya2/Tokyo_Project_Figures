################################################################################
# Transferred Worker Characteristics & Postwar Outcomes
#
# Part A: For offices that surrendered a worker (donor kakari x position),
#         what characterized the transferred worker?
#   - Were they women?
#   - Did they have longer tenure?
#   - Were they higher/lower rank (productivity proxy)?
#   Unit: individual worker x year, FE: ka + position + year
#
# Part B: For remaining workers in donor offices, postwar outcomes
#   - Tenure length postwar
#   - Promotion (rank change)
#   Compared to same-ka offices that did NOT have transfers out
#   Unit: individual worker, FE: ka + position (from 1944)
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

# ============================================================
# RANK ASSIGNMENT
# ============================================================

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

assign_rank_enriched <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr < 1948 & str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    yr < 1948 & str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr >= 1948 & str_detect(pos, "^部長$|^次長$") ~ 4L,
    yr >= 1948 & str_detect(pos, "^課長$|^所長$|^校長$|^場長$|^園長$|^支所長$|^區長$|^看護婦長$|^寮長$") ~ 3L,
    yr >= 1948 & str_detect(pos, "^係長$") ~ 2L,
    yr >= 1948 ~ 1L
  )
}

df <- df %>% mutate(rank_e = assign_rank_enriched(pos_norm, year_num))

# Z-score by era
df <- df %>%
  mutate(era = if_else(year_num < 1948, "pre", "post")) %>%
  group_by(era) %>%
  mutate(rank_z = (rank_e - mean(rank_e)) / sd(rank_e)) %>%
  ungroup()

# ============================================================
# TENURE COMPUTATION
# ============================================================

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

df <- df %>% left_join(staff_first_year, by = "staff_id") %>%
  mutate(tenure = year_num - first_year + 1)

# ============================================================
# PART A: IDENTIFY TRANSFERRED WORKERS
# ============================================================

# Lag: where was each worker last year?
staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, kakari, pos_norm, kyoku) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_kakari = kakari,
         lag_pos = pos_norm, lag_kyoku = kyoku) %>%
  mutate(year_num = year_num + 1)

worker_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    # Transfer = worker changed office_id from prior year (handles NAs safely)
    is_transferred = !is.na(lag_office) & !is.na(office_id) &
      (lag_office != office_id),
    is_transferred = replace_na(is_transferred, FALSE),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

# For Part A, we want to compare workers who were transferred (moved to a new unit)
# vs. workers who stayed in their unit. Individual-level analysis.

indiv_panel <- worker_transitions %>%
  filter(!is.na(lag_office)) %>%  # Must have prior-year record
  mutate(
    was_transferred = as.integer(is_transferred),
    lag_ka_id = if_else(!is.na(lag_ka) & !is.na(lag_kyoku),
                        paste(lag_kyoku, lag_ka, sep = "_"), NA_character_)
  )

# Use PRIOR year's unit for FE (since we're asking: among workers at a unit,
# who got selected to transfer?)
indiv_panel_ka <- indiv_panel %>% filter(!is.na(lag_ka_id))

cat("Part A sample:", nrow(indiv_panel_ka), "worker-year obs\n")
cat("  Transferred:", sum(indiv_panel_ka$was_transferred == 1), "\n")
cat("  Stayed:", sum(indiv_panel_ka$was_transferred == 0), "\n\n")

# --- Regressions: What predicts being transferred? ---

# A1: Were they women?
a1 <- feols(was_transferred ~ is_female | year_num + lag_ka_id + lag_pos,
            data = indiv_panel_ka, cluster = ~lag_office)

# A2: Did they have longer tenure?
a2 <- feols(was_transferred ~ tenure | year_num + lag_ka_id + lag_pos,
            data = indiv_panel_ka, cluster = ~lag_office)

# A3: Were they higher/lower rank? (enriched rank as productivity proxy)
a3 <- feols(was_transferred ~ rank_e | year_num + lag_ka_id + lag_pos,
            data = indiv_panel_ka, cluster = ~lag_office)

# A4: All together
a4 <- feols(was_transferred ~ is_female + tenure + rank_e |
              year_num + lag_ka_id + lag_pos,
            data = indiv_panel_ka, cluster = ~lag_office)

cat("===== PART A: WHO GETS TRANSFERRED? =====\n\n")
etable(a1, a2, a3, a4, se.below = TRUE, fitstat = ~n)

# ============================================================
# PART B: POSTWAR OUTCOMES FOR REMAINING WORKERS
# ============================================================

# Identify donor offices at kakari x position level (any year 1938-1945)
# A donor office is one where at least one worker transferred out

# For each (office_id, kakari, pos_norm), flag whether any worker transferred out
donor_units <- worker_transitions %>%
  filter(is_transferred == TRUE) %>%
  distinct(lag_office, lag_kakari, lag_pos) %>%
  rename(office_id = lag_office, kakari = lag_kakari, pos_norm = lag_pos) %>%
  mutate(ever_donor = 1L)

# Focus on 1944 non-drafted workers (same approach as NewTable3_FutureOutcomes)
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id)

workers_1944 <- df %>%
  filter(year_num == 1944) %>%
  anti_join(drafted_ids, by = "staff_id") %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office_id, pos_norm, kyoku, ka, kakari, is_female,
         rank_e_1944 = rank_e, rank_z_1944 = rank_z, tenure_1944 = tenure) %>%
  left_join(donor_units, by = c("office_id", "kakari", "pos_norm")) %>%
  mutate(
    ever_donor = replace_na(ever_donor, 0L),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

# Cumulative male baseline
cumul_male_stock <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop")

workers_1944 <- workers_1944 %>%
  left_join(cumul_male_stock, by = "office_id") %>%
  mutate(cumul_n_male = replace_na(cumul_n_male, 0))

# Postwar outcomes (1946-1955)
postwar_years <- 1946:1955

postwar_rank <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(
    avg_rank_e = mean(rank_e), avg_rank_z = mean(rank_z),
    .groups = "drop"
  )

postwar_tenure <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(tenure_postwar = n_distinct(year_num), .groups = "drop")

# Check if promoted (rank increased from 1944 to any postwar year)
postwar_promoted <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(max_rank_e = max(rank_e), .groups = "drop")

indiv_b <- workers_1944 %>%
  left_join(postwar_rank, by = "staff_id") %>%
  left_join(postwar_tenure, by = "staff_id") %>%
  left_join(postwar_promoted, by = "staff_id") %>%
  mutate(
    tenure_postwar = replace_na(tenure_postwar, 0),
    rank_z_change = avg_rank_z - rank_z_1944,
    promoted = as.integer(!is.na(max_rank_e) & max_rank_e > rank_e_1944)
  )

indiv_b_ka <- indiv_b %>% filter(!is.na(ka_id))
indiv_b_pw <- indiv_b_ka %>% filter(!is.na(avg_rank_e))  # appeared postwar

cat("\nPart B sample:\n")
cat("  All 1944 non-drafted:", nrow(indiv_b_ka), "\n")
cat("  Appeared postwar:", nrow(indiv_b_pw), "\n")
cat("  In donor offices:", sum(indiv_b_ka$ever_donor), "\n\n")

# --- Regressions ---

# B1: Postwar tenure ~ donor office
b1 <- feols(tenure_postwar ~ ever_donor + log(cumul_n_male + 1) |
              ka_id + pos_norm,
            data = indiv_b_ka, cluster = ~office_id)

# B2: Postwar tenure by gender
b2 <- feols(tenure_postwar ~ ever_donor * is_female + log(cumul_n_male + 1) |
              ka_id + pos_norm,
            data = indiv_b_ka, cluster = ~office_id)

# B3: Promotion (rank change) ~ donor office
b3 <- feols(rank_z_change ~ ever_donor + log(cumul_n_male + 1) |
              ka_id + pos_norm,
            data = indiv_b_pw, cluster = ~office_id)

# B4: Promotion by gender
b4 <- feols(rank_z_change ~ ever_donor * is_female + log(cumul_n_male + 1) |
              ka_id + pos_norm,
            data = indiv_b_pw, cluster = ~office_id)

cat("===== PART B: POSTWAR OUTCOMES FOR REMAINING WORKERS =====\n\n")
etable(b1, b2, b3, b4, se.below = TRUE, fitstat = ~n + r2)

# ============================================================
# EXPORT LaTeX — PART A
# ============================================================

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else {
    tex_raw
  }
}

clean_depvar <- function(tex_content) {
  drop_idx <- grep("was\\_transferred|tenure\\_postwar|rank\\_z\\_change", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  drop_idx3 <- grep("^\\s*&\\s*(OLS|Poisson)", tex_content)
  if (length(drop_idx3) > 0) tex_content <- tex_content[-drop_idx3]
  tex_content
}

dict_a <- c(
  is_femaleTRUE = "Female (=1)",
  tenure        = "Tenure (years)",
  rank_e        = "Rank (enriched)"
)

tex_a <- etable(a1, a2, a3, a4,
                dict = dict_a,
                headers = c("Female", "Tenure", "Rank", "All"),
                se.below = TRUE, fitstat = ~n, tex = TRUE)

tc_a <- clean_depvar(extract_tabular(tex_a))

tex_out_a <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Characteristics of Transferred Workers}",
  "\\label{tab:transferred_chars}",
  "\\begin{threeparttable}",
  tc_a,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions (linear probability model). ",
         "Dependent variable: transferred (=1 if worker moved to a different unit from prior year). ",
         "Unit of observation: individual worker $\\times$ year (1938--1945), restricted to workers with prior-year records. ",
         "Rank is an enriched 1--5 scale serving as a productivity proxy. ",
         "All specifications include year, ka (prior-year), and position (prior-year) fixed effects. ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out_a, here("TransferredWorkerChars.tex"))
writeLines(tex_out_a, here("..", "Tokyo_Project", "Tables_Figures", "TransferredWorkerChars.tex"))
cat("\nPart A table exported to TransferredWorkerChars.tex\n")

# ============================================================
# EXPORT LaTeX — PART B
# ============================================================

dict_b <- c(
  ever_donor                      = "Donor office (=1)",
  is_femaleTRUE                   = "Female (=1)",
  "ever_donor:is_femaleTRUE"      = "Donor $\\times$ Female"
)

tex_b <- etable(b1, b2, b3, b4,
                dict = dict_b,
                drop = "log",
                headers = c("Tenure", "Tenure", "Promotion", "Promotion"),
                se.below = TRUE, fitstat = ~n + r2, tex = TRUE)

tc_b <- clean_depvar(extract_tabular(tex_b))

tex_out_b <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Postwar Outcomes of Remaining Workers in Donor vs.\\ Non-Donor Offices}",
  "\\label{tab:donor_postwar}",
  "\\begin{threeparttable}",
  tc_b,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Sample: non-drafted workers observed in 1944. ",
         "Columns (1)--(2): dependent variable is postwar tenure (count of years appearing 1946--1955). ",
         "Columns (3)--(4): dependent variable is z-scored rank change (postwar avg.\\ minus 1944 rank). ",
         "A \\emph{donor} office had at least one worker transfer out during 1938--1945. ",
         "The comparison group is same-ka offices (kakari $\\times$ position) that did not have transfers out. ",
         "All specifications include ka and position fixed effects from 1944 and control for log cumulative male stock (not shown). ",
         "Standard errors clustered at the office level. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out_b, here("DonorPostwarOutcomes.tex"))
writeLines(tex_out_b, here("..", "Tokyo_Project", "Tables_Figures", "DonorPostwarOutcomes.tex"))
cat("Part B table exported to DonorPostwarOutcomes.tex\n")
