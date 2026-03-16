################################################################################
# Table 2: Gender Composition and Tenure (Interaction Design)
# C1: female_share_all ~ n_drafted_male                              (baseline)
# C2: female_share_all ~ n_drafted_male + ×Rank1 + ×Rank3           (rank int.)
# C3: female_share_all ~ n_drafted_male + ×Engineer                 (eng. int.)
# C4: avg_tenure ~ n_drafted_male                                   (baseline)
# C5: avg_tenure ~ n_drafted_male + ×Rank1 + ×Rank3                 (rank int.)
# C6: avg_tenure ~ n_drafted_male + ×Engineer                       (eng. int.)
# All: full sample, FE = year + ka + pos, cluster = office
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

# Rank (pre-1948 only here)
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^局長$") ~ 5L,
    str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    TRUE ~ 2L
  )
}

# --- Panel construction ---
office_initial_year <- df %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    is_new_hire = case_when(year_num == office_first_year ~ NA, TRUE ~ (year_num == first_year)),
    pos_rank = assign_rank(pos_norm)
  )

pos_ka_map <- staff_transitions %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

# --- Compute tenure for each worker-year ---
# Tenure = number of distinct years this staff_id has appeared up to current year
staff_year_counts <- df %>%
  group_by(staff_id) %>%
  arrange(year_num) %>%
  mutate(tenure = row_number()) %>%
  ungroup() %>%
  select(staff_id, year_num, tenure)

# Merge tenure into staff_transitions
staff_transitions <- staff_transitions %>%
  left_join(staff_year_counts, by = c("staff_id", "year_num"))

# All workers at position level (with avg tenure)
all_workers <- staff_transitions %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total = n(), n_female = sum(is_female), n_male = sum(!is_female),
    female_share_all = n_female / n_total,
    avg_tenure = mean(tenure, na.rm = TRUE),
    pos_rank = first(pos_rank),
    .groups = "drop"
  )

# Drafts
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_male = sum(!is_female), n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    .groups = "drop"
  )

# Cumulative baseline
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

panel <- all_workers %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted_male, draft_share, cumul_n_male), ~replace_na(., 0)),
    any_drafted = as.integer(n_drafted_male > 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    is_rank1 = as.integer(pos_rank == 1),
    is_rank2 = as.integer(pos_rank == 2),
    is_rank3 = as.integer(pos_rank >= 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

panel_ka <- panel %>% filter(!is.na(ka_id))
cat("Panel:", nrow(panel_ka), "obs\n")

# ============================================================
# REGRESSIONS (full sample, interaction design)
# ============================================================

# --- C1: Female share baseline ---
c1 <- feols(female_share_all ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- C2: Female share × Rank (rank 2 = base) ---
c2 <- feols(female_share_all ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- C3: Female share × Engineer ---
c3 <- feols(female_share_all ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- C4: Average tenure baseline ---
c4 <- feols(avg_tenure ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- C5: Average tenure × Rank (rank 2 = base) ---
c5 <- feols(avg_tenure ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- C6: Average tenure × Engineer ---
c6 <- feols(avg_tenure ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# PRINT
# ============================================================

cat("\n===== TABLE 2: GENDER COMPOSITION AND TENURE =====\n\n")
cat("--- C1: Female share (baseline) ---\n")
etable(c1, se.below = TRUE, fitstat = ~n + r2, headers = "Baseline")

cat("\n--- C2: Female share × Rank ---\n")
etable(c2, se.below = TRUE, fitstat = ~n + r2, headers = "× Rank")

cat("\n--- C3: Female share × Engineer ---\n")
etable(c3, se.below = TRUE, fitstat = ~n + r2, headers = "× Eng.")

cat("\n--- C4: Avg tenure (baseline) ---\n")
etable(c4, se.below = TRUE, fitstat = ~n + r2, headers = "Baseline")

cat("\n--- C5: Avg tenure × Rank ---\n")
etable(c5, se.below = TRUE, fitstat = ~n + r2, headers = "× Rank")

cat("\n--- C6: Avg tenure × Engineer ---\n")
etable(c6, se.below = TRUE, fitstat = ~n + r2, headers = "× Eng.")

# ============================================================
# EXPORT LaTeX
# ============================================================

dict <- c(
  n_drafted_male = "No. drafted workers",
  "n_drafted_male:is_rank1" = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3" = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer" = "No. drafted $\\times$ Engineer",
  "log(cumul_n_male + 1)" = "log(Male baseline + 1)",
  ka_id = "Ka"
)

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
  drop_idx <- grep("female\\_share|avg\\_tenure", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  tex_content
}

tex <- etable(c1, c2, c3, c4, c5, c6,
              dict = dict,
              drop = "log",
              order = c("No. drafted workers", "No. drafted .* Rank", "No. drafted .* Engineer"),
              headers = c("Baseline", "$\\times$ Rank", "$\\times$ Eng.",
                          "Baseline", "$\\times$ Rank", "$\\times$ Eng."),
              se.below = TRUE, fitstat = ~n,
              tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var panel header after the column-header line
# Find the first \midrule and insert dep-var row before it
mid_idx <- grep("\\\\midrule", tc)
if (length(mid_idx) > 0) {
  insert_at <- mid_idx[1]
  depvar_row <- "\\midrule Dep.\\ var: & \\multicolumn{3}{c}{Female share} & \\multicolumn{3}{c}{Avg.\\ tenure} \\\\"
  tc <- c(tc[1:(insert_at - 1)], depvar_row, tc[(insert_at + 1):length(tc)])
}

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Gender Composition and Tenure}",
  "\\label{tab:gender}",
  "\\small",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions at the position $\\times$ year level (1938--1945). ",
         "Columns (1)--(3): dependent variable is female share of all workers. ",
         "Columns (4)--(6): dependent variable is average tenure (years in data). ",
         "Column (1) and (4) report baseline effects. ",
         "Columns (2) and (5) interact draft exposure with rank indicators ",
         "(rank 1 = \\emph{yatoi}/temporary; rank 3+ = senior; rank 2 = base category, absorbed by position FE). ",
         "Columns (3) and (6) interact draft exposure with an engineer indicator ",
         "(positions containing \\begin{CJK}{UTF8}{min}技\\end{CJK}). ",
         "All specifications include log(cumulative male baseline $+$ 1) as a control (not displayed), ",
         "as well as year, ka, and position fixed effects. ",
         "Standard errors clustered at the office level. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable2_Gender.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable2_Gender.tex"))
cat("\nTable 2 exported to NewTable2_Gender.tex and Tokyo_Project\n")
