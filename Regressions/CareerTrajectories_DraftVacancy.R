################################################################################
# Career Trajectories and Draft-Induced Vacancies
#
# Research question: do workers assigned to draft-induced vacancies have better
# career outcomes than those assigned to natural-exit vacancies?
#
# Design (cross-section within donor cell):
#   For each donor unit (office × ka × pos) × year cell, find all workers
#   transferred out. Classify each destination as draft-induced or natural-exit
#   based on whether the destination office × ka had ANY draft departure in
#   that same year. Compare career outcomes within the donor cell.
#
# CareerOutcome_i = β·DraftInduced_i + γX_i + α_(unit×year) + ε_i
#
# Outcomes: years remaining in records, postwar survival, postwar years,
#           rank gain after transfer
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
# KYOKU NORMALIZATION + RANK
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

df <- df %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         pos_rank   = assign_rank(pos_norm, year_num))

df_all <- df_all %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         pos_rank   = assign_rank(pos_norm, year_num))

# ============================================================
# 1. CLASSIFY DESTINATION OFFICE × KA × YEAR AS DRAFT-INDUCED
#
# A destination (office, ka) in year t is draft-induced if ANY worker
# at that (office, ka) had drafted == TRUE in year t (i.e. was conscripted
# during that year, creating a vacancy filled by the incoming transfer).
# ============================================================

draft_dest <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  distinct(office_id, ka, year_num) %>%
  mutate(draft_induced_vacancy = 1L)

cat("Destination office×ka×year cells with at least one draft departure:\n")
cat(" ", nrow(draft_dest), "\n\n")

# ============================================================
# 2. IDENTIFY TRANSFERS AND JOIN VACANCY CLASSIFICATION
# ============================================================

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, pos_norm, pos_rank) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_pos = pos_norm, lag_rank = pos_rank) %>%
  mutate(year_num = year_num + 1)

transfers <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office), !is.na(lag_ka)) %>%
  filter(lag_office != office_id | lag_ka != ka) %>%
  # Join destination vacancy type at office × ka × year level
  left_join(draft_dest, by = c("office_id", "ka", "year_num")) %>%
  mutate(
    draft_induced_vacancy = replace_na(draft_induced_vacancy, 0L),
    donor_cell = paste(lag_office, lag_ka, lag_pos, year_num, sep = "_")
  )

cat("Total transfers:", nrow(transfers), "\n")
cat("  → Draft-induced destination:", sum(transfers$draft_induced_vacancy == 1), "\n")
cat("  → Natural-exit destination:",  sum(transfers$draft_induced_vacancy == 0), "\n\n")

# ============================================================
# 3. CHECK WITHIN-CELL VARIATION
# Identification requires donor cells with BOTH types of destination
# ============================================================

cell_variation <- transfers %>%
  group_by(donor_cell) %>%
  summarise(
    n_total   = n(),
    n_draft   = sum(draft_induced_vacancy == 1),
    n_natural = sum(draft_induced_vacancy == 0),
    has_both  = n_draft > 0 & n_natural > 0,
    .groups = "drop"
  )

cat("Donor cells (unit × year) with 2+ transfers:", sum(cell_variation$n_total >= 2), "\n")
cat("Donor cells with mixed destination types:   ", sum(cell_variation$has_both), "\n\n")

# ============================================================
# 4. CAREER OUTCOMES
# ============================================================

worker_outcomes <- df %>%
  group_by(staff_id) %>%
  summarise(
    max_rank   = max(pos_rank, na.rm = TRUE),
    last_year  = max(year_num),
    first_year = min(year_num),
    .groups = "drop"
  )

postwar_outcomes <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(
    postwar_years = n_distinct(year_num),
    .groups = "drop"
  )

# ============================================================
# 5. ANALYSIS DATASET
# ============================================================

analysis <- transfers %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  mutate(
    years_after         = last_year - year_num,
    postwar_survived    = as.integer(!is.na(postwar_years) & postwar_years > 0),
    postwar_years       = replace_na(postwar_years, 0L),
    rank_gain           = max_rank - lag_rank,
    tenure_at_transfer  = year_num - first_year
  ) %>%
  filter(!is.na(donor_cell), !is.na(lag_rank))

cat("Analysis sample:", nrow(analysis), "transfers\n\n")

# ============================================================
# 6. BALANCE TEST
# ============================================================

cat("=== BALANCE TEST: Pre-transfer characteristics ===\n")
for (v in c("lag_rank", "tenure_at_transfer", "is_female")) {
  b <- feols(as.formula(paste(v, "~ draft_induced_vacancy | donor_cell")),
             data = analysis, cluster = ~donor_cell)
  cat(sprintf("  %-25s  coef = %7.4f  se = %.4f\n",
              v, coef(b)["draft_induced_vacancy"], se(b)["draft_induced_vacancy"]))
}

# ============================================================
# 7. MAIN REGRESSIONS
# ============================================================

cat("\n=== MAIN REGRESSIONS ===\n")
cat("Outcome ~ DraftInduced | donor_cell + dest_pos_norm, clustered by donor_cell\n\n")

r1 <- feols(years_after ~ draft_induced_vacancy + lag_rank + tenure_at_transfer + is_female |
              donor_cell + lag_pos,
            data = analysis, cluster = ~donor_cell)

r2 <- feols(postwar_survived ~ draft_induced_vacancy + lag_rank + tenure_at_transfer + is_female |
              donor_cell + lag_pos,
            data = analysis, cluster = ~donor_cell)

r3 <- feols(postwar_years ~ draft_induced_vacancy + lag_rank + tenure_at_transfer + is_female |
              donor_cell + lag_pos,
            data = analysis, cluster = ~donor_cell)

r4 <- feols(rank_gain ~ draft_induced_vacancy + lag_rank + tenure_at_transfer + is_female |
              donor_cell + lag_pos,
            data = analysis %>% filter(!is.na(rank_gain)), cluster = ~donor_cell)

etable(r1, r2, r3, r4,
       se.below = TRUE, fitstat = ~n + r2 + G,
       headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"),
       dict = c(draft_induced_vacancy = "Draft-Induced Vacancy (=1)",
                lag_rank = "Pre-transfer Rank",
                tenure_at_transfer = "Tenure at Transfer",
                is_female = "Female (=1)",
                donor_cell = "Donor Unit × Year",
                lag_pos = "Donor Position"))

# ============================================================
# 8. EXPORT LaTeX TABLE
# ============================================================

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end   <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else { tex_raw }
}

clean_depvar <- function(x) x[-grep("Dependent Var", x)]

tex_main <- etable(r1, r2, r3, r4,
                   dict = c(draft_induced_vacancy = "Draft-Induced Vacancy (=1)",
                             lag_rank = "Pre-transfer Rank",
                             tenure_at_transfer = "Tenure at Transfer",
                             is_female = "Female (=1)",
                             donor_cell = "Donor Unit $\\times$ Year",
                             lag_pos = "Donor Position"),
                   headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"),
                   se.below = TRUE, fitstat = ~n + r2 + G,
                   tex = TRUE)

tc <- clean_depvar(extract_tabular(tex_main))

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Career Trajectories and Draft-Induced Vacancies}",
  "\\label{tab:career-draft-vacancy}",
  "\\begin{threeparttable}",
  "",
  tc,
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regressions at the transfer level. ",
    "The sample consists of workers transferred out of the same occupational role unit ",
    "(donor office $\\times$ section $\\times$ position) in the same year. ",
    "``Draft-Induced Vacancy'' equals one if the destination office $\\times$ section ",
    "had at least one conscripted worker in that year, and zero otherwise. ",
    "``Tenure After'' = years observed in the records after the transfer year. ",
    "``Postwar Surv.'' = 1 if worker appears in any year 1947--1955. ",
    "``Postwar Yrs'' = number of years observed 1947--1955. ",
    "``Rank Gain'' = maximum rank achieved minus pre-transfer rank. ",
    "All specifications control for pre-transfer rank, tenure at transfer, and gender. ",
    "Fixed effects: donor occupational unit $\\times$ year and donor position title. ",
    "Standard errors clustered at the donor unit $\\times$ year level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("CareerTrajectories_DraftVacancy.tex"))
cat("\nTable exported to CareerTrajectories_DraftVacancy.tex\n")
