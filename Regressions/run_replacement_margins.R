################################################################################
# Replacement margins: how offices with more vacancies responded
# Shows: retention (same section) + internal transfers + external hires
# Sources: NewTable1c_TransferType.R + Table7_NewHires.R logic, combined
################################################################################

suppressMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

KA_GROUP_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Documents", "GitHub", "Tokyo_Project_Figures",
  "Regressions", "ka_group_map.csv"
)

cat("Loading data...\n")
stopifnot(file.exists(DATA_PATH))
stopifnot(file.exists(KA_GROUP_PATH))

df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

df <- df_raw %>%
  filter(is_name == TRUE) %>%
  mutate(year_num  = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm  = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- df_raw %>%
  mutate(year_num  = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm  = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945

cat("Workers (is_name, distinct staff_id x year):", nrow(df), "\n")
cat("Drafted (wartime):", sum(df$drafted & df$year_num %in% years_of_interest), "\n\n")

# ============================================================
# KYOKU NORMALIZATION + GROUP CROSSWALK
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
    str_detect(k, "計[晝画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醍院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("電気局", "交通局")             ~ "transport",
    nk == "水道局"                             ~ "water",
    nk == "港湾局"                             ~ "port",
    nk == "教育局"                             ~ "education",
    nk %in% c("土木局", "計画局", "経理局",
              "建築局", "道路局")               ~ "infrastructure",
    nk %in% c("戰時生活局", "経済局",
              "中央卸売市場", "産業局")         ~ "economy",
    nk %in% c("健民局", "厚生局", "社会局",
              "保健局", "民生局", "養育院",
              "労働局")                         ~ "welfare",
    nk %in% c("市会事務局", "府会事務局")      ~ "assembly",
    nk %in% c("財務局", "監査局", "長官官房")  ~ "finance_admin",
    nk == "復興事業局"                         ~ "reconstruction",
    nk == "市民局"                             ~ "citizen",
    nk == "防衛局"                             ~ "defense",
    TRUE ~ NA_character_
  )
}

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

df <- df %>%
  mutate(norm_kyoku  = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku))

# ============================================================
# KA GROUP MAP (worker-flow clusters, 40% threshold)
# ============================================================

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

cat("ka_group_map rows:", nrow(ka_group_raw), "\n")

df <- df %>%
  mutate(kyoku_clean = replace_na(kyoku, ""),
         ka_clean    = replace_na(ka, "")) %>%
  left_join(ka_group_raw, by = c("year_num" = "year",
                                  "kyoku_clean" = "kyoku",
                                  "ka_clean"    = "ka"))

cat("Workers with ka_group:", sum(!is.na(df$ka_group)), "/", nrow(df), "\n\n")

# ============================================================
# CUMULATIVE MALE BASELINE
# ============================================================

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# ============================================================
# STAFF TRANSITIONS
# ============================================================

office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
         lag_kyoku_group = kyoku_group,
         lag_ka          = ka,
         lag_ka_group    = ka_group) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year,    by = "staff_id") %>%
  left_join(staff_lag,           by = c("staff_id", "year_num")) %>%
  mutate(
    # New hire: first year in dataset, excluding office's opening year
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    # Classify each worker by arrival channel
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group)         ~ NA_character_,
      # Same ka group (flow-based cluster) → retained / same-section
      !is.na(ka_group) & !is.na(lag_ka_group) &
        ka_group == lag_ka_group                           ~ "retention",
      # Fallback same-section: no ka_group but same raw ka within same kyoku
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka        ~ "retention",
      # Same kyoku, different ka → within-bureau transfer
      lag_kyoku_group == kyoku_group                       ~ "transfer_same_dept",
      # Different kyoku → cross-bureau transfer
      lag_kyoku_group != kyoku_group                       ~ "transfer_diff_dept",
      TRUE ~ NA_character_
    ),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

cat("Arrival type distribution (wartime):\n")
staff_transitions %>%
  filter(!is.na(arrival_type)) %>%
  count(arrival_type, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()
cat("\n")

# ============================================================
# AGGREGATE TO POSITION x KAKARI x YEAR
# ============================================================

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention          = sum(arrival_type == "retention",          na.rm = TRUE),
    n_transfer_same_dept = sum(arrival_type == "transfer_same_dept", na.rm = TRUE),
    n_transfer_diff_dept = sum(arrival_type == "transfer_diff_dept", na.rm = TRUE),
    n_new_hires          = sum(is_new_hire,                          na.rm = TRUE),
    n_workers            = n(),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept)

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts,  by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    ka_id       = if_else(!is.na(ka) & !is.na(kyoku),
                          paste(kyoku, ka, sep = "_"), NA_character_),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

panel_ka <- panel %>% filter(!is.na(ka_id))

cat("Panel (pos x kakari x year, with ka_id):", nrow(panel_ka), "obs\n")
cat("Unique offices:", n_distinct(panel_ka$office_id), "\n\n")

cat("Mean outcomes per cell:\n")
panel_ka %>%
  summarise(
    across(c(n_drafted_male, n_retention, n_transfer_same_dept,
             n_transfer_diff_dept, n_internal_total, n_new_hires), mean)
  ) %>%
  pivot_longer(everything()) %>%
  mutate(value = round(value, 4)) %>%
  print()
cat("\n")

# ============================================================
# REGRESSIONS
# All: OLS, FE = year + ka + position, cluster = office_id
# Treatment: n_drafted_male (count of male draftees from same cell)
# Control: log(cumul_n_male + 1)
# ============================================================

fe_formula <- function(dv) {
  as.formula(paste0(dv, " ~ n_drafted_male + log(cumul_n_male + 1) | ",
                    "year_num + ka_id + pos_norm"))
}

cat("=================================================================\n")
cat("REPLACEMENT MARGINS: per additional male drafted worker\n")
cat("OLS | FE: year + section + position | Cluster: office\n")
cat("=================================================================\n\n")

m_retention  <- feols(fe_formula("n_retention"),          data = panel_ka, cluster = ~office_id)
m_same_dept  <- feols(fe_formula("n_transfer_same_dept"), data = panel_ka, cluster = ~office_id)
m_diff_dept  <- feols(fe_formula("n_transfer_diff_dept"), data = panel_ka, cluster = ~office_id)
m_internal   <- feols(fe_formula("n_internal_total"),     data = panel_ka, cluster = ~office_id)
m_new_hires  <- feols(fe_formula("n_new_hires"),          data = panel_ka, cluster = ~office_id)

etable(
  m_retention, m_same_dept, m_diff_dept, m_internal, m_new_hires,
  dict    = c(n_drafted_male = "No. drafted workers"),
  order   = "No. drafted",
  drop    = "log",
  headers = c("Retention\n(same section)",
              "Transfer\n(same dept.)",
              "Transfer\n(diff. dept.)",
              "Total\ninternal",
              "New\nhires"),
  se.below  = TRUE,
  fitstat   = ~n + r2 + G
)

cat("\n=== RATIO SUMMARY ===\n")
get_coef <- function(m) unname(coef(m)["n_drafted_male"])

b_retention <- get_coef(m_retention)
b_same_dept <- get_coef(m_same_dept)
b_diff_dept <- get_coef(m_diff_dept)
b_total_int <- get_coef(m_internal)
b_new_hires <- get_coef(m_new_hires)

cat(sprintf("  Retention (same section):     %6.4f\n", b_retention))
cat(sprintf("  Transfer (same dept.):        %6.4f\n", b_same_dept))
cat(sprintf("  Transfer (diff. dept.):       %6.4f\n", b_diff_dept))
cat(sprintf("  Total internal:               %6.4f\n", b_total_int))
cat(sprintf("  New hires (external):         %6.4f\n", b_new_hires))
cat(sprintf("  Internal / External ratio:    %6.3f  (~2:1 if ~2)\n",
    b_total_int / b_new_hires))
cat(sprintf("  Retention share of internal:  %5.1f%%\n",
    100 * b_retention / b_total_int))
cat(sprintf("  Transfer share of internal:   %5.1f%%\n",
    100 * (b_same_dept + b_diff_dept) / b_total_int))
