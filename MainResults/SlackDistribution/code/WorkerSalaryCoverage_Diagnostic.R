################################################################################
# Diagnose salary coverage for the 1944 worker-level within-origin clogit.
#
# Questions:
# 1. Does the master panel have salary populated for 1944 workers at all?
# 2. Is salary coverage concentrated in pre-merger TokyoShi/TokyoFu years?
# 3. Is salary being lost in the worker-choice construction, or is it absent
#    upstream for the relevant 1944 candidate pool?
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")
reference_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "reference_tables")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")
BASE_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_ka_to_kyoku.csv")
FACILITY_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_facility_to_kyoku.csv")
KAKARI_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_kakari_to_kyoku.csv")

analysis_year <- 1944L

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "æŠ€") ~ "engineer",
    str_detect(pos, "é›‡|å‚­|å›‘è¨—") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^ä¸»äº‹$|^æŠ€å¸«$") ~ 3L,
    yr < 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "ä¿‚é•·") ~ 3L,
    yr >= 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$") ~ 1L,
    yr >= 1948 ~ 2L
  )
}

parse_court_rank <- function(r) {
  case_when(
    is.na(r) ~ NA_integer_,
    str_detect(r, "^æ­£å…«") ~ 1L,
    str_detect(r, "^æ­£ä¸ƒ") ~ 2L,
    str_detect(r, "^æ­£å…­") ~ 3L,
    str_detect(r, "^æ­£äº”") ~ 4L,
    str_detect(r, "^æ­£å››") ~ 5L,
    str_detect(r, "^æ­£ä¸‰") ~ 6L,
    str_detect(r, "^æ­£äºŒ") ~ 7L,
    str_detect(r, "^æ­£ä¸€") ~ 8L,
    str_detect(r, "^æ­£å") ~ 0L,
    TRUE ~ NA_integer_
  )
}

parse_decoration <- function(d) {
  case_when(
    is.na(d) ~ NA_integer_,
    str_detect(d, "^å‹²å…«|^å‹²å") ~ 1L,
    str_detect(d, "^å‹²ä¸ƒ") ~ 2L,
    str_detect(d, "^å‹²å…­") ~ 3L,
    str_detect(d, "^å‹²äº”") ~ 4L,
    str_detect(d, "^å‹²å››") ~ 5L,
    str_detect(d, "^å‹²ä¸‰") ~ 6L,
    str_detect(d, "^å‹²äºŒ") ~ 7L,
    str_detect(d, "^å‹²ä¸€") ~ 8L,
    TRUE ~ NA_integer_
  )
}

parse_salary <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c("ã€‡" = "0", "ä¸€" = "1", "äºŒ" = "2", "ä¸‰" = "3", "å››" = "4",
             "äº”" = "5", "å…­" = "6", "ä¸ƒ" = "7", "å…«" = "8", "ä¹" = "9")
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }

  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    is_annual <- str_detect(x, "^å¹´")
    cleaned <- str_remove(x, "^[æœˆå¹´]")
    if (nchar(cleaned) == 0) return(NA_real_)
    chars <- strsplit(cleaned, "")[[1]]
    digits <- sapply(chars, kanji_to_digit)
    if (any(is.na(digits))) return(NA_real_)
    val <- as.numeric(paste(digits, collapse = ""))
    if (is_annual) val <- val / 12
    val
  }

  sapply(s, parse_one, USE.NAMES = FALSE)
}

section_key <- function(office_id, ka_group, ka_name) {
  if_else(
    !is.na(ka_group),
    paste0(office_id, "_grp_", ka_group),
    paste0(office_id, "_name_", replace_na(ka_name, "missing"))
  )
}

arrival_distance <- function(dest_kyoku_group, dest_ka_group, dest_ka,
                             origin_kyoku_group, origin_ka_group, origin_ka) {
  case_when(
    !is.na(dest_ka_group) & !is.na(origin_ka_group) & dest_ka_group == origin_ka_group ~ "same_section",
    (is.na(dest_ka_group) | is.na(origin_ka_group)) &
      !is.na(dest_kyoku_group) & !is.na(origin_kyoku_group) &
      dest_kyoku_group == origin_kyoku_group &
      !is.na(dest_ka) & !is.na(origin_ka) &
      dest_ka == origin_ka ~ "same_section",
    !is.na(dest_kyoku_group) & !is.na(origin_kyoku_group) &
      dest_kyoku_group == origin_kyoku_group ~ "same_department",
    !is.na(dest_kyoku_group) & !is.na(origin_kyoku_group) &
      dest_kyoku_group != origin_kyoku_group ~ "different_department",
    TRUE ~ NA_character_
  )
}

apply_1944_tokyoto_backfill <- function(df, ka_group_raw) {
  base_ref <- read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
    filter(as.Date(effective_start) <= as.Date("1944-12-31"),
           as.Date(effective_end) >= as.Date("1944-01-01")) %>%
    count(ka, name = "base_candidate_count") %>%
    left_join(
      read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
        filter(as.Date(effective_start) <= as.Date("1944-12-31"),
               as.Date(effective_end) >= as.Date("1944-01-01")) %>%
        group_by(ka) %>% filter(n() == 1L) %>% ungroup() %>%
        transmute(ka, base_ref_kyoku = kyoku),
      by = "ka"
    )
  facility_ref <- read_csv(FACILITY_REF_PATH, show_col_types = FALSE) %>%
    transmute(ka = label, facility_ref_kyoku = kyoku)
  kakari_ref <- read_csv(KAKARI_REF_PATH, show_col_types = FALSE) %>%
    transmute(kakari = label, kakari_ref_kyoku = kyoku)
  adjacent_staff_kyoku <- df %>%
    filter(year_num %in% c(analysis_year - 1L, analysis_year + 1L), !is.na(norm_kyoku)) %>%
    group_by(staff_id) %>%
    summarise(adjacent_known_kyoku_n = n_distinct(norm_kyoku),
              adjacent_known_kyoku = if_else(n_distinct(norm_kyoku) == 1L, first(norm_kyoku), NA_character_),
              .groups = "drop")
  base_candidates_long <- read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
    filter(as.Date(effective_start) <= as.Date("1944-12-31"),
           as.Date(effective_end) >= as.Date("1944-01-01")) %>%
    select(ka, candidate_kyoku = kyoku)

  df2 <- df %>%
    left_join(base_ref, by = "ka") %>%
    left_join(facility_ref, by = "ka") %>%
    left_join(kakari_ref, by = "kakari") %>%
    left_join(adjacent_staff_kyoku, by = "staff_id")

  ambiguous_rows <- df2 %>%
    filter(year_num == analysis_year, (is.na(kyoku) | str_squish(replace_na(kyoku, "")) == ""),
           !is.na(ka), replace_na(base_candidate_count, 0L) > 1L, !is.na(adjacent_known_kyoku)) %>%
    select(staff_id, ka, adjacent_known_kyoku) %>% distinct() %>%
    inner_join(base_candidates_long, by = "ka", relationship = "many-to-many") %>%
    filter(adjacent_known_kyoku == candidate_kyoku) %>%
    group_by(staff_id, ka) %>%
    summarise(history_match_kyoku = if_else(n() == 1L, first(candidate_kyoku), NA_character_), .groups = "drop")

  df2 %>%
    left_join(ambiguous_rows, by = c("staff_id", "ka")) %>%
    mutate(
      kyoku_backfill = case_when(
        year_num != analysis_year ~ kyoku,
        !is.na(kyoku) & str_squish(kyoku) != "" ~ kyoku,
        !is.na(base_ref_kyoku) ~ base_ref_kyoku,
        !is.na(facility_ref_kyoku) ~ facility_ref_kyoku,
        !is.na(history_match_kyoku) ~ history_match_kyoku,
        !is.na(kakari_ref_kyoku) ~ kakari_ref_kyoku,
        TRUE ~ NA_character_
      ),
      norm_kyoku_filled = kyoku_backfill,
      kyoku_group_filled = assign_kyoku_group(norm_kyoku_filled),
      kyoku_clean_filled = replace_na(norm_kyoku_filled, ""),
      ka_clean_filled = replace_na(ka, "")
    ) %>%
    left_join(
      ka_group_raw %>%
        rename(ka_group_lookup = ka_group, kyoku_clean_filled = kyoku, ka_clean_filled = ka, year_num = year),
      by = c("year_num", "kyoku_clean_filled", "ka_clean_filled")
    ) %>%
    mutate(
      ka_group_filled = if_else(year_num == analysis_year, ka_group_lookup, ka_group),
      section_id_filled = section_key(office_id, ka_group_filled, ka)
    ) %>%
    select(-ka_group_lookup)
}

cat("Loading master panel...\n")

df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num),
    court_rank = parse_court_rank(rank),
    decor_rank = parse_decoration(decoration),
    salary_num = parse_salary(salary),
    raw_salary_present = !is.na(salary) & str_squish(salary) != "",
    parsed_salary_present = !is.na(salary_num) & salary_num > 0
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

year_gov_salary_summary <- df_raw %>%
  filter(is_name == TRUE) %>%
  group_by(year_num, gov_level) %>%
  summarise(
    n_workers = n(),
    share_raw_salary_present = mean(raw_salary_present),
    share_parsed_salary_present = mean(parsed_salary_present),
    .groups = "drop"
  ) %>%
  arrange(year_num, gov_level)

salary_1944_summary <- df_raw %>%
  filter(is_name == TRUE, year_num == analysis_year) %>%
  group_by(gov_level) %>%
  summarise(
    n_workers = n(),
    share_raw_salary_present = mean(raw_salary_present),
    share_parsed_salary_present = mean(parsed_salary_present),
    n_raw_salary_present = sum(raw_salary_present),
    n_parsed_salary_present = sum(parsed_salary_present),
    .groups = "drop"
  )

salary_1944_examples <- df_raw %>%
  filter(is_name == TRUE, year_num == analysis_year, raw_salary_present | parsed_salary_present) %>%
  select(staff_id, gov_level, office, kyoku, ka, kakari, position, salary, salary_num) %>%
  slice_head(n = 25)

cat("Rebuilding worker-choice source with salary...\n")

office_choice <- read_csv(
  file.path(result_dir, "vacancy_selection_1944_occ_rank_choice_dataset.csv"),
  show_col_types = FALSE
)

selected_origins <- office_choice %>%
  filter(chosen == 1) %>%
  transmute(
    event_id,
    year_num,
    dest_office_id,
    dest_section_id,
    dest_pos_norm,
    dest_occupation,
    dest_pos_rank,
    origin_office_id,
    chosen_distance = distance_cat,
    event_origin_id = paste(event_id, origin_office_id, sep = "||")
  )

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names_raw <- df_raw %>%
  filter(is_name == TRUE) %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_all_raw <- df_raw %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- apply_1944_tokyoto_backfill(df_names_raw, ka_group_raw)
df_all <- apply_1944_tokyoto_backfill(df_all_raw, ka_group_raw)

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_occupation = occupation,
    lag_is_female = gender_modern,
    lag_court_rank = court_rank,
    lag_decor_rank = decor_rank
  ) %>%
  mutate(year_num = year_num + 1L)

current_status <- df_all %>%
  filter(year_num == analysis_year) %>%
  transmute(
    staff_id, year_num,
    current_observed = 1L,
    current_drafted = drafted,
    current_salary = salary,
    current_salary_num = salary_num,
    raw_salary_present = raw_salary_present,
    parsed_salary_present = parsed_salary_present
  )

candidate_workers <- staff_lag %>%
  filter(year_num == analysis_year, !is.na(lag_office_id), !is.na(lag_occupation), !is.na(lag_pos_rank)) %>%
  left_join(current_status, by = c("staff_id", "year_num")) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    own_tenure = analysis_year - first_year
  ) %>%
  filter(current_observed == 1L, is.na(current_drafted) | current_drafted != TRUE)

candidate_salary_summary <- candidate_workers %>%
  summarise(
    n_candidates_total = n(),
    share_raw_salary_present = mean(raw_salary_present),
    share_parsed_salary_present = mean(parsed_salary_present)
  )

worker_choice_prejoin_summary <- selected_origins %>%
  inner_join(
    candidate_workers,
    by = c("year_num" = "year_num", "origin_office_id" = "lag_office_id"),
    relationship = "many-to-many"
  ) %>%
  filter(
    lag_occupation == dest_occupation,
    abs(lag_pos_rank - dest_pos_rank) <= 1
  ) %>%
  summarise(
    n_rows = n(),
    share_raw_salary_present = mean(raw_salary_present),
    share_parsed_salary_present = mean(parsed_salary_present)
  )

write_csv(year_gov_salary_summary, file.path(result_dir, "worker_salary_year_gov_summary.csv"))
write_csv(salary_1944_summary, file.path(result_dir, "worker_salary_1944_summary.csv"))
write_csv(salary_1944_examples, file.path(result_dir, "worker_salary_1944_examples.csv"))
write_csv(candidate_salary_summary, file.path(result_dir, "worker_salary_candidate_pool_summary.csv"))
write_csv(worker_choice_prejoin_summary, file.path(result_dir, "worker_salary_workerchoice_prejoin_summary.csv"))

cat("\nYear x gov salary summary (tail):\n")
print(year_gov_salary_summary %>% filter(year_num >= 1940), n = Inf)
cat("\n1944 salary summary:\n")
print(salary_1944_summary, n = Inf)
cat("\nCandidate pool salary summary:\n")
print(candidate_salary_summary)
cat("\nWorker-choice prejoin salary summary:\n")
print(worker_choice_prejoin_summary)
