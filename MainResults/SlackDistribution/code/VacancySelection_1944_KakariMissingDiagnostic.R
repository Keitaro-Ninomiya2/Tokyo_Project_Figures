################################################################################
# Diagnostic for missing kakari among selected same-section transfers in the
# refined 1944 office-level occ-rank selection sample.
#
# Goal:
# 1. Distinguish whether missingness is on destination side, origin side, or both
# 2. Check whether surrounding workers in the same destination/origin section-year
#    typically have kakari recorded, which helps separate row-specific missingness
#    from whole-section-level missingness
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

office_choice <- read_csv(
  file.path(result_dir, "vacancy_selection_1944_occ_rank_choice_dataset.csv"),
  show_col_types = FALSE
) %>%
  filter(chosen == 1, distance_cat == "same_section") %>%
  select(
    event_id, year_num, dest_office_id, dest_section_id, dest_pos_norm,
    dest_occupation, dest_pos_rank, origin_office_id
  )

df_names_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num),
    raw_kakari_present = !is.na(kakari) & str_squish(kakari) != ""
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names_raw <- df_names_raw %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- apply_1944_tokyoto_backfill(df_names_raw, ka_group_raw)

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_kyoku_group = kyoku_group_filled,
    lag_ka = ka,
    lag_ka_group = ka_group_filled,
    lag_kakari = kakari,
    lag_page = page,
    lag_raw_kakari_present = raw_kakari_present,
    lag_section_id = section_id_filled
  ) %>%
  mutate(year_num = year_num + 1L)

worker_arrivals_1944 <- df_names %>%
  filter(year_num == analysis_year) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    arrival_type = arrival_distance(
      kyoku_group_filled, ka_group_filled, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id,
    dest_raw_kakari_present = raw_kakari_present,
    dest_page = page
  )

selected_workers <- worker_arrivals_1944 %>%
  filter(is_transfer_in) %>%
  inner_join(
    office_choice,
    by = c(
      "year_num",
      "office_id" = "dest_office_id",
      "section_id_filled" = "dest_section_id",
      "pos_norm" = "dest_pos_norm",
      "occupation" = "dest_occupation",
      "pos_rank" = "dest_pos_rank",
      "lag_office_id" = "origin_office_id"
    ),
    relationship = "many-to-many"
  ) %>%
  mutate(
    side_missingness = case_when(
      dest_raw_kakari_present & lag_raw_kakari_present ~ "both_present",
      dest_raw_kakari_present & !lag_raw_kakari_present ~ "origin_missing_only",
      !dest_raw_kakari_present & lag_raw_kakari_present ~ "destination_missing_only",
      TRUE ~ "both_missing"
    )
  )

dest_section_context <- df_names %>%
  filter(year_num == analysis_year) %>%
  group_by(office_id, section_id_filled) %>%
  summarise(
    dest_section_workers = n(),
    dest_section_kakari_present_share = mean(raw_kakari_present),
    dest_section_nonmissing_count = sum(raw_kakari_present),
    .groups = "drop"
  )

origin_section_context <- df_names %>%
  filter(year_num == analysis_year - 1L) %>%
  group_by(office_id, section_id_filled) %>%
  summarise(
    origin_section_workers = n(),
    origin_section_kakari_present_share = mean(raw_kakari_present),
    origin_section_nonmissing_count = sum(raw_kakari_present),
    .groups = "drop"
  )

selected_workers_with_context <- selected_workers %>%
  left_join(
    dest_section_context,
    by = c("office_id", "section_id_filled")
  ) %>%
  left_join(
    origin_section_context,
    by = c("lag_office_id" = "office_id", "lag_section_id" = "section_id_filled")
  ) %>%
  mutate(
    dest_context_flag = case_when(
      is.na(dest_section_kakari_present_share) ~ "dest_context_missing",
      dest_section_nonmissing_count == 0 ~ "dest_section_all_missing",
      dest_section_kakari_present_share < 0.5 ~ "dest_section_partial",
      TRUE ~ "dest_section_mostly_present"
    ),
    origin_context_flag = case_when(
      is.na(origin_section_kakari_present_share) ~ "origin_context_missing",
      origin_section_nonmissing_count == 0 ~ "origin_section_all_missing",
      origin_section_kakari_present_share < 0.5 ~ "origin_section_partial",
      TRUE ~ "origin_section_mostly_present"
    )
  )

side_summary <- selected_workers_with_context %>%
  count(side_missingness, name = "n_selected_workers") %>%
  mutate(share = n_selected_workers / sum(n_selected_workers))

event_side_summary <- selected_workers_with_context %>%
  group_by(event_id) %>%
  summarise(
    any_both_present = any(side_missingness == "both_present"),
    any_origin_missing_only = any(side_missingness == "origin_missing_only"),
    any_destination_missing_only = any(side_missingness == "destination_missing_only"),
    all_both_missing = all(side_missingness == "both_missing"),
    .groups = "drop"
  ) %>%
  summarise(
    n_same_section_events = n(),
    share_any_both_present = mean(any_both_present),
    share_any_origin_missing_only = mean(any_origin_missing_only),
    share_any_destination_missing_only = mean(any_destination_missing_only),
    share_all_both_missing = mean(all_both_missing)
  )

context_summary <- selected_workers_with_context %>%
  count(dest_context_flag, origin_context_flag, name = "n_selected_workers") %>%
  arrange(desc(n_selected_workers))

recovery_feasibility <- selected_workers_with_context %>%
  summarise(
    n_selected_workers = n(),
    n_events = n_distinct(event_id),
    share_dest_raw_kakari_present = mean(dest_raw_kakari_present),
    share_origin_raw_kakari_present = mean(lag_raw_kakari_present),
    share_dest_section_has_any_kakari = mean(replace_na(dest_section_nonmissing_count, 0) > 0),
    share_origin_section_has_any_kakari = mean(replace_na(origin_section_nonmissing_count, 0) > 0),
    share_dest_section_mostly_present = mean(dest_context_flag == "dest_section_mostly_present"),
    share_origin_section_mostly_present = mean(origin_context_flag == "origin_section_mostly_present")
  )

sample_rows <- selected_workers_with_context %>%
  transmute(
    event_id,
    staff_id,
    dest_office_id = office_id,
    dest_section_id = section_id_filled,
    dest_ka = ka,
    dest_kakari = kakari,
    dest_raw_kakari_present,
    dest_page,
    origin_office_id = lag_office_id,
    origin_section_id = lag_section_id,
    origin_ka = lag_ka,
    origin_kakari = lag_kakari,
    lag_raw_kakari_present,
    lag_page,
    side_missingness,
    dest_section_kakari_present_share,
    origin_section_kakari_present_share,
    dest_context_flag,
    origin_context_flag
  ) %>%
  arrange(event_id, staff_id)

write_csv(side_summary, file.path(result_dir, "vacancy_selection_1944_kakari_missing_side_summary.csv"))
write_csv(event_side_summary, file.path(result_dir, "vacancy_selection_1944_kakari_missing_event_summary.csv"))
write_csv(context_summary, file.path(result_dir, "vacancy_selection_1944_kakari_missing_context_summary.csv"))
write_csv(recovery_feasibility, file.path(result_dir, "vacancy_selection_1944_kakari_missing_recovery_feasibility.csv"))
write_csv(sample_rows, file.path(result_dir, "vacancy_selection_1944_kakari_missing_sample_rows.csv"))

cat("Kakari missingness by side:\n")
print(side_summary, n = Inf)
cat("\nEvent-level missingness summary:\n")
print(event_side_summary)
cat("\nContext summary:\n")
print(context_summary, n = 20)
cat("\nRecovery feasibility summary:\n")
print(recovery_feasibility)
