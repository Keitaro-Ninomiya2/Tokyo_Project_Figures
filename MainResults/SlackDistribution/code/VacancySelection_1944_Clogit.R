################################################################################
# Vacancy-event conditional logit for 1944 transfer sourcing.
#
# Unit: one actual transfer into a draft-impacted destination section-year
#       defines one event/stratum. Candidate origins are offices with at least
#       one matched-position non-drafted worker in t-1.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(broom)
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
prewar_years <- 1934:1937

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "技") ~ "engineer",
    str_detect(pos, "雇|傭|囑託") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "係長") ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$") ~ 1L,
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

cat("Loading data...\n")

df_names_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names_raw <- df_names_raw %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_all_raw <- df_all_raw %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- apply_1944_tokyoto_backfill(df_names_raw, ka_group_raw)
df_all <- apply_1944_tokyoto_backfill(df_all_raw, ka_group_raw)

office_initial_year <- df_names %>% group_by(office_id) %>% summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df_names %>% group_by(staff_id) %>% summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_norm_kyoku = norm_kyoku_filled,
    lag_kyoku_group = kyoku_group_filled,
    lag_ka = ka,
    lag_ka_group = ka_group_filled,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_occupation = occupation,
    lag_section_id = section_id_filled,
    lag_is_female = is_female
  ) %>%
  mutate(year_num = year_num + 1L)

current_status <- df_all %>%
  filter(year_num == analysis_year) %>%
  transmute(
    staff_id, year_num,
    current_drafted = drafted,
    current_observed = 1L
  )

worker_arrivals_1944 <- df_names %>%
  filter(year_num == analysis_year) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(year_num == office_first_year ~ NA, TRUE ~ year_num == first_year),
    arrival_type = arrival_distance(
      kyoku_group_filled, ka_group_filled, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id
  )

drafted_profiles_1944 <- df_all %>%
  filter(year_num == analysis_year, drafted == TRUE, !is_female,
         !is.na(section_id_filled), !is.na(pos_norm), !is.na(occupation), !is.na(pos_rank)) %>%
  group_by(
    section_id_filled, office_id, year_num, ka, ka_group_filled,
    norm_kyoku_filled, kyoku_group_filled, pos_norm, occupation, pos_rank
  ) %>%
  summarise(n_drafted_profile = n(), .groups = "drop")

drafted_section_totals_1944 <- drafted_profiles_1944 %>%
  group_by(section_id_filled, office_id, year_num, ka, ka_group_filled,
           norm_kyoku_filled, kyoku_group_filled) %>%
  summarise(n_drafted_male = sum(n_drafted_profile), .groups = "drop")

actual_selected_events <- worker_arrivals_1944 %>%
  filter(is_transfer_in, !is.na(lag_office_id), !is.na(pos_norm), !is.na(occupation), !is.na(pos_rank)) %>%
  inner_join(
    drafted_section_totals_1944 %>% select(section_id_filled, year_num),
    by = c("section_id_filled", "year_num")
  ) %>%
  transmute(
    event_id = paste0("evt_", row_number()),
    selected_staff_id = staff_id,
    year_num,
    dest_office_id = office_id,
    dest_section_id = section_id_filled,
    dest_ka = ka,
    dest_ka_group = ka_group_filled,
    dest_norm_kyoku = norm_kyoku_filled,
    dest_kyoku_group = kyoku_group_filled,
    dest_pos_norm = pos_norm,
    dest_occupation = occupation,
    dest_pos_rank = pos_rank,
    chosen_origin_office_id = lag_office_id
  )

cat("Actual selected transfer events into draft destinations:", nrow(actual_selected_events), "\n")

section_hiring_lag_1944 <- df_names %>%
  filter(year_num == analysis_year - 1L) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(is_new_hire = case_when(year_num == office_first_year ~ NA, TRUE ~ year_num == first_year)) %>%
  group_by(section_id_filled, office_id, year_num) %>%
  summarise(origin_prior_hiring = sum(is_new_hire, na.rm = TRUE), .groups = "drop") %>%
  transmute(origin_section_id = section_id_filled, origin_office_id = office_id, year_num = year_num + 1L, origin_prior_hiring)

candidate_workers_1944 <- staff_lag %>%
  filter(year_num == analysis_year, !is.na(lag_office_id), !is.na(lag_occupation), !is.na(lag_pos_rank), lag_occupation != "engineer") %>%
  left_join(current_status, by = c("staff_id", "year_num")) %>%
  left_join(section_hiring_lag_1944, by = c("lag_section_id" = "origin_section_id", "lag_office_id" = "origin_office_id", "year_num")) %>%
  mutate(origin_prior_hiring = replace_na(origin_prior_hiring, 0L)) %>%
  filter(current_observed == 1L, is.na(current_drafted) | current_drafted != TRUE, origin_prior_hiring > 0)

prewar_office_ties <- worker_arrivals_1944 %>% 
  select(NULL) # placeholder

prewar_arrivals <- df_names %>%
  filter(year_num %in% prewar_years) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id)

prewar_office_ties <- prewar_arrivals %>%
  filter(is_transfer_in, !is.na(lag_office_id), !is.na(office_id)) %>%
  count(origin_office_id = lag_office_id, dest_office_id = office_id, name = "prewar_flow_count")

choice_rows <- actual_selected_events %>%
  inner_join(
    candidate_workers_1944,
    by = c(
      "year_num" = "year_num",
      "dest_pos_norm" = "lag_pos_norm",
      "dest_occupation" = "lag_occupation"
    ),
    relationship = "many-to-many"
  ) %>%
  filter(
    lag_office_id != dest_office_id,
    abs(lag_pos_rank - dest_pos_rank) <= 1
  ) %>%
  mutate(
    distance_cat = arrival_distance(
      dest_kyoku_group, dest_ka_group, dest_ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    chosen = as.integer(lag_office_id == chosen_origin_office_id)
  ) %>%
  filter(!is.na(distance_cat))

choice_office_level <- choice_rows %>%
  group_by(
    event_id, year_num, dest_office_id, dest_section_id, dest_norm_kyoku, dest_kyoku_group,
    dest_ka, dest_ka_group, dest_pos_norm, dest_occupation, dest_pos_rank,
    chosen_origin_office_id, origin_office_id = lag_office_id
  ) %>%
  summarise(
    chosen = max(chosen),
    distance_cat = first(distance_cat),
    origin_matched_workers = n_distinct(staff_id),
    origin_max_prior_hiring = max(origin_prior_hiring, na.rm = TRUE),
    origin_kyoku_group = first(lag_kyoku_group),
    origin_ka_group = first(lag_ka_group),
    .groups = "drop"
  ) %>%
  left_join(prewar_office_ties, by = c("origin_office_id", "dest_office_id")) %>%
  mutate(
    prewar_tie = as.integer(replace_na(prewar_flow_count, 0) > 0),
    same_section = as.integer(distance_cat == "same_section"),
    same_department = as.integer(distance_cat == "same_department"),
    log_origin_matched_workers = log(origin_matched_workers + 1),
    log_origin_prior_hiring = log(origin_max_prior_hiring + 1)
  )

choice_office_level <- choice_office_level %>%
  group_by(event_id) %>%
  filter(sum(chosen) == 1L, n() >= 2L) %>%
  ungroup()

event_summary <- choice_office_level %>%
  group_by(event_id) %>%
  summarise(
    n_candidates = n(),
    chosen_distance = first(distance_cat[chosen == 1]),
    .groups = "drop"
  )

cat("Usable choice events:", nrow(event_summary), "\n")
cat("Median candidates per event:", median(event_summary$n_candidates), "\n")

clogit_mod <- clogit(
  chosen ~ same_section + same_department + log_origin_matched_workers +
    log_origin_prior_hiring + prewar_tie + strata(event_id),
  data = choice_office_level
)

results <- tidy(clogit_mod, exponentiate = FALSE)

write_csv(choice_office_level, file.path(result_dir, "vacancy_selection_1944_choice_dataset.csv"))
write_csv(event_summary, file.path(result_dir, "vacancy_selection_1944_event_summary.csv"))
write_csv(results, file.path(result_dir, "vacancy_selection_1944_clogit_results.csv"))

cat("\nConditional logit results:\n")
print(results, n = Inf)
