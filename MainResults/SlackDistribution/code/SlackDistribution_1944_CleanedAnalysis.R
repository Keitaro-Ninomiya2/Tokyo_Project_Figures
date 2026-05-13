################################################################################
# 1944 cleaned-sample analysis after TokyoTo destination-side kyoku backfill.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
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

fmt_num <- function(x, digits = 4) sprintf(paste0("%.", digits, "f"), x)

apply_1944_tokyoto_backfill <- function(df, ka_group_raw) {
  base_ref <- read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
    filter(as.Date(effective_start) <= as.Date("1944-12-31"),
           as.Date(effective_end) >= as.Date("1944-01-01")) %>%
    count(ka, name = "base_candidate_count") %>%
    left_join(
      read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
        filter(as.Date(effective_start) <= as.Date("1944-12-31"),
               as.Date(effective_end) >= as.Date("1944-01-01")) %>%
        group_by(ka) %>%
        filter(n() == 1L) %>%
        ungroup() %>%
        transmute(ka, base_ref_kyoku = kyoku),
      by = "ka"
    )

  facility_ref <- read_csv(FACILITY_REF_PATH, show_col_types = FALSE) %>%
    transmute(ka = label, facility_ref_kyoku = kyoku)

  kakari_ref <- read_csv(KAKARI_REF_PATH, show_col_types = FALSE) %>%
    transmute(kakari = label, kakari_ref_kyoku = kyoku)

  adjacent_staff_kyoku <- df %>%
    filter(year_num %in% c(analysis_year - 1L, analysis_year + 1L),
           !is.na(norm_kyoku)) %>%
    group_by(staff_id) %>%
    summarise(
      adjacent_known_kyoku_n = n_distinct(norm_kyoku),
      adjacent_known_kyoku = if_else(n_distinct(norm_kyoku) == 1L, first(norm_kyoku), NA_character_),
      .groups = "drop"
    )

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
    filter(year_num == analysis_year,
           (is.na(kyoku) | str_squish(replace_na(kyoku, "")) == ""),
           !is.na(ka),
           replace_na(base_candidate_count, 0L) > 1L,
           !is.na(adjacent_known_kyoku)) %>%
    select(staff_id, ka, adjacent_known_kyoku) %>%
    distinct() %>%
    inner_join(base_candidates_long, by = "ka", relationship = "many-to-many") %>%
    filter(adjacent_known_kyoku == candidate_kyoku) %>%
    group_by(staff_id, ka) %>%
    summarise(
      history_match_n = n(),
      history_match_kyoku = if_else(n() == 1L, first(candidate_kyoku), NA_character_),
      .groups = "drop"
    )

  df2 <- df2 %>%
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
      kyoku_backfill_method = case_when(
        year_num != analysis_year ~ "original_non_1944",
        !is.na(kyoku) & str_squish(kyoku) != "" ~ "original",
        !is.na(base_ref_kyoku) ~ "base_ka_reference",
        !is.na(facility_ref_kyoku) ~ "facility_ka_reference",
        !is.na(history_match_kyoku) ~ "staff_history_tiebreak",
        !is.na(kakari_ref_kyoku) ~ "kakari_reference",
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

  df2
}

run_feols_1944 <- function(panel, outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ any_slack_same_section + log_slack_same_section + ",
      "log_slack_same_department + log_slack_different_department + ",
      "n_drafted_male + log_section_size | norm_kyoku_filled"
    )
  )
  feols(fml, data = panel, cluster = ~office_id)
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
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_all_raw <- df_all_raw %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- apply_1944_tokyoto_backfill(df_names_raw, ka_group_raw)
df_all <- apply_1944_tokyoto_backfill(df_all_raw, ka_group_raw)

dest_recovery_summary <- df_names_raw %>%
  filter(year_num == analysis_year) %>%
  mutate(dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == "") %>%
  select(staff_id, year_num, office_id, ka, kakari, dest_kyoku_missing) %>%
  left_join(
    df_names %>%
      filter(year_num == analysis_year) %>%
      transmute(staff_id, year_num, filled_kyoku = norm_kyoku_filled, backfill_method = kyoku_backfill_method),
    by = c("staff_id", "year_num")
  ) %>%
  filter(dest_kyoku_missing) %>%
  summarise(
    n_missing_original = n(),
    n_recovered = sum(!is.na(filled_kyoku)),
    recovery_rate = mean(!is.na(filled_kyoku))
  )

dest_recovery_by_method <- df_names_raw %>%
  filter(year_num == analysis_year) %>%
  mutate(dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == "") %>%
  select(staff_id, year_num, dest_kyoku_missing) %>%
  left_join(
    df_names %>%
      filter(year_num == analysis_year) %>%
      transmute(staff_id, year_num, filled_kyoku = norm_kyoku_filled, backfill_method = kyoku_backfill_method),
    by = c("staff_id", "year_num")
  ) %>%
  filter(dest_kyoku_missing, !is.na(filled_kyoku)) %>%
  count(backfill_method, sort = TRUE, name = "n_rows")

office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_gov_level = gov_level,
    lag_norm_kyoku = norm_kyoku_filled,
    lag_kyoku_group = kyoku_group_filled,
    lag_ka = ka,
    lag_ka_group = ka_group_filled,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_occupation = occupation,
    lag_section_id = section_id_filled
  ) %>%
  mutate(year_num = year_num + 1L)

current_status <- df_all %>%
  filter(year_num == analysis_year) %>%
  transmute(
    staff_id, year_num,
    current_office_id = office_id,
    current_section_id = section_id_filled,
    current_gov_level = gov_level,
    current_drafted = drafted,
    current_observed = 1L
  )

worker_arrivals_1944 <- df_names %>%
  filter(year_num == analysis_year) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    ),
    arrival_type = arrival_distance(
      kyoku_group_filled, ka_group_filled, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id
  )

worker_arrivals_1944_raw <- df_names_raw %>%
  filter(year_num == analysis_year) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(
    df_names_raw %>%
      select(
        staff_id, year_num,
        lag_office_id = office_id,
        lag_gov_level = gov_level,
        lag_norm_kyoku = norm_kyoku,
        lag_kyoku_group = kyoku_group,
        lag_ka = ka,
        lag_ka_group = ka_group
      ) %>%
      mutate(year_num = year_num + 1L),
    by = c("staff_id", "year_num")
  ) %>%
  mutate(
    arrival_type = arrival_distance(
      kyoku_group, ka_group, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id,
    dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == ""
  )

drafted_section_totals_1944 <- df_all %>%
  filter(
    year_num == analysis_year,
    drafted == TRUE,
    !is_female,
    !is.na(section_id_filled)
  ) %>%
  group_by(section_id_filled, year_num) %>%
  summarise(n_drafted_male = n(), .groups = "drop")

transfer_sample_dest_recovery <- worker_arrivals_1944_raw %>%
  select(staff_id, year_num, raw_arrival_type = arrival_type, dest_kyoku_missing) %>%
  right_join(
    worker_arrivals_1944 %>%
      filter(is_transfer_in) %>%
      inner_join(
        drafted_section_totals_1944 %>% select(section_id_filled, year_num),
        by = c("section_id_filled", "year_num")
      ) %>%
      select(staff_id, year_num),
    by = c("staff_id", "year_num")
  ) %>%
  filter(is.na(raw_arrival_type), dest_kyoku_missing) %>%
  left_join(
    df_names %>%
      filter(year_num == analysis_year) %>%
      transmute(staff_id, year_num, filled_kyoku = norm_kyoku_filled),
    by = c("staff_id", "year_num")
  ) %>%
  summarise(
    n_transfer_sample_missing_dest = n(),
    n_transfer_sample_recovered = sum(!is.na(filled_kyoku)),
    transfer_sample_recovery_rate = mean(!is.na(filled_kyoku))
  )

origin_distribution_1944 <- worker_arrivals_1944 %>%
  filter(is_transfer_in) %>%
  inner_join(
    drafted_section_totals_1944,
    by = c("section_id_filled", "year_num")
  ) %>%
  count(arrival_type, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

section_year_panel_1944 <- df_names %>%
  filter(year_num == analysis_year) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(is_new_hire = case_when(
    year_num == office_first_year ~ NA,
    TRUE ~ year_num == first_year
  )) %>%
  group_by(section_id_filled, office_id, year_num) %>%
  summarise(
    ka = first(na.omit(ka)),
    ka_group_filled = first(na.omit(ka_group_filled)),
    norm_kyoku_filled = first(na.omit(norm_kyoku_filled)),
    kyoku_group_filled = first(na.omit(kyoku_group_filled)),
    n_workers = n(),
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

section_hiring_lag_1944 <- df_names %>%
  filter(year_num == analysis_year - 1L) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(is_new_hire = case_when(
    year_num == office_first_year ~ NA,
    TRUE ~ year_num == first_year
  )) %>%
  group_by(section_id_filled, office_id, year_num) %>%
  summarise(origin_prior_hiring = sum(is_new_hire, na.rm = TRUE), .groups = "drop") %>%
  transmute(
    origin_section_id = section_id_filled,
    origin_office_id = office_id,
    year_num = year_num + 1L,
    origin_prior_hiring
  )

prewar_arrivals <- df_names %>%
  filter(year_num %in% prewar_years) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    arrival_type = arrival_distance(
      kyoku_group_filled, ka_group_filled, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id
  )

prewar_diffdept_pairs <- prewar_arrivals %>%
  filter(
    is_transfer_in,
    arrival_type == "different_department",
    !is.na(kyoku_group_filled),
    !is.na(lag_kyoku_group)
  ) %>%
  transmute(
    kyoku_a = pmin(kyoku_group_filled, lag_kyoku_group),
    kyoku_b = pmax(kyoku_group_filled, lag_kyoku_group)
  ) %>%
  distinct() %>%
  mutate(prewar_pair_allowed = 1L)

outcomes_section_1944 <- worker_arrivals_1944 %>%
  group_by(section_id_filled, office_id, year_num) %>%
  summarise(
    ka = first(na.omit(ka)),
    ka_group_filled = first(na.omit(ka_group_filled)),
    norm_kyoku_filled = first(na.omit(norm_kyoku_filled)),
    kyoku_group_filled = first(na.omit(kyoku_group_filled)),
    transfers_in = sum(is_transfer_in, na.rm = TRUE),
    new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

drafted_profiles_1944 <- df_all %>%
  filter(
    year_num == analysis_year,
    drafted == TRUE,
    !is_female,
    !is.na(section_id_filled),
    !is.na(pos_norm),
    !is.na(occupation),
    !is.na(pos_rank)
  ) %>%
  group_by(
    section_id_filled, office_id, year_num, ka, ka_group_filled,
    norm_kyoku_filled, kyoku_group_filled, pos_norm, occupation, pos_rank
  ) %>%
  summarise(n_drafted_profile = n(), .groups = "drop")

drafted_section_totals_full_1944 <- drafted_profiles_1944 %>%
  group_by(
    section_id_filled, office_id, year_num, ka, ka_group_filled,
    norm_kyoku_filled, kyoku_group_filled
  ) %>%
  summarise(n_drafted_male = sum(n_drafted_profile), .groups = "drop")

candidate_workers_1944 <- staff_lag %>%
  filter(
    year_num == analysis_year,
    !is.na(lag_office_id),
    !is.na(lag_occupation),
    !is.na(lag_pos_rank),
    lag_occupation != "engineer"
  ) %>%
  left_join(current_status, by = c("staff_id", "year_num")) %>%
  left_join(
    section_hiring_lag_1944,
    by = c("lag_section_id" = "origin_section_id", "lag_office_id" = "origin_office_id", "year_num")
  ) %>%
  mutate(origin_prior_hiring = replace_na(origin_prior_hiring, 0L)) %>%
  filter(current_observed == 1L, is.na(current_drafted) | current_drafted != TRUE, origin_prior_hiring > 0)

slack_matches_1944 <- drafted_profiles_1944 %>%
  rename(
    dest_section_id = section_id_filled,
    dest_office_id = office_id,
    dest_ka = ka,
    dest_ka_group = ka_group_filled,
    dest_norm_kyoku = norm_kyoku_filled,
    dest_kyoku_group = kyoku_group_filled,
    dest_pos_norm = pos_norm,
    dest_occupation = occupation,
    dest_pos_rank = pos_rank
  ) %>%
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
    slack_distance = arrival_distance(
      dest_kyoku_group, dest_ka_group, dest_ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    kyoku_a = pmin(dest_kyoku_group, lag_kyoku_group),
    kyoku_b = pmax(dest_kyoku_group, lag_kyoku_group)
  ) %>%
  left_join(prewar_diffdept_pairs, by = c("kyoku_a", "kyoku_b")) %>%
  mutate(prewar_pair_allowed = replace_na(prewar_pair_allowed, 0L)) %>%
  filter(slack_distance != "different_department" | prewar_pair_allowed == 1L) %>%
  filter(!is.na(slack_distance)) %>%
  distinct(dest_section_id, year_num, staff_id, slack_distance)

slack_counts_1944 <- slack_matches_1944 %>%
  count(dest_section_id, year_num, slack_distance, name = "n_slack") %>%
  pivot_wider(
    names_from = slack_distance,
    values_from = n_slack,
    values_fill = 0,
    names_prefix = "slack_"
  )

panel_1944 <- drafted_section_totals_full_1944 %>%
  left_join(
    outcomes_section_1944,
    by = c("section_id_filled", "office_id", "year_num", "ka", "ka_group_filled", "norm_kyoku_filled", "kyoku_group_filled")
  ) %>%
  left_join(
    section_year_panel_1944 %>% select(section_id_filled, office_id, year_num, n_workers),
    by = c("section_id_filled", "office_id", "year_num")
  ) %>%
  left_join(slack_counts_1944, by = c("section_id_filled" = "dest_section_id", "year_num")) %>%
  mutate(
    across(
      c(transfers_in, new_hires, slack_same_section, slack_same_department, slack_different_department),
      ~replace_na(., 0)
    ),
    transfer_share = if_else(transfers_in + new_hires > 0,
                             transfers_in / (transfers_in + new_hires),
                             NA_real_),
    any_slack_same_section = as.integer(slack_same_section > 0),
    log_slack_same_section = log(slack_same_section + 1),
    log_slack_same_department = log(slack_same_department + 1),
    log_slack_different_department = log(slack_different_department + 1),
    log_section_size = log(n_workers + 1)
  ) %>%
  filter(n_drafted_male > 0, !is.na(norm_kyoku_filled))

models_1944 <- list(
  transfers_in = run_feols_1944(panel_1944, "transfers_in"),
  new_hires = run_feols_1944(panel_1944, "new_hires"),
  transfer_share = run_feols_1944(panel_1944, "transfer_share")
)

regression_results_1944 <- imap_dfr(models_1944, function(mod, outcome) {
  tidy(mod) %>%
    filter(term %in% c(
      "any_slack_same_section",
      "log_slack_same_section",
      "log_slack_same_department",
      "log_slack_different_department",
      "n_drafted_male",
      "log_section_size"
    )) %>%
    mutate(
      outcome = outcome,
      n = nobs(mod)
    )
})

write_csv(dest_recovery_summary,
          file.path(result_dir, "tokyoto_1944_destination_recovery_summary_final.csv"))
write_csv(dest_recovery_by_method,
          file.path(result_dir, "tokyoto_1944_destination_recovery_by_method_final.csv"))
write_csv(transfer_sample_dest_recovery,
          file.path(result_dir, "tokyoto_1944_destination_recovery_transfer_sample.csv"))
write_csv(origin_distribution_1944,
          file.path(result_dir, "tokyoto_1944_cleaned_origin_distribution.csv"))
write_csv(panel_1944,
          file.path(result_dir, "tokyoto_1944_cleaned_slack_panel.csv"))
write_csv(regression_results_1944,
          file.path(result_dir, "tokyoto_1944_cleaned_slack_regression_results.csv"))

cat("Final destination recovery summary:\n")
print(dest_recovery_summary)
cat("\nRecovery by method:\n")
print(dest_recovery_by_method, n = Inf)
cat("\nTransfer-sample destination recovery:\n")
print(transfer_sample_dest_recovery)
cat("\n1944 cleaned origin distribution:\n")
print(origin_distribution_1944, n = Inf)
cat("\n1944 cleaned regression pattern:\n")
print(regression_results_1944, n = Inf)
