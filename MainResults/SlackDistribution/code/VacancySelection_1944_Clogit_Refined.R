################################################################################
# Refined 1944 vacancy-event conditional logit.
#
# Adds:
# - event-cell definition at destination section x position x year
# - drop diagnostics from selected transfers to usable strata
# - continuous prewar tie measure
# - exact-position and occupation-rank choice-set variants
# - same-section-feasibility split models
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

build_choice_rows <- function(events, candidate_workers, prewar_diffdept_pairs, variant = c("exact_pos", "occ_rank")) {
  variant <- match.arg(variant)
  if (variant == "exact_pos") {
    joined <- events %>%
      inner_join(
        candidate_workers,
        by = c(
          "year_num" = "year_num",
          "dest_pos_norm" = "lag_pos_norm",
          "dest_occupation" = "lag_occupation"
        ),
        relationship = "many-to-many"
      ) %>%
      filter(abs(lag_pos_rank - dest_pos_rank) <= 1)
  } else {
    joined <- events %>%
      inner_join(
        candidate_workers,
        by = c(
          "year_num" = "year_num",
          "dest_occupation" = "lag_occupation",
          "dest_pos_rank" = "lag_pos_rank"
        ),
        relationship = "many-to-many"
      )
  }

  joined %>%
    filter(lag_office_id != dest_office_id) %>%
    mutate(
      distance_cat = arrival_distance(
        dest_kyoku_group, dest_ka_group, dest_ka,
        lag_kyoku_group, lag_ka_group, lag_ka
      ),
      kyoku_a = pmin(dest_kyoku_group, lag_kyoku_group),
      kyoku_b = pmax(dest_kyoku_group, lag_kyoku_group)
    ) %>%
    left_join(prewar_diffdept_pairs, by = c("kyoku_a", "kyoku_b")) %>%
    mutate(prewar_pair_allowed = replace_na(prewar_pair_allowed, 0L)) %>%
    filter(distance_cat != "different_department" | prewar_pair_allowed == 1L) %>%
    filter(!is.na(distance_cat))
}

run_clogit_safe <- function(formula, data, method = "efron") {
  tryCatch(
    clogit(formula, data = data, method = method),
    error = function(e) NULL
  )
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
    lag_section_id = section_id_filled
  ) %>%
  mutate(year_num = year_num + 1L)

current_status <- df_all %>%
  filter(year_num == analysis_year) %>%
  transmute(staff_id, year_num, current_drafted = drafted, current_observed = 1L)

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
  group_by(section_id_filled, office_id, year_num, ka, ka_group_filled,
           norm_kyoku_filled, kyoku_group_filled, pos_norm, occupation, pos_rank) %>%
  summarise(n_drafted_profile = n(), .groups = "drop")

drafted_section_totals_1944 <- drafted_profiles_1944 %>%
  group_by(section_id_filled, office_id, year_num, ka, ka_group_filled,
           norm_kyoku_filled, kyoku_group_filled) %>%
  summarise(n_drafted_male = sum(n_drafted_profile), .groups = "drop")

# Event cells: destination section x position x year, with one or more actual
# transfer-ins into a draft destination.
event_cells <- worker_arrivals_1944 %>%
  filter(is_transfer_in, !is.na(lag_office_id), !is.na(pos_norm), !is.na(occupation), !is.na(pos_rank)) %>%
  inner_join(
    drafted_section_totals_1944 %>% select(section_id_filled, year_num),
    by = c("section_id_filled", "year_num"),
    relationship = "many-to-many"
  ) %>%
  group_by(
    year_num, dest_office_id = office_id, dest_section_id = section_id_filled,
    dest_ka = ka, dest_ka_group = ka_group_filled,
    dest_norm_kyoku = norm_kyoku_filled, dest_kyoku_group = kyoku_group_filled,
    dest_pos_norm = pos_norm, dest_occupation = occupation, dest_pos_rank = pos_rank
  ) %>%
  summarise(
    selected_origin_n = n_distinct(lag_office_id),
    selected_origin_offices = paste(sort(unique(lag_office_id)), collapse = "|"),
    selected_worker_n = n(),
    .groups = "drop"
  ) %>%
  mutate(event_id = paste0("evt_", row_number()))

cat("Draft-destination event cells:", nrow(event_cells), "\n")

selected_origins_long <- event_cells %>%
  separate_rows(selected_origin_offices, sep = "\\|") %>%
  transmute(event_id, chosen_origin_office_id = as.numeric(selected_origin_offices))

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
  filter(is_transfer_in, arrival_type == "different_department",
         !is.na(kyoku_group_filled), !is.na(lag_kyoku_group)) %>%
  transmute(kyoku_a = pmin(kyoku_group_filled, lag_kyoku_group),
            kyoku_b = pmax(kyoku_group_filled, lag_kyoku_group)) %>%
  distinct() %>% mutate(prewar_pair_allowed = 1L)

prewar_office_ties <- prewar_arrivals %>%
  filter(is_transfer_in, !is.na(lag_office_id), !is.na(office_id)) %>%
  count(origin_office_id = lag_office_id, dest_office_id = office_id, name = "prewar_flow_count")

build_choice_dataset <- function(variant = c("exact_pos", "occ_rank")) {
  variant <- match.arg(variant)
  choice_rows <- build_choice_rows(event_cells, candidate_workers_1944, prewar_diffdept_pairs, variant = variant)

  choice_office_level <- choice_rows %>%
    group_by(
      event_id, year_num, dest_office_id, dest_section_id, dest_norm_kyoku, dest_kyoku_group,
      dest_ka, dest_ka_group, dest_pos_norm, dest_occupation, dest_pos_rank,
      origin_office_id = lag_office_id
    ) %>%
    summarise(
      distance_cat = first(distance_cat),
      origin_matched_workers = n_distinct(staff_id),
      origin_max_prior_hiring = max(origin_prior_hiring, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(selected_origins_long %>% mutate(chosen = 1L),
              by = c("event_id", "origin_office_id" = "chosen_origin_office_id")) %>%
    mutate(chosen = replace_na(chosen, 0L)) %>%
    left_join(prewar_office_ties, by = c("origin_office_id", "dest_office_id")) %>%
    mutate(
      prewar_flow_count = replace_na(prewar_flow_count, 0L),
      log_prewar_flow_count = log(prewar_flow_count + 1),
      prewar_tie = as.integer(prewar_flow_count > 0),
      same_section = as.integer(distance_cat == "same_section"),
      same_department = as.integer(distance_cat == "same_department"),
      log_origin_matched_workers = log(origin_matched_workers + 1),
      log_origin_prior_hiring = log(origin_max_prior_hiring + 1)
    )

  event_diag <- choice_office_level %>%
    group_by(event_id) %>%
    summarise(
      n_candidates = n(),
      n_chosen = sum(chosen),
      same_section_feasible = as.integer(any(same_section == 1)),
      chosen_same_section = as.integer(any(chosen == 1 & same_section == 1)),
      .groups = "drop"
    ) %>%
    mutate(
      usable = n_candidates >= 2 & n_chosen >= 1,
      drop_reason = case_when(
        usable ~ "usable",
        n_candidates < 2 ~ "degenerate_choice_set",
        n_chosen == 0 ~ "chosen_origin_not_in_candidates",
        TRUE ~ "other"
      ),
      variant = variant
    )

  missing_events <- event_cells %>%
    anti_join(event_diag %>% select(event_id), by = "event_id") %>%
    transmute(
      event_id,
      n_candidates = 0L,
      n_chosen = 0L,
      same_section_feasible = NA_integer_,
      chosen_same_section = NA_integer_,
      usable = FALSE,
      drop_reason = "no_candidate_rows",
      variant = variant
    )

  event_diag <- bind_rows(event_diag, missing_events)

  list(choice = choice_office_level, diag = event_diag)
}

exact_obj <- build_choice_dataset("exact_pos")
occ_rank_obj <- build_choice_dataset("occ_rank")

drop_summary <- bind_rows(exact_obj$diag, occ_rank_obj$diag) %>%
  count(variant, drop_reason, sort = TRUE, name = "n_events") %>%
  group_by(variant) %>%
  mutate(share = n_events / sum(n_events)) %>%
  ungroup()

usable_exact <- exact_obj$choice %>%
  inner_join(exact_obj$diag %>% filter(usable) %>% select(event_id, same_section_feasible, chosen_same_section), by = "event_id")

usable_occ_rank <- occ_rank_obj$choice %>%
  inner_join(occ_rank_obj$diag %>% filter(usable) %>% select(event_id, same_section_feasible, chosen_same_section), by = "event_id")

run_specs <- function(dat, variant_label) {
  split_data <- list(
    all = dat,
    no_same_section_feasible = dat %>% filter(same_section_feasible == 0),
    same_section_feasible = dat %>% filter(same_section_feasible == 1)
  )
  mods <- list(
    all = run_clogit_safe(
      chosen ~ same_section + same_department + log_origin_matched_workers +
        log_origin_prior_hiring + log_prewar_flow_count + strata(event_id),
      data = split_data$all
    ),
    no_same_section_feasible = run_clogit_safe(
      chosen ~ same_department + log_origin_matched_workers +
        log_origin_prior_hiring + log_prewar_flow_count + strata(event_id),
      data = split_data$no_same_section_feasible
    ),
    same_section_feasible = run_clogit_safe(
      chosen ~ same_section + same_department + log_origin_matched_workers +
        log_origin_prior_hiring + log_prewar_flow_count + strata(event_id),
      data = split_data$same_section_feasible
    )
  )

  imap_dfr(mods, function(mod, spec_name) {
    dat_spec <- split_data[[spec_name]]
    if (is.null(mod)) {
      return(tibble(
        term = NA_character_, estimate = NA_real_, std.error = NA_real_,
        statistic = NA_real_, p.value = NA_real_, specification = spec_name,
        variant = variant_label, n_events = n_distinct(dat_spec$event_id), n_rows = nrow(dat_spec)
      ))
    }
    tidy(mod) %>%
      mutate(
        specification = spec_name,
        variant = variant_label,
        n_events = n_distinct(dat_spec$event_id),
        n_rows = nrow(dat_spec)
      )
  })
}

results <- bind_rows(
  run_specs(usable_exact, "exact_pos"),
  run_specs(usable_occ_rank, "occ_rank")
)

event_summary <- bind_rows(
  exact_obj$diag %>% group_by(variant) %>% summarise(
    n_events_total = n(),
    n_events_usable = sum(usable),
    median_candidates_usable = median(n_candidates[usable]),
    share_same_section_feasible = mean(same_section_feasible[usable]),
    share_chosen_same_section = mean(chosen_same_section[usable]),
    .groups = "drop"
  ),
  occ_rank_obj$diag %>% group_by(variant) %>% summarise(
    n_events_total = n(),
    n_events_usable = sum(usable),
    median_candidates_usable = median(n_candidates[usable]),
    share_same_section_feasible = mean(same_section_feasible[usable]),
    share_chosen_same_section = mean(chosen_same_section[usable]),
    .groups = "drop"
  )
)

write_csv(drop_summary, file.path(result_dir, "vacancy_selection_1944_drop_summary.csv"))
write_csv(event_summary, file.path(result_dir, "vacancy_selection_1944_refined_event_summary.csv"))
write_csv(results, file.path(result_dir, "vacancy_selection_1944_refined_clogit_results.csv"))
write_csv(usable_exact, file.path(result_dir, "vacancy_selection_1944_exact_choice_dataset.csv"))
write_csv(usable_occ_rank, file.path(result_dir, "vacancy_selection_1944_occ_rank_choice_dataset.csv"))

cat("\nDrop summary:\n")
print(drop_summary, n = Inf)
cat("\nEvent summary:\n")
print(event_summary, n = Inf)
cat("\nRefined clogit results:\n")
print(results, n = Inf)
