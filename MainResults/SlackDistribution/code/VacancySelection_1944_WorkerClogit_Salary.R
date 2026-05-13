################################################################################
# Within-origin worker selection conditional logit for 1944, salary variant.
#
# Adds current-year salary to the worker-level candidate set and reruns the
# within-origin clogit on the salary-observed subsample.
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

run_clogit_safe <- function(formula, data, method = "efron") {
  tryCatch(
    clogit(formula, data = data, method = method),
    error = function(e) NULL
  )
}

cat("Loading data...\n")

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
    dest_norm_kyoku,
    dest_kyoku_group,
    dest_ka,
    dest_ka_group,
    dest_pos_norm,
    dest_occupation,
    dest_pos_rank,
    origin_office_id,
    chosen_distance = distance_cat,
    event_origin_id = paste(event_id, origin_office_id, sep = "||")
  )

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
    pos_rank = assign_rank(pos_norm, year_num),
    court_rank = parse_court_rank(rank),
    decor_rank = parse_decoration(decoration),
    salary_num = parse_salary(salary)
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
    pos_rank = assign_rank(pos_norm, year_num),
    court_rank = parse_court_rank(rank),
    decor_rank = parse_decoration(decoration),
    salary_num = parse_salary(salary)
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
    lag_norm_kyoku = norm_kyoku_filled,
    lag_kyoku_group = kyoku_group_filled,
    lag_ka = ka,
    lag_ka_group = ka_group_filled,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_occupation = occupation,
    lag_is_female = is_female,
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
    current_office_id = office_id,
    current_section_id = section_id_filled,
    current_pos_norm = pos_norm,
    current_occupation = occupation,
    current_pos_rank = pos_rank,
    current_salary_num = salary_num,
    has_salary = as.integer(!is.na(salary_num) & salary_num > 0),
    log_salary = if_else(!is.na(salary_num) & salary_num > 0, log(salary_num + 1), NA_real_)
  )

worker_arrivals_1944 <- df_names %>%
  filter(year_num == analysis_year) %>%
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

selected_workers <- worker_arrivals_1944 %>%
  filter(is_transfer_in) %>%
  inner_join(
    selected_origins,
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
  transmute(
    event_origin_id,
    event_id,
    origin_office_id = lag_office_id,
    chosen_distance,
    selected_staff_id = staff_id
  )

selected_worker_summary <- selected_workers %>%
  group_by(event_origin_id, event_id, origin_office_id, chosen_distance) %>%
  summarise(n_selected_workers = n_distinct(selected_staff_id), .groups = "drop")

candidate_workers <- staff_lag %>%
  filter(year_num == analysis_year, !is.na(lag_office_id), !is.na(lag_occupation), !is.na(lag_pos_rank)) %>%
  left_join(current_status, by = c("staff_id", "year_num")) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    own_tenure = analysis_year - first_year
  ) %>%
  filter(current_observed == 1L, is.na(current_drafted) | current_drafted != TRUE)

worker_choice <- selected_origins %>%
  inner_join(
    candidate_workers,
    by = c("year_num" = "year_num", "origin_office_id" = "lag_office_id"),
    relationship = "many-to-many"
  ) %>%
  filter(
    lag_occupation == dest_occupation,
    abs(lag_pos_rank - dest_pos_rank) <= 1
  ) %>%
  mutate(
    event_origin_id = paste(event_id, origin_office_id, sep = "||"),
    exact_pos_match = as.integer(lag_pos_norm == dest_pos_norm),
    lag_court_rank_ext = coalesce(as.numeric(lag_court_rank), 0),
    lag_decor_rank_ext = coalesce(as.numeric(lag_decor_rank), 0),
    female = as.integer(coalesce(lag_is_female, FALSE)),
    rank_gap = dest_pos_rank - lag_pos_rank,
    lag_posrank_x_dest_rank = lag_pos_rank * dest_pos_rank
  ) %>%
  left_join(
    selected_workers %>%
      transmute(
        event_origin_id,
        event_id,
        origin_office_id,
        staff_id = selected_staff_id,
        chosen = 1L
      ),
    by = c("event_origin_id", "event_id", "origin_office_id", "staff_id")
  ) %>%
  mutate(chosen = replace_na(chosen, 0L))

worker_event_diag <- worker_choice %>%
  group_by(event_origin_id) %>%
  summarise(
    event_id = first(event_id),
    origin_office_id = first(origin_office_id),
    chosen_distance = first(chosen_distance),
    n_candidates = n(),
    n_selected = sum(chosen),
    n_exact_pos = sum(exact_pos_match),
    salary_candidate_share = mean(has_salary == 1, na.rm = TRUE),
    any_selected_with_salary = any(chosen == 1 & has_salary == 1),
    all_candidates_with_salary = all(has_salary == 1),
    .groups = "drop"
  ) %>%
  mutate(
    usable = n_candidates >= 2 & n_selected >= 1,
    usable_salary = usable & n_candidates >= 2 & n_selected >= 1 &
      all_candidates_with_salary & any_selected_with_salary,
    drop_reason = case_when(
      usable ~ "usable",
      n_candidates < 2 ~ "degenerate_choice_set",
      n_selected == 0 ~ "selected_worker_not_in_candidates",
      TRUE ~ "other"
    ),
    drop_reason_salary = case_when(
      usable_salary ~ "usable_salary",
      !usable ~ drop_reason,
      !any_selected_with_salary ~ "selected_worker_missing_salary",
      !all_candidates_with_salary ~ "candidate_missing_salary",
      TRUE ~ "other"
    )
  )

single_multi_diag <- selected_worker_summary %>%
  mutate(
    selection_multiplicity = case_when(
      n_selected_workers == 1 ~ "single_selected_worker",
      n_selected_workers > 1 ~ "multiple_selected_workers",
      TRUE ~ "other"
    )
  ) %>%
  count(selection_multiplicity, sort = TRUE, name = "n_event_origins") %>%
  mutate(share = n_event_origins / sum(n_event_origins))

worker_choice_salary <- worker_choice %>%
  filter(has_salary == 1) %>%
  inner_join(worker_event_diag %>% filter(usable_salary), by = "event_origin_id", suffix = c("", "_diag"))

run_worker_specs <- function(dat, spec_label) {
  models <- list(
    full = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat
    ),
    same_section_origin = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat %>% filter(chosen_distance == "same_section")
    ),
    cross_section_origin = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat %>% filter(chosen_distance != "same_section")
    )
  )

  split_data <- list(
    full = dat,
    same_section_origin = dat %>% filter(chosen_distance == "same_section"),
    cross_section_origin = dat %>% filter(chosen_distance != "same_section")
  )

  imap_dfr(models, function(mod, name) {
    dat_spec <- split_data[[name]]
    if (is.null(mod)) {
      return(tibble(
        term = NA_character_, estimate = NA_real_, std.error = NA_real_,
        statistic = NA_real_, p.value = NA_real_, specification = name,
        sample = spec_label, n_event_origins = n_distinct(dat_spec$event_origin_id),
        n_rows = nrow(dat_spec)
      ))
    }
    tidy(mod) %>%
      mutate(
        specification = name,
        sample = spec_label,
        n_event_origins = n_distinct(dat_spec$event_origin_id),
        n_rows = nrow(dat_spec)
      )
  })
}

results <- run_worker_specs(worker_choice_salary, "occ_rank_worker_pool_salary")

salary_coverage_summary <- worker_event_diag %>%
  summarise(
    n_event_origins_total = n(),
    n_usable_no_salary = sum(usable),
    n_usable_salary = sum(usable_salary),
    median_salary_candidate_share = median(salary_candidate_share),
    share_all_candidates_with_salary = mean(all_candidates_with_salary),
    share_selected_with_salary = mean(any_selected_with_salary)
  )

write_csv(worker_event_diag, file.path(result_dir, "worker_selection_1944_salary_event_origin_diagnostic.csv"))
write_csv(single_multi_diag, file.path(result_dir, "worker_selection_1944_salary_single_multi_diagnostic.csv"))
write_csv(worker_choice_salary, file.path(result_dir, "worker_selection_1944_salary_choice_dataset.csv"))
write_csv(results, file.path(result_dir, "worker_selection_1944_salary_clogit_results.csv"))
write_csv(salary_coverage_summary, file.path(result_dir, "worker_selection_1944_salary_coverage_summary.csv"))

cat("\nSelected-worker multiplicity diagnostic:\n")
print(single_multi_diag, n = Inf)
cat("\nWorker-event diagnostic (salary-aware):\n")
print(worker_event_diag %>% count(drop_reason_salary, sort = TRUE, name = "n_event_origins"), n = Inf)
cat("\nSalary coverage summary:\n")
print(salary_coverage_summary)
cat("\nUsable salary event-origin strata:", n_distinct(worker_choice_salary$event_origin_id), "\n")
cat("Median candidates per usable salary stratum:",
    worker_event_diag %>% filter(usable_salary) %>% summarise(m = median(n_candidates)) %>% pull(m), "\n")
cat("\nWorker-level salary clogit results:\n")
print(results, n = Inf)
