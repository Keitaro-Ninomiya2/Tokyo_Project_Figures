################################################################################
# Bilateral transfer-flow PPML, Tokyo civil service offices, 1938-1945
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
reference_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "reference_tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- Sys.getenv("TOKYO_PERSONNEL_MASTER", unset = file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
))
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")
BASE_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_ka_to_kyoku.csv")
FACILITY_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_facility_to_kyoku.csv")
KAKARI_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_kakari_to_kyoku.csv")

years_of_interest <- 1938:1945
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

clean_label <- function(x) {
  x <- str_squish(replace_na(as.character(x), ""))
  na_if(x, "")
}

office_key <- function(kyoku_clean, ka_clean) {
  if_else(!is.na(kyoku_clean) & !is.na(ka_clean),
          paste(kyoku_clean, ka_clean, sep = " :: "),
          NA_character_)
}

safe_first <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

section_key <- function(office_id, ka_group, ka_name) {
  if_else(
    !is.na(ka_group),
    paste0(office_id, "_grp_", ka_group),
    paste0(office_id, "_name_", replace_na(ka_name, "missing"))
  )
}

apply_1944_tokyoto_backfill <- function(df, ka_group_raw) {
  base_ref_long <- read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
    filter(as.Date(effective_start) <= as.Date("1944-12-31"),
           as.Date(effective_end) >= as.Date("1944-01-01"))

  base_ref <- base_ref_long %>%
    count(ka, name = "base_candidate_count") %>%
    left_join(
      base_ref_long %>%
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

  ambiguous_rows <- df %>%
    left_join(base_ref, by = "ka") %>%
    left_join(adjacent_staff_kyoku, by = "staff_id") %>%
    filter(year_num == analysis_year,
           is.na(clean_label(kyoku)),
           !is.na(ka),
           replace_na(base_candidate_count, 0L) > 1L,
           !is.na(adjacent_known_kyoku)) %>%
    select(staff_id, ka, adjacent_known_kyoku) %>%
    distinct() %>%
    inner_join(base_ref_long %>% select(ka, candidate_kyoku = kyoku),
               by = "ka", relationship = "many-to-many") %>%
    filter(adjacent_known_kyoku == candidate_kyoku) %>%
    group_by(staff_id, ka) %>%
    summarise(
      history_match_n = n(),
      history_match_kyoku = if_else(n() == 1L, first(candidate_kyoku), NA_character_),
      .groups = "drop"
    )

  df %>%
    left_join(base_ref, by = "ka") %>%
    left_join(facility_ref, by = "ka") %>%
    left_join(kakari_ref, by = "kakari") %>%
    left_join(ambiguous_rows, by = c("staff_id", "ka")) %>%
    mutate(
      kyoku_backfill = case_when(
        year_num != analysis_year ~ kyoku,
        !is.na(clean_label(kyoku)) ~ kyoku,
        !is.na(base_ref_kyoku) ~ base_ref_kyoku,
        !is.na(facility_ref_kyoku) ~ facility_ref_kyoku,
        !is.na(history_match_kyoku) ~ history_match_kyoku,
        !is.na(kakari_ref_kyoku) ~ kakari_ref_kyoku,
        TRUE ~ NA_character_
      ),
      kyoku_backfill_method = case_when(
        year_num != analysis_year ~ "original_non_1944",
        !is.na(clean_label(kyoku)) ~ "original",
        !is.na(base_ref_kyoku) ~ "base_ka_reference",
        !is.na(facility_ref_kyoku) ~ "facility_ka_reference",
        !is.na(history_match_kyoku) ~ "staff_history_tiebreak",
        !is.na(kakari_ref_kyoku) ~ "kakari_reference",
        TRUE ~ NA_character_
      ),
      norm_kyoku_filled = normalize_kyoku(kyoku_backfill),
      kyoku_group_filled = assign_kyoku_group(norm_kyoku_filled),
      kyoku_clean_filled = replace_na(norm_kyoku_filled, ""),
      ka_clean_filled = replace_na(clean_label(ka), "")
    ) %>%
    left_join(
      ka_group_raw %>%
        rename(ka_group_lookup = ka_group, kyoku_clean_filled = kyoku,
               ka_clean_filled = ka, year_num = year),
      by = c("year_num", "kyoku_clean_filled", "ka_clean_filled")
    ) %>%
    mutate(
      ka_group_filled = if_else(year_num == analysis_year, ka_group_lookup, ka_group),
      section_id_filled = section_key(office_id, ka_group_filled, ka),
      office_clean = office_key(norm_kyoku_filled, clean_label(ka))
    ) %>%
    select(-ka_group_lookup)
}

first_nonmissing_by_group <- function(df, group_vars, value_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(value_vars), safe_first), .groups = "drop")
}

cat("Loading master panel from: ", DATA_PATH, "\n", sep = "")

raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.integer(as.numeric(year)),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(clean_label(ka), ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num),
    occ_rank = paste(occupation, pos_rank, sep = "_")
  )

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""), ka = replace_na(clean_label(ka), ""))

raw <- raw %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- raw %>%
  filter(is_name == TRUE) %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  apply_1944_tokyoto_backfill(ka_group_raw)

df_all <- raw %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  apply_1944_tokyoto_backfill(ka_group_raw)

lag_panel <- df_names %>%
  filter(year_num %in% (years_of_interest - 1L)) %>%
  select(staff_id, lag_year = year_num, origin_office = office_clean,
         origin_kyoku = norm_kyoku_filled, origin_ka = ka,
         origin_kyoku_group = kyoku_group_filled, origin_ka_group = ka_group_filled,
         lag_occupation = occupation, lag_pos_rank = pos_rank,
         lag_occ_rank = occ_rank, lag_pos_norm = pos_norm) %>%
  mutate(year_num = lag_year + 1L)

current_panel <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
  select(staff_id, year_num, destination_office = office_clean,
         destination_kyoku = norm_kyoku_filled, destination_ka = ka,
         destination_kyoku_group = kyoku_group_filled,
         destination_ka_group = ka_group_filled)

transfers <- current_panel %>%
  inner_join(lag_panel, by = c("staff_id", "year_num")) %>%
  filter(!is.na(origin_office), !is.na(destination_office),
         origin_office != destination_office) %>%
  count(year_num, origin_office, destination_office, name = "flow_count")

office_attrs <- df_names %>%
  filter(year_num %in% (years_of_interest - 1L),
         !is.na(office_clean), !is.na(norm_kyoku_filled), !is.na(clean_label(ka))) %>%
  group_by(year_num, office_clean) %>%
  summarise(
    kyoku = safe_first(norm_kyoku_filled),
    ka = safe_first(clean_label(ka)),
    kyoku_group = safe_first(kyoku_group_filled),
    ka_group = safe_first(ka_group_filled),
    .groups = "drop"
  )

office_profiles <- df_all %>%
  filter(year_num %in% (years_of_interest - 1L),
         !is.na(office_clean), is.na(drafted) | drafted != TRUE,
         !is.na(occ_rank), !is.na(pos_norm)) %>%
  count(year_num, office_clean, occ_rank, pos_norm, name = "n_profile")

office_occ_rank <- office_profiles %>%
  group_by(year_num, office_clean, occ_rank) %>%
  summarise(n_occ_rank = sum(n_profile), .groups = "drop")

pair_sizes <- office_occ_rank %>%
  rename(origin_office = office_clean, origin_n_profile = n_occ_rank) %>%
  inner_join(
    office_occ_rank %>% rename(destination_office = office_clean, destination_n_profile = n_occ_rank),
    by = c("year_num", "occ_rank"),
    relationship = "many-to-many"
  ) %>%
  filter(origin_office != destination_office) %>%
  group_by(year_num, origin_office, destination_office) %>%
  summarise(
    N_o = sum(origin_n_profile),
    N_d = sum(destination_n_profile),
    shared_occ_rank_cells = n_distinct(occ_rank),
    .groups = "drop"
  ) %>%
  mutate(year_num = year_num + 1L)

profile_norms <- office_profiles %>%
  group_by(year_num, office_clean) %>%
  summarise(norm = sqrt(sum(n_profile^2)), .groups = "drop")

occ_similarity <- office_profiles %>%
  select(year_num, pos_norm, origin_office = office_clean, origin_n = n_profile) %>%
  inner_join(
    office_profiles %>%
      select(year_num, pos_norm, destination_office = office_clean, destination_n = n_profile),
    by = c("year_num", "pos_norm"),
    relationship = "many-to-many"
  ) %>%
  filter(origin_office != destination_office) %>%
  group_by(year_num, origin_office, destination_office) %>%
  summarise(dot_product = sum(origin_n * destination_n), .groups = "drop") %>%
  left_join(profile_norms %>% rename(origin_office = office_clean, origin_norm = norm),
            by = c("year_num", "origin_office")) %>%
  left_join(profile_norms %>% rename(destination_office = office_clean, destination_norm = norm),
            by = c("year_num", "destination_office")) %>%
  mutate(
    occ_similarity = if_else(origin_norm > 0 & destination_norm > 0,
                             dot_product / (origin_norm * destination_norm), NA_real_),
    year_num = year_num + 1L
  ) %>%
  select(year_num, origin_office, destination_office, occ_similarity)

origin_attrs <- office_attrs %>%
  transmute(year_num = year_num + 1L, origin_office = office_clean,
            origin_kyoku = kyoku, origin_ka = ka,
            origin_kyoku_group = kyoku_group, origin_ka_group = ka_group)

dest_attrs <- office_attrs %>%
  transmute(year_num = year_num + 1L, destination_office = office_clean,
            destination_kyoku = kyoku, destination_ka = ka,
            destination_kyoku_group = kyoku_group, destination_ka_group = ka_group)

draft_counts <- df_all %>%
  filter(year_num %in% years_of_interest,
         drafted == TRUE, !is_female, !is.na(office_clean)) %>%
  count(year_num, office_clean, name = "D_count")

pair_panel_full <- pair_sizes %>%
  left_join(occ_similarity, by = c("year_num", "origin_office", "destination_office")) %>%
  left_join(origin_attrs, by = c("year_num", "origin_office")) %>%
  left_join(dest_attrs, by = c("year_num", "destination_office")) %>%
  left_join(transfers, by = c("year_num", "origin_office", "destination_office")) %>%
  left_join(draft_counts %>% rename(origin_office = office_clean, D_origin = D_count),
            by = c("year_num", "origin_office")) %>%
  left_join(draft_counts %>% rename(destination_office = office_clean, D_destination = D_count),
            by = c("year_num", "destination_office")) %>%
  mutate(
    flow_count = replace_na(flow_count, 0L),
    D_origin = replace_na(D_origin, 0L),
    D_destination = replace_na(D_destination, 0L),
    D_origin_any = as.integer(D_origin > 0L),
    D_destination_any = as.integer(D_destination > 0L),
    log_N_o = log(N_o),
    log_N_d = log(N_d),
    occ_similarity = replace_na(occ_similarity, 0),
    occ_similarity_binary = as.integer(occ_similarity > 0.5),
    distance_category = case_when(
      !is.na(origin_kyoku) & !is.na(destination_kyoku) &
        !is.na(origin_ka) & !is.na(destination_ka) &
        origin_kyoku == destination_kyoku & origin_ka == destination_ka ~ "same_section",
      !is.na(origin_kyoku) & !is.na(destination_kyoku) &
        origin_kyoku == destination_kyoku ~ "same_department",
      !is.na(origin_kyoku) & !is.na(destination_kyoku) &
        origin_kyoku != destination_kyoku ~ "different_department",
      TRUE ~ NA_character_
    ),
    same_department = as.integer(distance_category == "same_department"),
    different_department = as.integer(distance_category == "different_department"),
    pair_id = paste(origin_office, destination_office, sep = " => "),
    year = year_num
  ) %>%
  filter(!is.na(origin_office), !is.na(destination_office),
         !is.na(origin_kyoku), !is.na(destination_kyoku),
         !is.na(distance_category), distance_category != "same_section",
         is.finite(log_N_o), is.finite(log_N_d))

pair_panel_nondraft <- pair_panel_full %>%
  filter(D_origin == 0L, D_destination == 0L)

fit_warnings <- character()
fit_errors <- character()

capture_fit <- function(label, expr) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, paste(label, conditionMessage(w), sep = ": "))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      fit_errors <<- c(fit_errors, paste(label, conditionMessage(e), sep = ": "))
      NULL
    }
  )
}

coef_table <- function(model, model_name, sample_name) {
  if (is.null(model)) {
    return(tibble(
      model = model_name,
      sample = sample_name,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      nobs = NA_integer_,
      pseudo_r2 = NA_real_,
      dispersion = NA_real_,
      status = "failed"
    ))
  }
  td <- broom::tidy(model, conf.int = TRUE)
  model_nobs <- tryCatch(as.integer(model$nobs), error = function(e) NA_integer_)
  pseudo_r2 <- tryCatch(as.numeric(fitstat(model, "pr2")[[1L]]), error = function(e) NA_real_)
  pearson <- tryCatch(residuals(model, type = "pearson"), error = function(e) rep(NA_real_, model_nobs))
  residual_df <- tryCatch(df.residual(model), error = function(e) model_nobs - length(coef(model)))
  dispersion <- if (all(is.na(pearson)) || is.na(residual_df) || residual_df <= 0) {
    NA_real_
  } else {
    sum(pearson^2, na.rm = TRUE) / residual_df
  }
  td %>%
    mutate(
      model = model_name,
      sample = sample_name,
      nobs = model_nobs,
      pseudo_r2 = pseudo_r2,
      dispersion = dispersion,
      status = "estimated",
      .before = 1
    )
}

base_formula_pair <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity + same_department + different_department | pair_id + year

base_formula_od_year <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity + same_department + different_department |
  origin_office^year + destination_office^year

draft_formula_pair <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity + same_department + different_department +
  D_origin + D_destination +
  D_origin:log_N_o + D_origin:log_N_d + D_origin:occ_similarity +
  D_origin:same_department + D_origin:different_department +
  D_destination:log_N_o + D_destination:log_N_d + D_destination:occ_similarity +
  D_destination:same_department + D_destination:different_department | pair_id + year

draft_formula_od_year <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity + same_department + different_department +
  D_origin + D_destination +
  D_origin:log_N_o + D_origin:log_N_d + D_origin:occ_similarity +
  D_origin:same_department + D_origin:different_department +
  D_destination:log_N_o + D_destination:log_N_d + D_destination:occ_similarity +
  D_destination:same_department + D_destination:different_department |
  origin_office^year + destination_office^year

draft_formula_binary_similarity <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity_binary + same_department + different_department +
  D_origin + D_destination +
  D_origin:log_N_o + D_origin:log_N_d + D_origin:occ_similarity_binary +
  D_origin:same_department + D_origin:different_department +
  D_destination:log_N_o + D_destination:log_N_d + D_destination:occ_similarity_binary +
  D_destination:same_department + D_destination:different_department | pair_id + year

draft_formula_indicator <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity + same_department + different_department +
  D_origin_any + D_destination_any +
  D_origin_any:log_N_o + D_origin_any:log_N_d + D_origin_any:occ_similarity +
  D_origin_any:same_department + D_origin_any:different_department +
  D_destination_any:log_N_o + D_destination_any:log_N_d + D_destination_any:occ_similarity +
  D_destination_any:same_department + D_destination_any:different_department | pair_id + year

cat("Estimating PPML models...\n")

baseline_model <- capture_fit(
  "baseline_pair_year",
  fepois(base_formula_pair, data = pair_panel_nondraft,
         cluster = ~ origin_office + destination_office)
)

draft_model <- capture_fit(
  "draft_pair_year",
  fepois(draft_formula_pair, data = pair_panel_full,
         cluster = ~ origin_office + destination_office)
)

robust_models <- list(
  origin_destination_year_fe = capture_fit(
    "origin_destination_year_fe",
    fepois(draft_formula_od_year, data = pair_panel_full,
           cluster = ~ origin_office + destination_office)
  ),
  pre_merger_1938_1942 = capture_fit(
    "pre_merger_1938_1942",
    fepois(draft_formula_pair, data = filter(pair_panel_full, year %in% 1938:1942),
           cluster = ~ origin_office + destination_office)
  ),
  post_merger_1944_1945 = capture_fit(
    "post_merger_1944_1945",
    fepois(draft_formula_pair, data = filter(pair_panel_full, year %in% 1944:1945),
           cluster = ~ origin_office + destination_office)
  ),
  binary_occupational_similarity = capture_fit(
    "binary_occupational_similarity",
    fepois(draft_formula_binary_similarity, data = pair_panel_full,
           cluster = ~ origin_office + destination_office)
  ),
  draft_indicator = capture_fit(
    "draft_indicator",
    fepois(draft_formula_indicator, data = pair_panel_full,
           cluster = ~ origin_office + destination_office)
  )
)

baseline_results <- coef_table(baseline_model, "baseline_pair_year_fe", "nondraft_pairs")
draft_results <- coef_table(draft_model, "draft_pair_year_fe", "full_panel")
robustness_results <- imap_dfr(robust_models, ~ coef_table(.x, .y, "full_or_named_subsample"))

sample_summary <- pair_panel_full %>%
  group_by(year) %>%
  summarise(
    n_pair_years = n(),
    n_pairs = n_distinct(pair_id),
    zero_flow_share = mean(flow_count == 0),
    positive_flow_share = mean(flow_count > 0),
    total_flows = sum(flow_count),
    mean_flow = mean(flow_count),
    max_flow = max(flow_count),
    n_origin_draft_pair_years = sum(D_origin > 0),
    n_destination_draft_pair_years = sum(D_destination > 0),
    n_both_draft_pair_years = sum(D_origin > 0 & D_destination > 0),
    n_neither_draft_pair_years = sum(D_origin == 0 & D_destination == 0),
    share_origin_draft = mean(D_origin > 0),
    share_destination_draft = mean(D_destination > 0),
    share_both_draft = mean(D_origin > 0 & D_destination > 0),
    share_neither_draft = mean(D_origin == 0 & D_destination == 0),
    .groups = "drop"
  )

distance_diagnostics <- pair_panel_full %>%
  group_by(distance_category) %>%
  summarise(
    n_pair_years = n(),
    positive_flow_share = mean(flow_count > 0),
    total_flows = sum(flow_count),
    zero_flow_share = mean(flow_count == 0),
    .groups = "drop"
  )

flow_diagnostics <- pair_panel_full %>%
  summarise(
    n_pair_years = n(),
    min_flow = min(flow_count),
    p50_flow = quantile(flow_count, 0.50),
    p90_flow = quantile(flow_count, 0.90),
    p99_flow = quantile(flow_count, 0.99),
    max_flow = max(flow_count),
    zero_flow_share = mean(flow_count == 0),
    negative_flow_count = sum(flow_count < 0),
    n_nonmissing_origin = sum(!is.na(origin_kyoku)),
    n_nonmissing_destination = sum(!is.na(destination_kyoku))
  )

draft_share_diagnostics <- pair_panel_full %>%
  mutate(draft_status = case_when(
    D_origin > 0 & D_destination > 0 ~ "both",
    D_origin > 0 & D_destination == 0 ~ "origin_only",
    D_origin == 0 & D_destination > 0 ~ "destination_only",
    TRUE ~ "neither"
  )) %>%
  count(year, draft_status, name = "n_pair_years") %>%
  group_by(year) %>%
  mutate(share = n_pair_years / sum(n_pair_years)) %>%
  ungroup()

model_diagnostics <- bind_rows(
  coef_table(baseline_model, "baseline_pair_year_fe", "nondraft_pairs") %>%
    distinct(model, sample, nobs, pseudo_r2, dispersion, status),
  coef_table(draft_model, "draft_pair_year_fe", "full_panel") %>%
    distinct(model, sample, nobs, pseudo_r2, dispersion, status),
  robustness_results %>% distinct(model, sample, nobs, pseudo_r2, dispersion, status)
) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(baseline_results, file.path(out_dir, "bilateral_flow_baseline_results.csv"))
write_csv(draft_results, file.path(out_dir, "bilateral_flow_draft_results.csv"))
write_csv(robustness_results, file.path(out_dir, "bilateral_flow_robustness_results.csv"))
write_csv(sample_summary, file.path(out_dir, "bilateral_flow_sample_summary.csv"))
write_csv(distance_diagnostics, file.path(out_dir, "bilateral_flow_distance_diagnostics.csv"))
write_csv(flow_diagnostics, file.path(out_dir, "bilateral_flow_distribution_diagnostics.csv"))
write_csv(draft_share_diagnostics, file.path(out_dir, "bilateral_flow_draft_share_diagnostics.csv"))
write_csv(model_diagnostics, file.path(out_dir, "bilateral_flow_model_diagnostics.csv"))
write_rds(pair_panel_full, file.path(out_dir, "bilateral_flow_pair_panel_full.rds"))
write_rds(pair_panel_nondraft, file.path(out_dir, "bilateral_flow_pair_panel_nondraft.rds"))

notes <- c(
  "Bilateral Transfer Flow PPML: construction notes",
  "",
  paste("Run date:", as.character(Sys.time())),
  paste("Master personnel panel:", DATA_PATH),
  "",
  "Office definition:",
  "- The operational office is kyoku x ka after kyoku normalization and the 1944 TokyoTo ka/facility/kakari backfill used in the SlackDistribution cleanup scripts.",
  "- Kakari was not used in the office identifier because of the known coverage gaps; this remains flagged for substantive review.",
  "",
  "Pair-year construction:",
  "- Years are 1938-1945. A transfer is a staff_id observed at a different kyoku x ka office in year t than in t-1.",
  "- Within-office moves are excluded. The estimation panel includes zero-flow origin-destination pairs.",
  "- Pairs with missing origin or destination kyoku classification after cleanup are dropped.",
  "",
  "Feasibility and size:",
  "- The primary feasibility filter keeps origin-destination pairs whose offices share at least one non-drafted occupation x rank cell in t-1.",
  "- log_N_o and log_N_d are log counts of non-drafted workers in the shared occupation x rank cells at the origin and destination in t-1.",
  "- The exact-position feasibility variant is not substituted automatically; it is flagged for review as requested.",
  "",
  "Occupational similarity:",
  "- Continuous similarity is cosine similarity of non-drafted position-composition vectors in t-1.",
  "- The robustness specification uses an indicator for cosine similarity greater than 0.5.",
  "",
  "Distance:",
  "- same_section is kyoku x ka equality and is excluded as within-office.",
  "- same_department is same cleaned kyoku but different ka.",
  "- different_department is different cleaned kyoku.",
  "- Because same_section pairs are excluded, the same_department and different_department dummies have no observed same_section baseline in the final flow panel. With pair fixed effects, distance main effects are identified only where classifications change over time.",
  "",
  "Draft shocks:",
  "- D_origin and D_destination are male draftee counts at the origin/destination kyoku x ka office in year t.",
  "- The main draft model includes origin and destination draft variables jointly. The indicator robustness replaces counts with any-draft indicators.",
  "",
  "Fixed effects and clustering:",
  "- Main models use pair_id and year fixed effects.",
  "- Robustness includes origin_office x year and destination_office x year fixed effects.",
  "- Standard errors are clustered two-way by origin and destination offices throughout.",
  "",
  "Review flags from prompt:",
  "1. Confirm kyoku x ka as the operational office level.",
  "2. Confirm occupation x rank feasibility as primary versus exact-position feasibility.",
  "3. Confirm use of year fixed effects in pair-FE specifications.",
  "4. Confirm treatment of office identifier changes across the 1943 merger via cleaned reference table mappings.",
  "",
  "Diagnostics:",
  paste("- Flow distribution diagnostic file:", "bilateral_flow_distribution_diagnostics.csv"),
  paste("- Distance positive-flow diagnostic file:", "bilateral_flow_distance_diagnostics.csv"),
  paste("- Draft share diagnostic file:", "bilateral_flow_draft_share_diagnostics.csv"),
  paste("- Model convergence/warning diagnostic file:", "bilateral_flow_model_diagnostics.csv"),
  if (length(fit_warnings) == 0L) "- No fepois warnings were captured." else paste("- Captured fepois warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "- No fepois errors were captured." else paste("- Captured fepois errors:", paste(fit_errors, collapse = " | "))
)

writeLines(notes, file.path(out_dir, "bilateral_flow_specification_notes.txt"))

cat("Finished. Results written to: ", out_dir, "\n", sep = "")
