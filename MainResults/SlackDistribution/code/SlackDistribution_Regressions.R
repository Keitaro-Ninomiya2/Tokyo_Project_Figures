################################################################################
# Slack Distribution and Replacement Margins
#
# Unit: destination office x section (ka group) x year, 1938--1945.
# Goal: test whether lagged nearby slack predicts whether draft-impacted units
# rely on transfers or external hires.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")
description_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "descriptions")

dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(description_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")

years_of_interest <- 1938:1945
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
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                    ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")         ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                    ~ 2L
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

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.10 ~ "$^{*}$",
    TRUE ~ ""
  )
}

cat("Loading data...\n")

df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
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

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
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

df_names <- df_names %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_all <- df_all %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

section_year_panel <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    )
  ) %>%
  group_by(section_id, office_id, year_num) %>%
  summarise(
    ka = first(na.omit(ka)),
    ka_group = first(na.omit(ka_group)),
    norm_kyoku = first(na.omit(norm_kyoku)),
    kyoku_group = first(na.omit(kyoku_group)),
    n_workers = n(),
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

section_hiring_lag <- section_year_panel %>%
  transmute(
    origin_section_id = section_id,
    origin_office_id = office_id,
    year_num = year_num + 1L,
    origin_prior_hiring = n_new_hires
  )

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_gov_level = gov_level,
    lag_norm_kyoku = norm_kyoku,
    lag_kyoku_group = kyoku_group,
    lag_ka = ka,
    lag_ka_group = ka_group,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_occupation = occupation,
    lag_section_id = section_id
  ) %>%
  mutate(year_num = year_num + 1L)

current_status <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  transmute(
    staff_id, year_num,
    current_office_id = office_id,
    current_section_id = section_id,
    current_gov_level = gov_level,
    current_drafted = drafted,
    current_observed = 1L
  )

worker_arrivals <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    ),
    arrival_type = arrival_distance(
      kyoku_group, ka_group, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id
  )

prewar_arrivals <- df_names %>%
  filter(year_num %in% prewar_years) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    arrival_type = arrival_distance(
      kyoku_group, ka_group, ka,
      lag_kyoku_group, lag_ka_group, lag_ka
    ),
    is_transfer_in = !is.na(lag_office_id) & lag_office_id != office_id
  )

prewar_diffdept_pairs <- prewar_arrivals %>%
  filter(
    is_transfer_in,
    arrival_type == "different_department",
    !is.na(kyoku_group),
    !is.na(lag_kyoku_group)
  ) %>%
  transmute(
    kyoku_a = pmin(kyoku_group, lag_kyoku_group),
    kyoku_b = pmax(kyoku_group, lag_kyoku_group)
  ) %>%
  distinct() %>%
  mutate(prewar_pair_allowed = 1L)

outcomes_section <- worker_arrivals %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(section_id, office_id, year_num) %>%
  summarise(
    ka = first(na.omit(ka)),
    ka_group = first(na.omit(ka_group)),
    norm_kyoku = first(na.omit(norm_kyoku)),
    kyoku_group = first(na.omit(kyoku_group)),
    transfers_in = sum(is_transfer_in, na.rm = TRUE),
    transfers_same_section = sum(is_transfer_in & arrival_type == "same_section", na.rm = TRUE),
    transfers_same_department = sum(is_transfer_in & arrival_type == "same_department", na.rm = TRUE),
    transfers_different_department = sum(is_transfer_in & arrival_type == "different_department", na.rm = TRUE),
    new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

drafted_profiles <- df_all %>%
  filter(
    year_num %in% years_of_interest,
    drafted == TRUE,
    !is_female,
    !is.na(section_id),
    !is.na(pos_norm),
    !is.na(occupation),
    !is.na(pos_rank)
  ) %>%
  group_by(section_id, office_id, year_num, ka, ka_group, norm_kyoku, kyoku_group, pos_norm, occupation, pos_rank) %>%
  summarise(n_drafted_profile = n(), .groups = "drop")

drafted_section_totals <- drafted_profiles %>%
  group_by(section_id, office_id, year_num, ka, ka_group, norm_kyoku, kyoku_group) %>%
  summarise(n_drafted_male = sum(n_drafted_profile), .groups = "drop")

actual_transfer_origins <- worker_arrivals %>%
  filter(is_transfer_in) %>%
  inner_join(
    drafted_section_totals %>%
      select(section_id, year_num, dest_drafted_male = n_drafted_male, dest_kyoku_group = kyoku_group),
    by = c("section_id", "year_num")
  ) %>%
  mutate(
    origin_kyoku_group = lag_kyoku_group
  )

na_transfer_diagnostic <- actual_transfer_origins %>%
  filter(is.na(arrival_type)) %>%
  mutate(
    na_reason = case_when(
      is.na(lag_kyoku_group) & is.na(kyoku_group) ~ "missing_both_kyoku_groups",
      is.na(lag_kyoku_group) & !is.na(kyoku_group) ~ "missing_origin_kyoku_group",
      !is.na(lag_kyoku_group) & is.na(kyoku_group) ~ "missing_destination_kyoku_group",
      !is.na(lag_office_id) & lag_office_id == office_id ~ "same_office_unclassified",
      TRUE ~ "other_unclassified"
    ),
    gov_transition = paste(replace_na(lag_gov_level, "missing"), replace_na(gov_level, "missing"), sep = " -> "),
    merger_boundary = as.integer(year_num == 1944L)
  )

na_failure_mode_summary <- na_transfer_diagnostic %>%
  mutate(
    has_origin_kyoku_string = !is.na(lag_norm_kyoku) | (!is.na(lag_ka) & lag_ka != ""),
    has_dest_kyoku_string = !is.na(norm_kyoku) | (!is.na(ka) & ka != ""),
    origin_norm_present = !is.na(lag_norm_kyoku),
    dest_norm_present = !is.na(norm_kyoku),
    origin_group_assigned = !is.na(lag_kyoku_group),
    dest_group_assigned = !is.na(kyoku_group)
  ) %>%
  count(
    has_origin_kyoku_string,
    has_dest_kyoku_string,
    origin_norm_present,
    dest_norm_present,
    origin_group_assigned,
    dest_group_assigned,
    sort = TRUE,
    name = "n_transfers"
  ) %>%
  mutate(share = n_transfers / sum(n_transfers))

na_failure_mode_tokyofu_summary <- na_transfer_diagnostic %>%
  filter(gov_transition == "TokyoFu -> TokyoFu") %>%
  mutate(
    has_origin_kyoku_string = !is.na(lag_norm_kyoku) | (!is.na(lag_ka) & lag_ka != ""),
    has_dest_kyoku_string = !is.na(norm_kyoku) | (!is.na(ka) & ka != ""),
    origin_norm_present = !is.na(lag_norm_kyoku),
    dest_norm_present = !is.na(norm_kyoku),
    origin_group_assigned = !is.na(lag_kyoku_group),
    dest_group_assigned = !is.na(kyoku_group)
  ) %>%
  count(
    has_origin_kyoku_string,
    has_dest_kyoku_string,
    origin_norm_present,
    dest_norm_present,
    origin_group_assigned,
    dest_group_assigned,
    sort = TRUE,
    name = "n_transfers"
  ) %>%
  mutate(share = n_transfers / sum(n_transfers))

na_unmatched_origin_strings <- na_transfer_diagnostic %>%
  filter(is.na(lag_kyoku_group)) %>%
  count(lag_norm_kyoku, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

na_unmatched_dest_strings <- na_transfer_diagnostic %>%
  filter(is.na(kyoku_group)) %>%
  count(norm_kyoku, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

na_reason_summary <- na_transfer_diagnostic %>%
  count(na_reason, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

na_year_summary <- na_transfer_diagnostic %>%
  count(year_num, sort = FALSE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

na_gov_summary <- na_transfer_diagnostic %>%
  count(gov_transition, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

na_merger_summary <- na_transfer_diagnostic %>%
  summarise(
    n_na_transfers = n(),
    share_1944 = mean(merger_boundary),
    share_missing_origin_kyoku_group = mean(is.na(lag_kyoku_group)),
    share_missing_destination_kyoku_group = mean(is.na(kyoku_group))
  )

origin_distance_summary <- actual_transfer_origins %>%
  count(arrival_type, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

diffdept_origin_concentration <- actual_transfer_origins %>%
  filter(arrival_type == "different_department", !is.na(origin_kyoku_group)) %>%
  count(section_id, year_num, origin_kyoku_group, name = "n_from_origin") %>%
  group_by(section_id, year_num) %>%
  summarise(
    total_diffdept_transfers = sum(n_from_origin),
    n_distinct_origin_kyoku = n(),
    top_origin_share = max(n_from_origin) / sum(n_from_origin),
    .groups = "drop"
  )

diffdept_origin_concentration_summary <- diffdept_origin_concentration %>%
  summarise(
    n_dest_section_years = n(),
    mean_distinct_origin_kyoku = mean(n_distinct_origin_kyoku),
    median_distinct_origin_kyoku = median(n_distinct_origin_kyoku),
    mean_top_origin_share = mean(top_origin_share),
    median_top_origin_share = median(top_origin_share)
  )

diffdept_pair_counts <- actual_transfer_origins %>%
  filter(arrival_type == "different_department", !is.na(origin_kyoku_group), !is.na(dest_kyoku_group)) %>%
  count(dest_kyoku_group, origin_kyoku_group, sort = TRUE, name = "n_transfers")

candidate_workers <- staff_lag %>%
  filter(
    year_num %in% years_of_interest,
    !is.na(lag_office_id),
    !is.na(lag_occupation),
    !is.na(lag_pos_rank),
    lag_occupation != "engineer"
  ) %>%
  left_join(current_status, by = c("staff_id", "year_num")) %>%
  left_join(section_hiring_lag, by = c("lag_section_id" = "origin_section_id", "lag_office_id" = "origin_office_id", "year_num")) %>%
  mutate(origin_prior_hiring = replace_na(origin_prior_hiring, 0L)) %>%
  filter(current_observed == 1L, is.na(current_drafted) | current_drafted != TRUE, origin_prior_hiring > 0)

cat("Constructing matched slack pools...\n")

slack_matches <- drafted_profiles %>%
  rename(
    dest_section_id = section_id,
    dest_office_id = office_id,
    dest_ka = ka,
    dest_ka_group = ka_group,
    dest_norm_kyoku = norm_kyoku,
    dest_kyoku_group = kyoku_group,
    dest_pos_norm = pos_norm,
    dest_occupation = occupation,
    dest_pos_rank = pos_rank
  ) %>%
  inner_join(
    candidate_workers,
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

slack_counts <- slack_matches %>%
  count(dest_section_id, year_num, slack_distance, name = "n_slack") %>%
  pivot_wider(
    names_from = slack_distance,
    values_from = n_slack,
    values_fill = 0,
    names_prefix = "slack_"
  )

panel <- drafted_section_totals %>%
  left_join(outcomes_section, by = c("section_id", "office_id", "year_num", "ka", "ka_group", "norm_kyoku", "kyoku_group")) %>%
  left_join(section_year_panel %>% select(section_id, office_id, year_num, n_workers), by = c("section_id", "office_id", "year_num")) %>%
  left_join(slack_counts, by = c("section_id" = "dest_section_id", "year_num")) %>%
  mutate(
    across(
      c(
        transfers_in, transfers_same_section, transfers_same_department,
        transfers_different_department, new_hires,
        slack_same_section, slack_same_department, slack_different_department
      ),
      ~replace_na(., 0)
    ),
    transfer_share = if_else(transfers_in + new_hires > 0,
                             transfers_in / (transfers_in + new_hires),
                             NA_real_),
    any_slack_same_section = as.integer(slack_same_section > 0),
    any_slack_same_department = as.integer(slack_same_department > 0),
    any_slack_different_department = as.integer(slack_different_department > 0),
    log_slack_same_section = log(slack_same_section + 1),
    log_slack_same_department = log(slack_same_department + 1),
    log_slack_different_department = log(slack_different_department + 1),
    slack_same_section_per_draft = slack_same_section / n_drafted_male,
    slack_same_department_per_draft = slack_same_department / n_drafted_male,
    slack_different_department_per_draft = slack_different_department / n_drafted_male,
    log_slack_same_section_per_draft = log(slack_same_section_per_draft + 1),
    log_slack_same_department_per_draft = log(slack_same_department_per_draft + 1),
    log_slack_different_department_per_draft = log(slack_different_department_per_draft + 1),
    log_section_size = log(n_workers + 1)
  ) %>%
  filter(n_drafted_male > 0, !is.na(norm_kyoku))

cat("Draft-impacted section-years:", nrow(panel), "\n")
cat("Mean transfers_in:", round(mean(panel$transfers_in), 3), "\n")
cat("Mean new_hires:", round(mean(panel$new_hires), 3), "\n")
cat("Mean transfer_share (positive replacement cells):", round(mean(panel$transfer_share, na.rm = TRUE), 3), "\n")

descriptive_summary <- panel %>%
  summarise(
    n = n(),
    mean_drafts = mean(n_drafted_male),
    mean_transfers = mean(transfers_in),
    mean_hires = mean(new_hires),
    mean_transfer_share = mean(transfer_share, na.rm = TRUE),
    mean_slack_same_section = mean(slack_same_section),
    mean_slack_same_department = mean(slack_same_department),
    mean_slack_different_department = mean(slack_different_department),
    share_any_slack_same_section = mean(any_slack_same_section),
    share_any_slack_same_department = mean(any_slack_same_department),
    share_any_slack_different_department = mean(any_slack_different_department),
    p50_slack_same_section = median(slack_same_section),
    p50_slack_same_department = median(slack_same_department),
    p50_slack_different_department = median(slack_different_department)
  )

by_distance_summary <- panel %>%
  summarise(
    same_section_mean = mean(slack_same_section),
    same_department_mean = mean(slack_same_department),
    different_department_mean = mean(slack_different_department),
    same_section_sd = sd(slack_same_section),
    same_department_sd = sd(slack_same_department),
    different_department_sd = sd(slack_different_department)
  ) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value")

diagnostic_summary <- tibble(
  diagnostic = c(
    "pool_ratio_sameDept_to_sameSection_mean",
    "pool_ratio_diffDept_to_sameSection_mean",
    "median_same_section_slack",
    "share_any_same_section_slack",
    "corr_draft_log_same_section",
    "corr_draft_log_same_department",
    "corr_draft_log_different_department"
  ),
  value = c(
    mean(panel$slack_same_department) / max(mean(panel$slack_same_section), 1e-8),
    mean(panel$slack_different_department) / max(mean(panel$slack_same_section), 1e-8),
    median(panel$slack_same_section),
    mean(panel$any_slack_same_section),
    suppressWarnings(cor(panel$n_drafted_male, panel$log_slack_same_section)),
    suppressWarnings(cor(panel$n_drafted_male, panel$log_slack_same_department)),
    suppressWarnings(cor(panel$n_drafted_male, panel$log_slack_different_department))
  )
)

run_feols <- function(depvar, ratio = FALSE) {
  rhs <- if (!ratio) {
    "any_slack_same_section + log_slack_same_section + log_slack_same_department + log_slack_different_department + n_drafted_male + log_section_size"
  } else {
    "any_slack_same_section + log_slack_same_section_per_draft + log_slack_same_department_per_draft + log_slack_different_department_per_draft + n_drafted_male + log_section_size"
  }
  feols(
    as.formula(paste0(depvar, " ~ ", rhs, " | year_num + norm_kyoku")),
    data = panel %>% filter(!(depvar == "transfer_share" & is.na(transfer_share))),
    cluster = ~office_id
  )
}

models_level <- list(
  transfers_in = run_feols("transfers_in"),
  new_hires = run_feols("new_hires"),
  transfer_share = run_feols("transfer_share")
)

models_ratio <- list(
  transfers_in = run_feols("transfers_in", ratio = TRUE),
  new_hires = run_feols("new_hires", ratio = TRUE),
  transfer_share = run_feols("transfer_share", ratio = TRUE)
)

extract_results <- function(model_list, spec_label) {
  imap_dfr(model_list, function(mod, outcome) {
    tidy(mod) %>%
      filter(term %in% c(
        "any_slack_same_section",
        "log_slack_same_section",
        "log_slack_same_department",
        "log_slack_different_department",
        "log_slack_same_section_per_draft",
        "log_slack_same_department_per_draft",
        "log_slack_different_department_per_draft",
        "n_drafted_male",
        "log_section_size"
      )) %>%
      mutate(
        outcome = outcome,
        specification = spec_label,
        n = nobs(mod)
      )
  })
}

results_long <- bind_rows(
  extract_results(models_level, "level_slack"),
  extract_results(models_ratio, "ratio_slack")
)

write_csv(descriptive_summary, file.path(result_dir, "slack_distribution_summary.csv"))
write_csv(by_distance_summary, file.path(result_dir, "slack_distribution_by_distance.csv"))
write_csv(diagnostic_summary, file.path(result_dir, "slack_distribution_diagnostics.csv"))
write_csv(panel, file.path(result_dir, "slack_distribution_panel.csv"))
write_csv(results_long, file.path(result_dir, "slack_distribution_regression_results.csv"))
write_csv(origin_distance_summary, file.path(result_dir, "slack_distribution_actual_transfer_origin_shares.csv"))
write_csv(diffdept_origin_concentration_summary, file.path(result_dir, "slack_distribution_diffdept_origin_concentration_summary.csv"))
write_csv(diffdept_pair_counts, file.path(result_dir, "slack_distribution_diffdept_origin_pair_counts.csv"))
write_csv(na_reason_summary, file.path(result_dir, "slack_distribution_na_transfer_reasons.csv"))
write_csv(na_year_summary, file.path(result_dir, "slack_distribution_na_transfer_years.csv"))
write_csv(na_gov_summary, file.path(result_dir, "slack_distribution_na_transfer_gov_transitions.csv"))
write_csv(na_merger_summary, file.path(result_dir, "slack_distribution_na_transfer_merger_summary.csv"))
write_csv(na_failure_mode_summary, file.path(result_dir, "slack_distribution_na_failure_modes.csv"))
write_csv(na_failure_mode_tokyofu_summary, file.path(result_dir, "slack_distribution_na_failure_modes_tokyofu.csv"))
write_csv(na_unmatched_origin_strings, file.path(result_dir, "slack_distribution_na_unmatched_origin_kyoku.csv"))
write_csv(na_unmatched_dest_strings, file.path(result_dir, "slack_distribution_na_unmatched_dest_kyoku.csv"))

level_core <- results_long %>%
  filter(specification == "level_slack",
         term %in% c("any_slack_same_section", "log_slack_same_section", "log_slack_same_department", "log_slack_different_department")) %>%
  select(outcome, term, estimate, std.error, p.value)

cat("\nCore coefficient pattern (level slack):\n")
print(level_core, n = Inf)
cat("\nSlack diagnostics:\n")
print(diagnostic_summary, n = Inf)
cat("\nActual transfer origin shares:\n")
print(origin_distance_summary, n = Inf)
cat("\nDifferent-department origin concentration:\n")
print(diffdept_origin_concentration_summary, n = Inf)
cat("\nNA transfer diagnostic: reasons\n")
print(na_reason_summary, n = Inf)
cat("\nNA transfer diagnostic: merger/government summary\n")
print(na_merger_summary, n = Inf)
cat("\nNA transfer diagnostic: failure modes\n")
print(na_failure_mode_summary, n = Inf)
cat("\nNA transfer diagnostic: TokyoFu -> TokyoFu failure modes\n")
print(na_failure_mode_tokyofu_summary, n = Inf)

check_monotone <- function(dat, outcome_name, positive = TRUE) {
  sub <- dat %>%
    filter(outcome == outcome_name) %>%
    filter(term %in% c("log_slack_same_section", "log_slack_same_department", "log_slack_different_department")) %>%
    mutate(order = match(term, c("log_slack_same_section", "log_slack_same_department", "log_slack_different_department"))) %>%
    arrange(order)
  if (nrow(sub) != 3) return(FALSE)
  est <- sub$estimate
  if (positive) {
    est[1] > est[2] & est[2] > est[3]
  } else {
    est[1] < est[2] & est[2] < est[3]
  }
}

transfers_monotone <- check_monotone(level_core, "transfers_in", positive = TRUE)
hires_mirror <- check_monotone(level_core, "new_hires", positive = FALSE)

framework_lines <- c(
  "Slack Distribution Framework",
  "",
  "Draft-impacted sections choose between internal sourcing and external hiring.",
  "Internal sourcing is cheaper when there are non-drafted, non-scarce workers in nearby sections that are plausible rank-adjacent substitutes and come from sections with prior hiring activity.",
  "The relevant constraint is not whether transfers are possible in the abstract, but how the available slack pool is distributed across organizational distance.",
  "Same-section slack should be the cheapest margin, same-department slack the next cheapest, and different-department slack the most costly internal margin; external hiring fills the residual when internal slack is thin.",
  "The empirical 2:1 transfer-to-hire ratio is therefore an aggregate outcome of section-level optimization against this distance-specific slack distribution.",
  "In the regressions, the key prediction is a declining coefficient profile for transfers as slack moves from same-section to same-department to different-department sources, with hiring absorbing vacancies when nearby slack is unavailable."
)

if (transfers_monotone && hires_mirror) {
  framework_lines <- c(
    framework_lines,
    "",
    "The estimated coefficient pattern matches this mechanism: nearby slack predicts more transfers, while hires become relatively more important when the available slack pool is farther away."
  )
} else {
  framework_lines <- c(
    framework_lines,
    "",
    "The estimated coefficient pattern only partially matches a simple monotone-distance mechanism.",
    "The emerging evidence instead points to a threshold role for same-section slack and a distinction between same-department slack, which appears to be absorbed through non-transfer adjustments, and different-department slack, which appears more closely tied to formal transfer reliance.",
    "The framework should therefore emphasize that the form of adjustment depends on where slack is located, rather than presuming a single declining distance gradient."
  )
}

writeLines(framework_lines, file.path(description_dir, "SlackDistribution_Framework.txt"))

build_table <- function(model_list, spec_label, outfile, title) {
  key_terms <- if (spec_label == "level_slack") {
    c("any_slack_same_section", "log_slack_same_section", "log_slack_same_department", "log_slack_different_department", "n_drafted_male", "log_section_size")
  } else {
    c("any_slack_same_section", "log_slack_same_section_per_draft", "log_slack_same_department_per_draft", "log_slack_different_department_per_draft", "n_drafted_male", "log_section_size")
  }
  labels <- c(
    any_slack_same_section = "Any slack: same section",
    log_slack_same_section = "log(slack same section + 1)",
    log_slack_same_department = "log(slack same dept. + 1)",
    log_slack_different_department = "log(slack diff. dept. + 1)",
    log_slack_same_section_per_draft = "log(slack/draft same section + 1)",
    log_slack_same_department_per_draft = "log(slack/draft same dept. + 1)",
    log_slack_different_department_per_draft = "log(slack/draft diff. dept. + 1)",
    n_drafted_male = "No. drafted workers",
    log_section_size = "log(section size + 1)"
  )
  cols <- c("transfers_in", "new_hires", "transfer_share")
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", title, "}"),
    "\\small",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & Transfers in & New hires & Transfer share \\\\",
    "\\midrule"
  )
  for (term in key_terms) {
    coef_row <- c(labels[[term]])
    se_row <- c(" ")
    for (col in cols) {
      tt <- tidy(model_list[[col]]) %>% filter(term == !!term)
      coef_row <- c(coef_row, if (nrow(tt) == 0) "" else paste0(fmt_num(tt$estimate), stars(tt$p.value)))
      se_row <- c(se_row, if (nrow(tt) == 0) "" else paste0("(", fmt_num(tt$std.error), ")"))
    }
    lines <- c(
      lines,
      paste0(coef_row[1], " & ", paste(coef_row[-1], collapse = " & "), " \\\\"),
      paste0(se_row[1], " & ", paste(se_row[-1], collapse = " & "), " \\\\[3pt]")
    )
  }
  n_row <- paste0(
    "Observations & ",
    paste(vapply(cols, function(col) format(nobs(model_list[[col]]), big.mark = ","), character(1)), collapse = " & "),
    " \\\\"
  )
  lines <- c(
    lines,
    "\\midrule",
    n_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Unit of observation: draft-impacted destination office $\\times$ section $\\times$ year, 1938--1945. Slack pools are measured from the prior year and count non-drafted, non-engineer workers in origin sections with positive prior hiring activity whose exact position title matches the drafted destination seat and whose position rank is at most one step from that seat. Same-section slack uses the same section group in a different office; same-department slack uses the same department group but a different section group; different-department slack uses different department groups. The same-section margin enters both as an indicator for any nearby slack and as a logged count. All regressions include year and normalized department fixed effects and cluster standard errors by destination office.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lines, outfile)
}

build_table(
  models_level,
  "level_slack",
  file.path(result_dir, "SlackDistribution_LevelRegressions.tex"),
  "Slack Distribution and Replacement Margins"
)

build_table(
  models_ratio,
  "ratio_slack",
  file.path(result_dir, "SlackDistribution_RatioRegressions.tex"),
  "Slack Distribution and Replacement Margins: Per-Draft Slack"
)

cat("\nExports:\n")
cat("  ", file.path(result_dir, "slack_distribution_summary.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_by_distance.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_diagnostics.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_regression_results.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_actual_transfer_origin_shares.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_diffdept_origin_concentration_summary.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_diffdept_origin_pair_counts.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_transfer_reasons.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_transfer_years.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_transfer_gov_transitions.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_transfer_merger_summary.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_failure_modes.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_failure_modes_tokyofu.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_unmatched_origin_kyoku.csv"), "\n")
cat("  ", file.path(result_dir, "slack_distribution_na_unmatched_dest_kyoku.csv"), "\n")
cat("  ", file.path(result_dir, "SlackDistribution_LevelRegressions.tex"), "\n")
cat("  ", file.path(result_dir, "SlackDistribution_RatioRegressions.tex"), "\n")
cat("  ", file.path(description_dir, "SlackDistribution_Framework.txt"), "\n")
