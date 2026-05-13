################################################################################
# Diagnose kyoku normalization failures among transfer observations into
# draft-impacted sections.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")

years_of_interest <- 1938:1945

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

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

df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku)
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
    kyoku_group = assign_kyoku_group(norm_kyoku)
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

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_gov_level = gov_level,
    lag_kyoku_raw = kyoku,
    lag_norm_kyoku = norm_kyoku,
    lag_kyoku_group = kyoku_group,
    lag_ka = ka,
    lag_ka_group = ka_group,
    lag_position = position
  ) %>%
  mutate(year_num = year_num + 1L)

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

drafted_section_totals <- df_all %>%
  filter(
    year_num %in% years_of_interest,
    drafted == TRUE,
    !is_female,
    !is.na(section_id)
  ) %>%
  group_by(section_id, year_num) %>%
  summarise(n_drafted_male = n(), .groups = "drop")

actual_transfer_origins <- worker_arrivals %>%
  filter(is_transfer_in) %>%
  inner_join(
    drafted_section_totals,
    by = c("section_id", "year_num")
  )

na_transfer_diagnostic <- actual_transfer_origins %>%
  filter(is.na(arrival_type)) %>%
  mutate(
    gov_transition = paste(replace_na(lag_gov_level, "missing"), replace_na(gov_level, "missing"), sep = " -> "),
    dest_raw_present = !is.na(kyoku) & str_squish(kyoku) != "",
    origin_raw_present = !is.na(lag_kyoku_raw) & str_squish(lag_kyoku_raw) != "",
    dest_norm_missing_with_raw = dest_raw_present & is.na(norm_kyoku),
    origin_norm_missing_with_raw = origin_raw_present & is.na(lag_norm_kyoku),
    dest_norm_missing_no_raw = !dest_raw_present & is.na(norm_kyoku),
    origin_norm_missing_no_raw = !origin_raw_present & is.na(lag_norm_kyoku)
  )

dest_unmapped_counts <- na_transfer_diagnostic %>%
  filter(dest_norm_missing_with_raw) %>%
  count(kyoku, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

origin_unmapped_counts <- na_transfer_diagnostic %>%
  filter(origin_norm_missing_with_raw) %>%
  count(lag_kyoku_raw, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

dest_unmapped_sample <- na_transfer_diagnostic %>%
  filter(dest_norm_missing_with_raw) %>%
  transmute(
    side = "destination",
    staff_id,
    year_num,
    gov_transition,
    office_id,
    kyoku_raw = kyoku,
    ka,
    position
  ) %>%
  distinct() %>%
  slice_head(n = 25)

origin_unmapped_sample <- na_transfer_diagnostic %>%
  filter(origin_norm_missing_with_raw) %>%
  transmute(
    side = "origin",
    staff_id,
    year_num,
    gov_transition,
    office_id = lag_office_id,
    kyoku_raw = lag_kyoku_raw,
    ka = lag_ka,
    position = lag_position
  ) %>%
  distinct() %>%
  slice_head(n = 25)

tokyofu_origin_unmapped_counts <- na_transfer_diagnostic %>%
  filter(gov_transition == "TokyoFu -> TokyoFu", origin_norm_missing_with_raw) %>%
  count(lag_kyoku_raw, sort = TRUE, name = "n_transfers") %>%
  mutate(share = n_transfers / sum(n_transfers))

write_csv(dest_unmapped_counts, file.path(result_dir, "kyoku_norm_fail_destination_counts.csv"))
write_csv(origin_unmapped_counts, file.path(result_dir, "kyoku_norm_fail_origin_counts.csv"))
write_csv(bind_rows(dest_unmapped_sample, origin_unmapped_sample),
          file.path(result_dir, "kyoku_norm_fail_samples.csv"))
write_csv(tokyofu_origin_unmapped_counts,
          file.path(result_dir, "kyoku_norm_fail_tokyofu_origin_counts.csv"))

summary_table <- tibble(
  metric = c(
    "na_transfer_obs",
    "dest_norm_missing_with_raw",
    "origin_norm_missing_with_raw",
    "dest_norm_missing_no_raw",
    "origin_norm_missing_no_raw"
  ),
  value = c(
    nrow(na_transfer_diagnostic),
    sum(na_transfer_diagnostic$dest_norm_missing_with_raw),
    sum(na_transfer_diagnostic$origin_norm_missing_with_raw),
    sum(na_transfer_diagnostic$dest_norm_missing_no_raw),
    sum(na_transfer_diagnostic$origin_norm_missing_no_raw)
  )
)

write_csv(summary_table, file.path(result_dir, "kyoku_norm_fail_summary.csv"))

cat("Wrote kyoku normalization diagnostics to:\n")
cat(" -", file.path(result_dir, "kyoku_norm_fail_summary.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_norm_fail_destination_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_norm_fail_origin_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_norm_fail_tokyofu_origin_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_norm_fail_samples.csv"), "\n")
