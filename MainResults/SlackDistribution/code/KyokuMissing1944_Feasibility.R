################################################################################
# Feasibility check for TOC-based kyoku backfill on 1944 NA transfer rows.
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
    lag_kakari = kakari,
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

na_transfer_1944 <- worker_arrivals %>%
  filter(year_num == 1944, is_transfer_in) %>%
  inner_join(drafted_section_totals, by = c("section_id", "year_num")) %>%
  filter(is.na(arrival_type)) %>%
  mutate(
    gov_transition = paste(replace_na(lag_gov_level, "missing"), replace_na(gov_level, "missing"), sep = " -> "),
    dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == "",
    origin_kyoku_missing = is.na(lag_kyoku_raw) | str_squish(replace_na(lag_kyoku_raw, "")) == "",
    dest_ka_present = !is.na(ka) & str_squish(ka) != "",
    dest_kakari_present = !is.na(kakari) & str_squish(kakari) != "",
    dest_position_present = !is.na(position) & str_squish(position) != "",
    origin_ka_present = !is.na(lag_ka) & str_squish(lag_ka) != "",
    origin_kakari_present = !is.na(lag_kakari) & str_squish(lag_kakari) != "",
    origin_position_present = !is.na(lag_position) & str_squish(lag_position) != ""
  )

summary_table <- tibble(
  metric = c(
    "n_1944_na_transfers",
    "n_dest_kyoku_missing",
    "n_origin_kyoku_missing",
    "share_dest_ka_present_when_dest_kyoku_missing",
    "share_dest_kakari_present_when_dest_kyoku_missing",
    "share_dest_position_present_when_dest_kyoku_missing",
    "share_origin_ka_present_when_origin_kyoku_missing",
    "share_origin_kakari_present_when_origin_kyoku_missing",
    "share_origin_position_present_when_origin_kyoku_missing"
  ),
  value = c(
    nrow(na_transfer_1944),
    sum(na_transfer_1944$dest_kyoku_missing),
    sum(na_transfer_1944$origin_kyoku_missing),
    mean(na_transfer_1944$dest_ka_present[na_transfer_1944$dest_kyoku_missing]),
    mean(na_transfer_1944$dest_kakari_present[na_transfer_1944$dest_kyoku_missing]),
    mean(na_transfer_1944$dest_position_present[na_transfer_1944$dest_kyoku_missing]),
    mean(na_transfer_1944$origin_ka_present[na_transfer_1944$origin_kyoku_missing]),
    mean(na_transfer_1944$origin_kakari_present[na_transfer_1944$origin_kyoku_missing]),
    mean(na_transfer_1944$origin_position_present[na_transfer_1944$origin_kyoku_missing])
  )
)

dest_ka_counts <- na_transfer_1944 %>%
  filter(dest_kyoku_missing, dest_ka_present) %>%
  count(ka, sort = TRUE, name = "n_rows")

dest_kakari_counts <- na_transfer_1944 %>%
  filter(dest_kyoku_missing, dest_kakari_present) %>%
  count(kakari, sort = TRUE, name = "n_rows")

origin_ka_counts <- na_transfer_1944 %>%
  filter(origin_kyoku_missing, origin_ka_present) %>%
  count(lag_ka, sort = TRUE, name = "n_rows")

origin_kakari_counts <- na_transfer_1944 %>%
  filter(origin_kyoku_missing, origin_kakari_present) %>%
  count(lag_kakari, sort = TRUE, name = "n_rows")

dest_sample <- na_transfer_1944 %>%
  filter(dest_kyoku_missing) %>%
  transmute(
    year_num, gov_transition, office_id,
    kyoku, ka, kakari, position,
    lag_office_id, lag_kyoku_raw, lag_ka, lag_kakari, lag_position
  ) %>%
  slice_head(n = 50)

origin_sample <- na_transfer_1944 %>%
  filter(origin_kyoku_missing) %>%
  transmute(
    year_num, gov_transition, lag_office_id,
    lag_kyoku_raw, lag_ka, lag_kakari, lag_position,
    office_id, kyoku, ka, kakari, position
  ) %>%
  slice_head(n = 50)

write_csv(summary_table, file.path(result_dir, "kyoku_missing_1944_feasibility_summary.csv"))
write_csv(dest_ka_counts, file.path(result_dir, "kyoku_missing_1944_dest_ka_counts.csv"))
write_csv(dest_kakari_counts, file.path(result_dir, "kyoku_missing_1944_dest_kakari_counts.csv"))
write_csv(origin_ka_counts, file.path(result_dir, "kyoku_missing_1944_origin_ka_counts.csv"))
write_csv(origin_kakari_counts, file.path(result_dir, "kyoku_missing_1944_origin_kakari_counts.csv"))
write_csv(dest_sample, file.path(result_dir, "kyoku_missing_1944_dest_sample.csv"))
write_csv(origin_sample, file.path(result_dir, "kyoku_missing_1944_origin_sample.csv"))

cat("Wrote 1944 feasibility outputs to:\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_feasibility_summary.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_dest_ka_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_dest_kakari_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_origin_ka_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_origin_kakari_counts.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_dest_sample.csv"), "\n")
cat(" -", file.path(result_dir, "kyoku_missing_1944_origin_sample.csv"), "\n")
