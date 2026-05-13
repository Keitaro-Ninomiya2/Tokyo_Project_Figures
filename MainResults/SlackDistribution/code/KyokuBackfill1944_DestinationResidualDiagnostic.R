################################################################################
# Diagnose unresolved 1944 destination-side kyoku residuals after ka->kyoku
# backfill. Focus on facility-style ka labels and the no-ka / kakari fallback.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")

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

classify_suffix <- function(x) {
  case_when(
    str_detect(x, "修錬所$") ~ "training_center",
    str_detect(x, "保健所$") ~ "health_center",
    str_detect(x, "健康指導所$") ~ "health_guidance_center",
    str_detect(x, "診療所$") ~ "clinic",
    str_detect(x, "作業所$") ~ "workshop_office",
    str_detect(x, "作業場$") ~ "workshop_site",
    str_detect(x, "出張所$") ~ "branch_office",
    str_detect(x, "工場$") ~ "factory",
    str_detect(x, "学園$") ~ "academy",
    str_detect(x, "学校$") ~ "school",
    str_detect(x, "市場$") ~ "market",
    str_detect(x, "住宅") ~ "housing_unit",
    str_detect(x, "防疫所$") ~ "quarantine_office",
    str_detect(x, "裁縫場$|裁縫所$") ~ "sewing_site",
    str_detect(x, "課") ~ "composite_or_ka_like",
    TRUE ~ "other"
  )
}

df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names <- df_names %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(
    kyoku_group = assign_kyoku_group(norm_kyoku),
    section_id = section_key(office_id, ka_group, ka)
  )

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
    lag_kyoku_group = kyoku_group,
    lag_ka = ka,
    lag_ka_group = ka_group
  ) %>%
  mutate(year_num = year_num + 1L)

worker_arrivals <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
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

drafted_section_totals <- df_names %>%
  filter(
    year_num %in% years_of_interest,
    drafted == TRUE,
    gender_modern != "female",
    !is.na(section_id)
  ) %>%
  group_by(section_id, year_num) %>%
  summarise(n_drafted_male = n(), .groups = "drop")

na_transfer_1944 <- worker_arrivals %>%
  filter(year_num == 1944, is_transfer_in) %>%
  inner_join(drafted_section_totals, by = c("section_id", "year_num")) %>%
  filter(is.na(arrival_type)) %>%
  mutate(
    dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == "",
    dest_ka_present = !is.na(ka) & str_squish(ka) != "",
    dest_kakari_present = !is.na(kakari) & str_squish(kakari) != ""
  )

dest_backfill <- read_csv(
  file.path(result_dir, "tokyoto_1944_dest_ka_backfill_rows.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    unresolved = is.na(recovered_kyoku),
    suffix_group = classify_suffix(ka)
  )

facility_distribution <- dest_backfill %>%
  filter(unresolved) %>%
  count(ka, suffix_group, sort = TRUE, name = "n_rows")

suffix_summary <- facility_distribution %>%
  group_by(suffix_group) %>%
  summarise(
    n_rows = sum(n_rows),
    n_unique_labels = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rows))

no_ka_rows <- na_transfer_1944 %>%
  filter(dest_kyoku_missing, !dest_ka_present) %>%
  mutate(
    kakari_suffix_group = classify_suffix(replace_na(kakari, "")),
    position_present = !is.na(position) & str_squish(position) != ""
  )

no_ka_summary <- tibble(
  metric = c(
    "n_dest_missing_no_ka",
    "share_kakari_present",
    "share_position_present"
  ),
  value = c(
    nrow(no_ka_rows),
    mean(no_ka_rows$dest_kakari_present),
    mean(no_ka_rows$position_present)
  )
)

no_ka_top_kakari <- no_ka_rows %>%
  filter(dest_kakari_present) %>%
  count(kakari, sort = TRUE, name = "n_rows")

write_csv(facility_distribution,
          file.path(result_dir, "tokyoto_1944_dest_unresolved_facility_distribution.csv"))
write_csv(suffix_summary,
          file.path(result_dir, "tokyoto_1944_dest_unresolved_suffix_summary.csv"))
write_csv(no_ka_summary,
          file.path(result_dir, "tokyoto_1944_dest_no_ka_summary.csv"))
write_csv(no_ka_top_kakari,
          file.path(result_dir, "tokyoto_1944_dest_no_ka_top_kakari.csv"))

cat("Wrote destination residual diagnostics to:\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_unresolved_facility_distribution.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_unresolved_suffix_summary.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_no_ka_summary.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_no_ka_top_kakari.csv"), "\n")
