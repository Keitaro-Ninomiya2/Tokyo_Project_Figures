################################################################################
# Apply 1943-44 TokyoTo ka -> kyoku reference table to 1944 NA transfer rows.
# Uses same-office same-year known assignments as a conservative tiebreaker for
# repeated ka labels.
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
REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_ka_to_kyoku.csv")
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

na_transfer_1944 <- worker_arrivals %>%
  filter(year_num == 1944, is_transfer_in) %>%
  inner_join(drafted_section_totals, by = c("section_id", "year_num")) %>%
  filter(is.na(arrival_type)) %>%
  mutate(
    dest_kyoku_missing = is.na(kyoku) | str_squish(replace_na(kyoku, "")) == "",
    origin_kyoku_missing = is.na(lag_kyoku_raw) | str_squish(replace_na(lag_kyoku_raw, "")) == "",
    dest_ka_present = !is.na(ka) & str_squish(ka) != "",
    origin_ka_present = !is.na(lag_ka) & str_squish(lag_ka) != ""
  )

ref_table <- read_csv(REF_PATH, show_col_types = FALSE) %>%
  mutate(
    effective_start = as.Date(effective_start),
    effective_end = as.Date(effective_end)
  ) %>%
  filter(effective_start <= as.Date("1944-12-31"), effective_end >= as.Date("1944-01-01"))

ref_1944 <- ref_table %>%
  distinct(ka, kyoku, source_type, source_document, notes)

ref_candidate_counts <- ref_1944 %>%
  count(ka, name = "ref_candidate_count")

ref_unique <- ref_1944 %>%
  group_by(ka) %>%
  filter(n() == 1L) %>%
  ungroup() %>%
  transmute(ka, ref_kyoku = kyoku, ref_source_type = source_type)

office_ka_known <- df_names %>%
  filter(year_num == 1944, !is.na(kyoku), !is.na(ka), str_squish(ka) != "") %>%
  group_by(office_id, year_num, ka) %>%
  summarise(
    office_known_kyoku_n = n_distinct(kyoku),
    office_known_kyoku = if_else(n_distinct(kyoku) == 1L, first(kyoku), NA_character_),
    .groups = "drop"
  )

origin_office_ka_known <- df_names %>%
  filter(year_num == 1943, !is.na(kyoku), !is.na(ka), str_squish(ka) != "") %>%
  group_by(origin_office_id = office_id, origin_year_num = year_num, lag_ka = ka) %>%
  summarise(
    origin_known_kyoku_n = n_distinct(kyoku),
    origin_known_kyoku = if_else(n_distinct(kyoku) == 1L, first(kyoku), NA_character_),
    .groups = "drop"
  )

dest_backfill <- na_transfer_1944 %>%
  filter(dest_kyoku_missing, dest_ka_present) %>%
  left_join(ref_candidate_counts, by = "ka") %>%
  left_join(ref_unique, by = "ka") %>%
  left_join(office_ka_known, by = c("office_id", "year_num", "ka")) %>%
  mutate(
    recovered_kyoku = coalesce(office_known_kyoku, ref_kyoku),
    recovery_method = case_when(
      !is.na(office_known_kyoku) ~ "office_year_ka_tiebreak",
      !is.na(ref_kyoku) ~ "unique_ka_reference",
      TRUE ~ NA_character_
    )
  )

origin_backfill <- na_transfer_1944 %>%
  filter(origin_kyoku_missing, origin_ka_present) %>%
  left_join(
    ref_candidate_counts %>% rename(lag_ka = ka),
    by = "lag_ka"
  ) %>%
  left_join(
    ref_unique %>% rename(lag_ka = ka, ref_kyoku = ref_kyoku, ref_source_type = ref_source_type),
    by = "lag_ka"
  ) %>%
  mutate(origin_year_num = year_num - 1L) %>%
  left_join(origin_office_ka_known, by = c("lag_office_id" = "origin_office_id", "origin_year_num", "lag_ka")) %>%
  mutate(
    recovered_kyoku = coalesce(origin_known_kyoku, ref_kyoku),
    recovery_method = case_when(
      !is.na(origin_known_kyoku) ~ "office_year_ka_tiebreak",
      !is.na(ref_kyoku) ~ "unique_ka_reference",
      TRUE ~ NA_character_
    )
  )

summary_table <- bind_rows(
  dest_backfill %>%
    summarise(
      side = "destination",
      n_missing_kyoku = n(),
      n_ka_present = n(),
      n_recovered = sum(!is.na(recovered_kyoku)),
      recovery_rate = mean(!is.na(recovered_kyoku)),
      n_recovered_office_tiebreak = sum(recovery_method == "office_year_ka_tiebreak", na.rm = TRUE),
      n_recovered_unique_reference = sum(recovery_method == "unique_ka_reference", na.rm = TRUE),
      n_ambiguous_reference_ka = sum(replace_na(ref_candidate_count, 0) > 1L),
      n_no_reference_match = sum(is.na(ref_candidate_count))
    ),
  origin_backfill %>%
    summarise(
      side = "origin",
      n_missing_kyoku = n(),
      n_ka_present = n(),
      n_recovered = sum(!is.na(recovered_kyoku)),
      recovery_rate = mean(!is.na(recovered_kyoku)),
      n_recovered_office_tiebreak = sum(recovery_method == "office_year_ka_tiebreak", na.rm = TRUE),
      n_recovered_unique_reference = sum(recovery_method == "unique_ka_reference", na.rm = TRUE),
      n_ambiguous_reference_ka = sum(replace_na(ref_candidate_count, 0) > 1L),
      n_no_reference_match = sum(is.na(ref_candidate_count))
    )
)

dest_unresolved_top <- dest_backfill %>%
  filter(is.na(recovered_kyoku)) %>%
  count(ka, sort = TRUE, name = "n_rows") %>%
  left_join(ref_candidate_counts, by = "ka")

origin_unresolved_top <- origin_backfill %>%
  filter(is.na(recovered_kyoku)) %>%
  count(lag_ka, sort = TRUE, name = "n_rows") %>%
  left_join(ref_candidate_counts %>% rename(lag_ka = ka), by = "lag_ka")

dest_recovered_examples <- dest_backfill %>%
  filter(!is.na(recovered_kyoku)) %>%
  transmute(
    staff_id, office_id, year_num, ka, position,
    recovered_kyoku, recovery_method, ref_candidate_count
  ) %>%
  distinct() %>%
  slice_head(n = 50)

origin_recovered_examples <- origin_backfill %>%
  filter(!is.na(recovered_kyoku)) %>%
  transmute(
    staff_id, lag_office_id, origin_year = year_num - 1L, lag_ka, lag_position,
    recovered_kyoku, recovery_method, ref_candidate_count
  ) %>%
  distinct() %>%
  slice_head(n = 50)

write_csv(summary_table, file.path(result_dir, "tokyoto_1944_ka_backfill_summary.csv"))
write_csv(dest_backfill, file.path(result_dir, "tokyoto_1944_dest_ka_backfill_rows.csv"))
write_csv(origin_backfill, file.path(result_dir, "tokyoto_1944_origin_ka_backfill_rows.csv"))
write_csv(dest_unresolved_top, file.path(result_dir, "tokyoto_1944_dest_ka_unresolved_top.csv"))
write_csv(origin_unresolved_top, file.path(result_dir, "tokyoto_1944_origin_ka_unresolved_top.csv"))
write_csv(dest_recovered_examples, file.path(result_dir, "tokyoto_1944_dest_ka_recovered_examples.csv"))
write_csv(origin_recovered_examples, file.path(result_dir, "tokyoto_1944_origin_ka_recovered_examples.csv"))

cat("Wrote TokyoTo 1944 ka backfill outputs to:\n")
cat(" -", file.path(result_dir, "tokyoto_1944_ka_backfill_summary.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_ka_backfill_rows.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_ka_backfill_rows.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_ka_unresolved_top.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_ka_unresolved_top.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_ka_recovered_examples.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_ka_recovered_examples.csv"), "\n")
