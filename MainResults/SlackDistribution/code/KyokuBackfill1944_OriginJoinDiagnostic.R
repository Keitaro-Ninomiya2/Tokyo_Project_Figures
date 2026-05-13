################################################################################
# Diagnose unresolved 1944 origin-side ka lookups after TokyoTo ka -> kyoku
# backfill. Goal: distinguish join-logic/string-match problems from genuinely
# missing reference coverage.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")
reference_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "reference_tables")

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")
REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_ka_to_kyoku.csv")

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

normalize_ka_lookup <- function(x) {
  x %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("課課", "課") %>%
    str_replace_all("係係", "係")
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
  )

ref_table <- read_csv(REF_PATH, show_col_types = FALSE) %>%
  mutate(
    ka_norm = normalize_ka_lookup(ka)
  )

origin_backfill <- read_csv(
  file.path(result_dir, "tokyoto_1944_origin_ka_backfill_rows.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    lag_ka_norm = normalize_ka_lookup(lag_ka)
  )

global_1943_ka_map <- df_names %>%
  filter(
    year_num == 1943,
    !is.na(ka),
    str_squish(ka) != "",
    !is.na(kyoku),
    str_squish(kyoku) != ""
  ) %>%
  mutate(
    ka_norm = normalize_ka_lookup(ka)
  ) %>%
  group_by(ka, ka_norm) %>%
  summarise(
    global_known_kyoku_n = n_distinct(kyoku),
    global_known_kyoku = if_else(n_distinct(kyoku) == 1L, first(kyoku), NA_character_),
    global_known_kyoku_values = paste(sort(unique(kyoku)), collapse = " | "),
    .groups = "drop"
  )

ref_exact_map <- ref_table %>%
  group_by(ka) %>%
  summarise(
    ref_exact_n = n_distinct(kyoku),
    ref_exact_kyoku = if_else(n_distinct(kyoku) == 1L, first(kyoku), NA_character_),
    ref_exact_values = paste(sort(unique(kyoku)), collapse = " | "),
    .groups = "drop"
  )

ref_norm_map <- ref_table %>%
  group_by(ka_norm) %>%
  summarise(
    ref_norm_n = n_distinct(kyoku),
    ref_norm_kyoku = if_else(n_distinct(kyoku) == 1L, first(kyoku), NA_character_),
    ref_norm_values = paste(sort(unique(kyoku)), collapse = " | "),
    ref_norm_ka_values = paste(sort(unique(ka)), collapse = " | "),
    .groups = "drop"
  )

origin_unresolved <- origin_backfill %>%
  filter(is.na(recovered_kyoku)) %>%
  left_join(ref_exact_map, by = c("lag_ka" = "ka")) %>%
  left_join(ref_norm_map, by = c("lag_ka_norm" = "ka_norm")) %>%
  left_join(global_1943_ka_map, by = c("lag_ka" = "ka", "lag_ka_norm" = "ka_norm")) %>%
  mutate(
    exact_ref_match = !is.na(ref_exact_n),
    norm_ref_match = !is.na(ref_norm_n),
    global_1943_match = !is.na(global_known_kyoku_n),
    unique_global_1943_match = global_known_kyoku_n == 1L,
    diagnosis = case_when(
      exact_ref_match ~ "exact_ref_match_not_recovered",
      !exact_ref_match & norm_ref_match ~ "normalizable_to_ref_ka",
      global_1943_match & unique_global_1943_match ~ "present_in_1943_observed_data_only",
      global_1943_match & !unique_global_1943_match ~ "observed_1943_but_ambiguous",
      TRUE ~ "no_ref_or_observed_match"
    )
  )

diagnosis_summary <- origin_unresolved %>%
  count(diagnosis, sort = TRUE, name = "n_rows") %>%
  mutate(share = n_rows / sum(n_rows))

potential_bug_rows <- origin_unresolved %>%
  filter(exact_ref_match | norm_ref_match) %>%
  distinct(
    lag_ka, lag_ka_norm, ref_exact_n, ref_exact_values,
    ref_norm_n, ref_norm_values, ref_norm_ka_values, diagnosis
  ) %>%
  arrange(diagnosis, lag_ka)

global_only_rows <- origin_unresolved %>%
  filter(diagnosis %in% c("present_in_1943_observed_data_only", "observed_1943_but_ambiguous")) %>%
  distinct(
    lag_ka, lag_ka_norm, global_known_kyoku_n, global_known_kyoku,
    global_known_kyoku_values, diagnosis
  ) %>%
  arrange(desc(global_known_kyoku_n), lag_ka)

top_unresolved <- origin_unresolved %>%
  count(
    lag_ka, diagnosis, ref_exact_n, ref_norm_n, global_known_kyoku_n,
    global_known_kyoku_values,
    sort = TRUE,
    name = "n_rows"
  )

write_csv(diagnosis_summary,
          file.path(result_dir, "tokyoto_1944_origin_join_diagnosis_summary.csv"))
write_csv(potential_bug_rows,
          file.path(result_dir, "tokyoto_1944_origin_join_potential_ref_matches.csv"))
write_csv(global_only_rows,
          file.path(result_dir, "tokyoto_1944_origin_join_global_1943_matches.csv"))
write_csv(top_unresolved,
          file.path(result_dir, "tokyoto_1944_origin_join_top_unresolved.csv"))

cat("Wrote origin-side join diagnostics to:\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_join_diagnosis_summary.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_join_potential_ref_matches.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_join_global_1943_matches.csv"), "\n")
cat(" -", file.path(result_dir, "tokyoto_1944_origin_join_top_unresolved.csv"), "\n")
