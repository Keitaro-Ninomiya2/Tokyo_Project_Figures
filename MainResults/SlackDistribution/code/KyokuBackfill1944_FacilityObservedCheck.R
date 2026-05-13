################################################################################
# Check whether unresolved destination-side facility labels appear elsewhere in
# the master panel with known kyoku assignments.
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

unresolved <- read_csv(
  file.path(result_dir, "tokyoto_1944_dest_unresolved_facility_distribution.csv"),
  show_col_types = FALSE
) %>%
  select(ka, suffix_group) %>%
  distinct()

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year))

observed_matches <- unresolved %>%
  left_join(
    df %>%
      filter(!is.na(kyoku), !is.na(ka), str_squish(ka) != "") %>%
      group_by(ka) %>%
      summarise(
        n_kyoku = n_distinct(kyoku),
        kyoku_values = paste(sort(unique(kyoku)), collapse = " | "),
        years = paste(sort(unique(year_num)), collapse = ","),
        n_rows_observed = n(),
        .groups = "drop"
      ),
    by = "ka"
  ) %>%
  arrange(desc(!is.na(n_kyoku)), desc(n_rows_observed), ka)

write_csv(
  observed_matches,
  file.path(result_dir, "tokyoto_1944_dest_unresolved_observed_elsewhere.csv")
)

cat("Wrote observed-elsewhere facility check to:\n")
cat(" -", file.path(result_dir, "tokyoto_1944_dest_unresolved_observed_elsewhere.csv"), "\n")
