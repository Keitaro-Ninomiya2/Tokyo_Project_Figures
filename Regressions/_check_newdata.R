library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year))

cat("Total rows (is_name):", nrow(df), "\n")
cat("Unique staff_id x year:", df %>% distinct(staff_id, year_num) %>% nrow(), "\n")
cat("Duplicates per staff_id x year:\n")
df %>% count(staff_id, year_num) %>% filter(n > 1) %>%
  summarise(n_dupes = n(), mean_n = mean(n), max_n = max(n)) %>% print()

cat("\nYears:\n")
df %>% count(year_num) %>% print(n = 30)

cat("\nNew columns check:\n")
cat("Has rinji:", "rinji" %in% names(df), "\n")
cat("Has grade:", "grade" %in% names(df), "\n")
cat("Has bu:", "bu" %in% names(df), "\n")
cat("Non-NA rinji:", sum(!is.na(df$rinji) & df$rinji != ""), "\n")

cat("\nSample duplicates:\n")
dupes <- df %>% count(staff_id, year_num) %>% filter(n > 1)
if (nrow(dupes) > 0) {
  sample_ids <- head(dupes$staff_id, 3)
  df %>% filter(staff_id %in% sample_ids) %>%
    select(staff_id, year_num, office_id, ka, kakari, position, name) %>%
    arrange(staff_id, year_num) %>% print(n = 30)
}
