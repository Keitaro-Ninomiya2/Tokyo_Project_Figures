################################################################################
# Diagnose why Table 1a transfer results changed with new data
################################################################################
library(tidyverse)

# Load BOTH datasets
DATA_NEW <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
DATA_OLD <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

df_new <- read_csv(DATA_NEW, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_old <- read_csv(DATA_OLD, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("=== Size comparison ===\n")
cat("Old (deduped):", nrow(df_old), "\n")
cat("New (deduped):", nrow(df_new), "\n")

cat("\n=== Years comparison ===\n")
cat("Old years:", sort(unique(df_old$year_num)), "\n")
cat("New years:", sort(unique(df_new$year_num)), "\n")

# Focus on 1937-1945
years <- 1937:1945
cat("\n=== 1937-1945 counts ===\n")
old_counts <- df_old %>% filter(year_num %in% years) %>% count(year_num)
new_counts <- df_new %>% filter(year_num %in% years) %>% count(year_num)
comparison <- full_join(old_counts, new_counts, by = "year_num", suffix = c("_old", "_new"))
print(comparison)

cat("\n=== office_id overlap ===\n")
old_offices <- df_old %>% filter(year_num %in% 1938:1945) %>% distinct(office_id) %>% pull()
new_offices <- df_new %>% filter(year_num %in% 1938:1945) %>% distinct(office_id) %>% pull()
cat("Old offices:", length(old_offices), "\n")
cat("New offices:", length(new_offices), "\n")
cat("Shared:", length(intersect(old_offices, new_offices)), "\n")
cat("Only in old:", length(setdiff(old_offices, new_offices)), "\n")
cat("Only in new:", length(setdiff(new_offices, old_offices)), "\n")

cat("\n=== staff_id overlap (1938-1945) ===\n")
old_staff <- df_old %>% filter(year_num %in% 1938:1945) %>% distinct(staff_id) %>% pull()
new_staff <- df_new %>% filter(year_num %in% 1938:1945) %>% distinct(staff_id) %>% pull()
cat("Old staff:", length(old_staff), "\n")
cat("New staff:", length(new_staff), "\n")
cat("Shared:", length(intersect(old_staff, new_staff)), "\n")

cat("\n=== Drafting comparison ===\n")
old_drafts <- df_old %>% filter(year_num %in% 1938:1945, drafted == TRUE)
new_drafts <- df_new %>% filter(year_num %in% 1938:1945, drafted == TRUE)
cat("Old drafted:", nrow(old_drafts), "\n")
cat("New drafted:", nrow(new_drafts), "\n")

cat("\n=== Transfer detection comparison ===\n")
# Simple transfer check: staff_id appears at different office_id in consecutive years
check_transfers <- function(df_in, label) {
  lag_data <- df_in %>%
    filter(year_num %in% years) %>%
    distinct(staff_id, year_num, .keep_all = TRUE) %>%
    select(staff_id, year_num, lag_office = office_id) %>%
    mutate(year_num = year_num + 1)

  transfers <- df_in %>%
    filter(year_num %in% 1938:1945) %>%
    left_join(lag_data, by = c("staff_id", "year_num")) %>%
    mutate(is_transfer = !is.na(lag_office) & lag_office != office_id)

  cat(label, "- Total obs:", nrow(transfers), "\n")
  cat(label, "- Transfers:", sum(transfers$is_transfer, na.rm = TRUE), "\n")
  cat(label, "- Transfer rate:", round(mean(transfers$is_transfer, na.rm = TRUE), 4), "\n")
}
check_transfers(df_old, "Old")
check_transfers(df_new, "New")

cat("\n=== Position counts comparison (1938-1945) ===\n")
old_pos <- df_old %>% filter(year_num %in% 1938:1945) %>% count(pos_norm, sort = TRUE) %>% head(15)
new_pos <- df_new %>% filter(year_num %in% 1938:1945) %>% count(pos_norm, sort = TRUE) %>% head(15)
cat("Old top positions:\n")
print(old_pos)
cat("\nNew top positions:\n")
print(new_pos)
