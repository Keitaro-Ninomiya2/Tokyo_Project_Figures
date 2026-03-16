library(tidyverse)
DATA_NEW <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
DATA_OLD <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

df_old <- read_csv(DATA_OLD, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_new <- read_csv(DATA_NEW, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Get drafted staff x year pairs
old_drafted <- df_old %>% filter(drafted == TRUE) %>% select(staff_id, year_num, name, position, office_id)
new_drafted <- df_new %>% filter(drafted == TRUE) %>% select(staff_id, year_num, name, position, office_id)

cat("=== Overlap of drafted staff_id x year ===\n")
old_pairs <- old_drafted %>% select(staff_id, year_num)
new_pairs <- new_drafted %>% select(staff_id, year_num)
shared <- inner_join(old_pairs, new_pairs, by = c("staff_id", "year_num"))
cat("Old drafted pairs:", nrow(old_pairs), "\n")
cat("New drafted pairs:", nrow(new_pairs), "\n")
cat("Shared:", nrow(shared), "\n")
cat("Only in old:", nrow(anti_join(old_pairs, new_pairs, by = c("staff_id", "year_num"))), "\n")
cat("Only in new:", nrow(anti_join(new_pairs, old_pairs, by = c("staff_id", "year_num"))), "\n")

# Are the new drafted a subset of old?
cat("\n=== Is new a subset of old? ===\n")
new_not_in_old <- anti_join(new_pairs, old_pairs, by = c("staff_id", "year_num"))
cat("New drafted NOT in old:", nrow(new_not_in_old), "\n")
if (nrow(new_not_in_old) > 0) {
  cat("Sample:\n")
  df_new %>% filter(drafted == TRUE) %>%
    semi_join(new_not_in_old, by = c("staff_id", "year_num")) %>%
    select(staff_id, year_num, name, position, office_id) %>%
    head(10) %>% print(width = 150)
}

# What do the old-only drafted look like?
cat("\n=== Old-only drafted (lost in new data) ===\n")
old_only <- anti_join(old_pairs, new_pairs, by = c("staff_id", "year_num"))
cat("Count:", nrow(old_only), "\n")
cat("By year:\n")
old_only %>% count(year_num) %>% print()

cat("\nSample old-only drafted:\n")
df_old %>% filter(drafted == TRUE) %>%
  semi_join(old_only, by = c("staff_id", "year_num")) %>%
  select(staff_id, year_num, name, position, office_id) %>%
  head(15) %>% print(width = 150)

# Check: do the old-only drafted staff exist in new data at all (just not flagged)?
cat("\n=== Do old-only drafted staff exist in new data? ===\n")
old_only_staff <- unique(old_only$staff_id)
in_new <- df_new %>% filter(staff_id %in% old_only_staff) %>% distinct(staff_id)
cat("Old-only drafted unique staff:", length(old_only_staff), "\n")
cat("Of those, found in new data (unflagged):", nrow(in_new), "\n")

cat("\nSample: old-drafted staff in new data (not flagged):\n")
df_new %>%
  filter(staff_id %in% head(old_only_staff, 5)) %>%
  select(staff_id, year_num, name, position, office_id, drafted) %>%
  arrange(staff_id, year_num) %>%
  print(n = 30, width = 150)

cat("\nSame staff in OLD data:\n")
df_old %>%
  filter(staff_id %in% head(old_only_staff, 5)) %>%
  select(staff_id, year_num, name, position, office_id, drafted) %>%
  arrange(staff_id, year_num) %>%
  print(n = 30, width = 150)
