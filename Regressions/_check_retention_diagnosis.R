library(tidyverse)
DATA_NEW <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
DATA_OLD <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

load_data <- function(path) {
  read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
    filter(is_name == TRUE) %>%
    mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
           pos_norm = str_replace_all(position, "\\s+", "")) %>%
    distinct(staff_id, year_num, .keep_all = TRUE)
}

load_all <- function(path) {
  read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
    mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
           pos_norm = str_replace_all(position, "\\s+", "")) %>%
    distinct(staff_id, year_num, .keep_all = TRUE)
}

df_old <- load_data(DATA_OLD)
df_new <- load_data(DATA_NEW)
dfa_old <- load_all(DATA_OLD)
dfa_new <- load_all(DATA_NEW)

years <- 1938:1945

cat("=== Drafted counts by year (is_name==TRUE, deduped) ===\n")
old_d <- df_old %>% filter(drafted == TRUE, year_num %in% years) %>% count(year_num, name = "old")
new_d <- df_new %>% filter(drafted == TRUE, year_num %in% years) %>% count(year_num, name = "new")
full_join(old_d, new_d, by = "year_num") %>% print()

cat("\n=== Drafted counts by year (df_all, deduped) ===\n")
old_da <- dfa_old %>% filter(drafted == TRUE, year_num %in% years) %>% count(year_num, name = "old")
new_da <- dfa_new %>% filter(drafted == TRUE, year_num %in% years) %>% count(year_num, name = "new")
full_join(old_da, new_da, by = "year_num") %>% print()

cat("\n=== Draft counts at office x pos level (treatment variable) ===\n")
draft_counts_old <- dfa_old %>%
  filter(year_num %in% years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted = sum(drafted == TRUE & !is_female, na.rm = TRUE), .groups = "drop")
draft_counts_new <- dfa_new %>%
  filter(year_num %in% years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted = sum(drafted == TRUE & !is_female, na.rm = TRUE), .groups = "drop")

cat("Old - positions with any drafts:", sum(draft_counts_old$n_drafted > 0), "\n")
cat("New - positions with any drafts:", sum(draft_counts_new$n_drafted > 0), "\n")
cat("Old - mean drafts (where > 0):", round(mean(draft_counts_old$n_drafted[draft_counts_old$n_drafted > 0]), 2), "\n")
cat("New - mean drafts (where > 0):", round(mean(draft_counts_new$n_drafted[draft_counts_new$n_drafted > 0]), 2), "\n")

cat("\nOld draft count distribution:\n")
print(table(draft_counts_old$n_drafted))
cat("\nNew draft count distribution:\n")
print(table(draft_counts_new$n_drafted))

cat("\n=== Excluded staff (ever drafted) ===\n")
old_excluded <- dfa_old %>% filter(drafted == TRUE) %>% distinct(staff_id) %>% nrow()
new_excluded <- dfa_new %>% filter(drafted == TRUE) %>% distinct(staff_id) %>% nrow()
cat("Old:", old_excluded, "\n")
cat("New:", new_excluded, "\n")
