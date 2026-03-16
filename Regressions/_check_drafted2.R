library(tidyverse)
DATA_NEW <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
DATA_OLD <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

df_new <- read_csv(DATA_NEW, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
df_old <- read_csv(DATA_OLD, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("=== Drafted with is_name == TRUE ===\n")
cat("OLD:", sum(df_old$drafted == TRUE & df_old$is_name == TRUE, na.rm = TRUE), "\n")
cat("NEW:", sum(df_new$drafted == TRUE & df_new$is_name == TRUE, na.rm = TRUE), "\n")

cat("\n=== Drafted with is_name == TRUE, by year ===\n")
cat("OLD:\n")
df_old %>% filter(drafted == TRUE, is_name == TRUE) %>% count(year) %>% print()
cat("\nNEW:\n")
df_new %>% filter(drafted == TRUE, is_name == TRUE) %>% count(year) %>% print()

cat("\n=== Drafted with staff_id not NA ===\n")
cat("OLD:", sum(df_old$drafted == TRUE & !is.na(df_old$staff_id), na.rm = TRUE), "\n")
cat("NEW:", sum(df_new$drafted == TRUE & !is.na(df_new$staff_id), na.rm = TRUE), "\n")

cat("\n=== Sample of OLD drafted with is_name == TRUE ===\n")
df_old %>% filter(drafted == TRUE, is_name == TRUE) %>%
  select(staff_id, year, office_id, position, name, drafted) %>%
  head(15) %>% print(width = 150)

cat("\n=== Sample of NEW drafted with is_name == TRUE ===\n")
df_new %>% filter(drafted == TRUE, is_name == TRUE) %>%
  select(staff_id, year, office_id, position, name, drafted) %>%
  head(15) %>% print(width = 150)

cat("\n=== Unique drafted staff (is_name & deduped) ===\n")
old_drafted_staff <- df_old %>% filter(drafted == TRUE, is_name == TRUE) %>%
  distinct(staff_id, year, .keep_all = TRUE)
new_drafted_staff <- df_new %>% filter(drafted == TRUE, is_name == TRUE) %>%
  distinct(staff_id, year, .keep_all = TRUE)
cat("OLD unique drafted staff x year:", nrow(old_drafted_staff), "\n")
cat("NEW unique drafted staff x year:", nrow(new_drafted_staff), "\n")
