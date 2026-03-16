library(tidyverse)
DATA_NEW <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
DATA_OLD <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

df_new <- read_csv(DATA_NEW, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
df_old <- read_csv(DATA_OLD, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

cat("=== Drafted flag distribution ===\n")
cat("OLD:\n")
print(table(df_old$drafted, useNA = "always"))
cat("\nNEW:\n")
print(table(df_new$drafted, useNA = "always"))

cat("\n=== Drafted by year (OLD) ===\n")
df_old %>% filter(drafted == TRUE) %>% count(year) %>% print(n = 30)

cat("\n=== Drafted by year (NEW) ===\n")
df_new %>% filter(drafted == TRUE) %>% count(year) %>% print(n = 30)

cat("\n=== Sample drafted records (NEW) ===\n")
df_new %>% filter(drafted == TRUE) %>%
  select(staff_id, year, office_id, position, name, drafted) %>%
  head(30) %>% print(width = 150)
