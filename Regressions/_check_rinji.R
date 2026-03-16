library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("=== Rinji by year ===\n")
df %>%
  mutate(has_rinji = !is.na(rinji) & rinji != "") %>%
  group_by(year_num) %>%
  summarise(n = n(), n_rinji = sum(has_rinji), pct = round(100 * n_rinji / n, 1), .groups = "drop") %>%
  print(n = 30)

cat("\n=== Rinji values (top 30) ===\n")
df %>%
  filter(!is.na(rinji) & rinji != "") %>%
  count(rinji, sort = TRUE) %>%
  print(n = 30)

cat("\n=== Rinji by year and gender ===\n")
df %>%
  filter(!is.na(rinji) & rinji != "") %>%
  mutate(is_female = gender_modern == "female") %>%
  group_by(year_num, is_female) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = is_female, values_from = n, values_fill = 0) %>%
  rename(male = `FALSE`, female = `TRUE`) %>%
  print(n = 30)

cat("\n=== Rinji by position (top 20) ===\n")
df %>%
  filter(!is.na(rinji) & rinji != "") %>%
  count(position, sort = TRUE) %>%
  print(n = 20)

cat("\n=== Sample rinji records ===\n")
df %>%
  filter(!is.na(rinji) & rinji != "", year_num == 1944) %>%
  select(staff_id, year_num, office_id, ka, position, name, gender_modern, salary, rinji, drafted) %>%
  head(20) %>%
  print(width = 200)

cat("\n=== Drafted status of rinji recipients ===\n")
df %>%
  filter(!is.na(rinji) & rinji != "") %>%
  count(drafted) %>%
  print()
