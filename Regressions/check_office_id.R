library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# What does office_id look like?
cat("=== Sample office_id values ===\n")
df %>% filter(year_num == 1942) %>% distinct(office_id, kyoku, ka, kakari) %>%
  arrange(kyoku, ka, kakari) %>% print(n = 30)

# How is office_id constructed?
cat("\n=== office_id uniqueness within year ===\n")
for (yr in 1938:1944) {
  n_oid <- df %>% filter(year_num == yr) %>% n_distinct(.$office_id)
  n_kyoku_ka <- df %>% filter(year_num == yr) %>%
    mutate(kk = paste(kyoku, ka, sep = "|")) %>% n_distinct(.$kk)
  cat(sprintf("%d: %d unique office_id, %d unique kyoku|ka\n", yr, n_oid, n_kyoku_ka))
}

# Check kyoku-only stability (ignoring ka)
cat("\n=== Kyoku-only stability across consecutive years ===\n")
for (yr in 1938:1944) {
  wa <- df %>% filter(year_num == yr) %>% select(staff_id, kyoku_a = kyoku)
  wb <- df %>% filter(year_num == yr + 1) %>% select(staff_id, kyoku_b = kyoku)
  mm <- inner_join(wa, wb, by = "staff_id")
  same <- sum(mm$kyoku_a == mm$kyoku_b, na.rm = TRUE)
  cat(sprintf("  %d->%d: %d matched, %d same kyoku (%.0f%%)\n",
      yr, yr+1, nrow(mm), same, 100*same/nrow(mm)))
}

# Check if office_id = kyoku + ka + something
cat("\n=== Sample office_id structure ===\n")
df %>% filter(year_num == 1942) %>%
  select(office_id, kyoku, ka, kakari) %>%
  head(20) %>% print()
