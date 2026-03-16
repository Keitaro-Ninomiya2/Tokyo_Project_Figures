library(tidyverse)

DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# 1. All kyoku by year
cat("=== Kyoku by year ===\n")
for (yr in 1937:1945) {
  kyokus <- df %>% filter(year_num == yr) %>% distinct(kyoku) %>% pull(kyoku) %>% sort()
  cat(sprintf("\n%d (%d): %s\n", yr, length(kyokus), paste(kyokus, collapse = ", ")))
}

# 2. Worker-based kyoku flow: 1942 -> 1943
cat("\n\n=== Worker flow 1942 -> 1943 (kyoku level) ===\n")
w42 <- df %>% filter(year_num == 1942) %>% select(staff_id, kyoku_42 = kyoku, ka_42 = ka)
w43 <- df %>% filter(year_num == 1943) %>% select(staff_id, kyoku_43 = kyoku, ka_43 = ka)
m <- inner_join(w42, w43, by = "staff_id")
cat("Matched:", nrow(m), "\n\n")
m %>% count(kyoku_42, kyoku_43, sort = TRUE) %>% print(n = 50)

# 3. Worker-based kyoku flow: 1943 -> 1944
cat("\n\n=== Worker flow 1943 -> 1944 (kyoku level) ===\n")
w44 <- df %>% filter(year_num == 1944) %>% select(staff_id, kyoku_44 = kyoku, ka_44 = ka)
m2 <- inner_join(w43, w44, by = "staff_id")
cat("Matched:", nrow(m2), "\n\n")
m2 %>% count(kyoku_43, kyoku_44, sort = TRUE) %>% print(n = 50)

# 4. Worker-based kyoku flow: 1942 -> 1944 (skip 1943)
cat("\n\n=== Worker flow 1942 -> 1944 (kyoku level, skip 1943) ===\n")
m3 <- inner_join(w42, w44, by = "staff_id")
cat("Matched:", nrow(m3), "\n\n")
m3 %>% count(kyoku_42, kyoku_44, sort = TRUE) %>% print(n = 50)

# 5. ka-level flow 1942 -> 1944
cat("\n\n=== Worker flow 1942 -> 1944 (ka level) ===\n")
m3 %>%
  mutate(unit_42 = paste(kyoku_42, ka_42, sep = " > "),
         unit_44 = paste(kyoku_44, ka_44, sep = " > ")) %>%
  count(unit_42, unit_44, sort = TRUE) %>%
  print(n = 80)

# 6. For non-merger years (e.g. 1938->1939), what fraction of workers stay in same kyoku+ka?
cat("\n\n=== Stability check: same kyoku+ka across consecutive years ===\n")
for (yr in 1938:1944) {
  wa <- df %>% filter(year_num == yr) %>% select(staff_id, kyoku_a = kyoku, ka_a = ka)
  wb <- df %>% filter(year_num == yr + 1) %>% select(staff_id, kyoku_b = kyoku, ka_b = ka)
  mm <- inner_join(wa, wb, by = "staff_id")
  same <- sum(mm$kyoku_a == mm$kyoku_b & mm$ka_a == mm$ka_b, na.rm = TRUE)
  cat(sprintf("  %d->%d: %d matched, %d same kyoku+ka (%.0f%%)\n",
      yr, yr+1, nrow(mm), same, 100*same/nrow(mm)))
}
