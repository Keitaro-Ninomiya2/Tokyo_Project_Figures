library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("\n=== Transfer rates by year pair ===\n")
for (t in 1938:1945) {
  prev <- df %>% filter(year_num == t-1) %>% select(staff_id, office_id_prev = office_id)
  curr <- df %>% filter(year_num == t) %>% select(staff_id, office_id_curr = office_id)
  matched <- inner_join(prev, curr, by = "staff_id")
  n_same <- sum(matched$office_id_prev == matched$office_id_curr)
  n_diff <- sum(matched$office_id_prev != matched$office_id_curr)
  cat(sprintf("%d->%d: %d matched, %d same office, %d diff office (%.1f%%)\n",
      t-1, t, nrow(matched), n_same, n_diff, 100*n_diff/nrow(matched)))
}

cat("\n=== What does office_id look like? ===\n")
cat("Sample office_ids in 1942:\n")
print(head(sort(unique(df$office_id[df$year_num == 1942])), 20))
cat("\nSample office_ids in 1943:\n")
print(head(sort(unique(df$office_id[df$year_num == 1943])), 20))

cat("\n=== Kyoku-Ka stability across 1942->1943 ===\n")
prev <- df %>% filter(year_num == 1942) %>% select(staff_id, kyoku_prev = kyoku, ka_prev = ka, kakari_prev = kakari)
curr <- df %>% filter(year_num == 1943) %>% select(staff_id, kyoku_curr = kyoku, ka_curr = ka, kakari_curr = kakari)
matched <- inner_join(prev, curr, by = "staff_id")
cat(sprintf("Same kyoku+ka: %d / %d (%.1f%%)\n",
    sum(matched$kyoku_prev == matched$kyoku_curr & matched$ka_prev == matched$ka_curr, na.rm = TRUE),
    nrow(matched),
    100*sum(matched$kyoku_prev == matched$kyoku_curr & matched$ka_prev == matched$ka_curr, na.rm = TRUE)/nrow(matched)))
