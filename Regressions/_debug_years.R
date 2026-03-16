library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("Workers by year:\n")
df %>% count(year_num) %>% print(n = 40)

cat("\nConsecutive year overlap (workers in both t-1 and t, same office):\n")
for (t in 1935:1955) {
  prev <- df %>% filter(year_num == t - 1) %>% select(staff_id, office_id)
  curr <- df %>% filter(year_num == t) %>% select(staff_id, curr_office = office_id)
  matched <- inner_join(prev, curr, by = "staff_id") %>% filter(office_id == curr_office)
  if (nrow(prev) > 0 && nrow(curr) > 0) {
    cat(sprintf("%d->%d: prev=%d, curr=%d, same_office=%d\n", t-1, t, nrow(prev), nrow(curr), nrow(matched)))
  }
}
