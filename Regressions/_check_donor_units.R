library(tidyverse)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year),
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, pos_norm) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

transfers <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office), !is.na(lag_ka)) %>%
  filter(lag_office != office_id | lag_ka != ka) %>%
  mutate(donor_cell = paste(lag_office, lag_ka, lag_pos, year_num, sep = "_"))

cat("Total transfers:", nrow(transfers), "\n")
cat("Total donor cells (unit × year):", n_distinct(transfers$donor_cell), "\n\n")

dist <- transfers %>%
  count(donor_cell, name = "n_transfers") %>%
  count(n_transfers, name = "n_cells")

cat("Distribution of transfers per donor unit × year:\n")
print(as.data.frame(dist), row.names = FALSE)
cat("\nMedian transfers per cell:", median(transfers %>% count(donor_cell) %>% pull(n)), "\n")
cat("Mean transfers per cell:",   round(mean(transfers %>% count(donor_cell) %>% pull(n)), 2), "\n")

cat("\nTop 10 donor cells by transfer count:\n")
top_cells <- transfers %>%
  count(donor_cell, lag_office, lag_ka, lag_pos, year_num, name = "n_transfers") %>%
  arrange(desc(n_transfers))
print(as.data.frame(top_cells %>% head(10)), row.names = FALSE)
