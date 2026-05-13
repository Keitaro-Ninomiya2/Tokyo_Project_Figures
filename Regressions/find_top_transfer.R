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

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

df <- df %>% mutate(pos_rank = assign_rank(pos_norm, year_num))

# Identify transfers (cross-ka or cross-kyoku)
staff_lag <- df %>%
  select(staff_id, year_num, ka, kyoku, pos_norm, pos_rank) %>%
  rename(lag_ka = ka, lag_kyoku = kyoku, lag_pos = pos_norm, lag_rank = pos_rank) %>%
  mutate(year_num = year_num + 1)

transfers <- df %>%
  filter(year_num %in% 1938:1945) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_ka), !is.na(ka), (lag_ka != ka | lag_kyoku != kyoku)) %>%
  select(staff_id, transfer_year = year_num, lag_ka, lag_kyoku, ka, kyoku,
         lag_pos, lag_rank, dest_pos = pos_norm, dest_rank = pos_rank)

# For each transferred worker, get their full career trajectory
career <- df %>%
  group_by(staff_id) %>%
  summarise(
    max_rank = max(pos_rank, na.rm = TRUE),
    max_rank_pos = pos_norm[which.max(pos_rank)],
    max_rank_year = year_num[which.max(pos_rank)],
    last_year = max(year_num),
    first_year = min(year_num),
    .groups = "drop"
  )

# Get names
names_df <- df %>%
  select(staff_id, name) %>%
  distinct(staff_id, .keep_all = TRUE)

# Join and compute rank gain from transfer
result <- transfers %>%
  left_join(career, by = "staff_id") %>%
  left_join(names_df, by = "staff_id") %>%
  mutate(rank_gain = max_rank - lag_rank) %>%
  arrange(desc(rank_gain), desc(max_rank), desc(last_year - transfer_year))

cat("=== TOP 20 WORKERS BY RANK GAIN AFTER TRANSFER ===\n\n")
top <- result %>% head(20)
for (i in 1:nrow(top)) {
  r <- top[i,]
  cat(sprintf("%2d. %s (ID: %s)\n    Transfer: %d, %s -> %s\n    Pre-transfer pos: %s (rank %d) -> Peak pos: %s (rank %d) in %d\n    Rank gain: %d, Career span: %d-%d\n\n",
    i, r$name, r$staff_id, r$transfer_year, r$lag_ka, r$ka,
    r$lag_pos, r$lag_rank, r$max_rank_pos, r$max_rank, r$max_rank_year,
    r$rank_gain, r$first_year, r$last_year))
}

# Also show their full career trajectory for the top person
cat("\n=== FULL CAREER OF TOP WORKER ===\n")
top1_id <- top$staff_id[1]
top1_career <- df %>%
  filter(staff_id == top1_id) %>%
  arrange(year_num) %>%
  select(year_num, name, kyoku, ka, pos_norm, pos_rank)
print(as.data.frame(top1_career))
