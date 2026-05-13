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

# More granular rank: 5 levels
assign_rank_fine <- function(pos, yr) {
  case_when(
    str_detect(pos, "課長|所長|院長|場長|局長|部長|館長") ~ 5L,
    str_detect(pos, "係長")                                ~ 4L,
    str_detect(pos, "^主事$|^技師$|主事補|技師補")         ~ 3L,
    str_detect(pos, "^書記$|^技手$|事務官|技官|吏員")      ~ 2L,
    str_detect(pos, "^雇$|^囑託$")                         ~ 1L,
    TRUE                                                    ~ 2L
  )
}

df <- df %>% mutate(pos_rank = assign_rank_fine(pos_norm, year_num))

# Identify transfers during wartime
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

# Career outcomes after transfer
career_after <- df %>%
  inner_join(transfers %>% select(staff_id, transfer_year), by = "staff_id") %>%
  filter(year_num >= transfer_year) %>%
  group_by(staff_id) %>%
  summarise(
    max_rank_after = max(pos_rank, na.rm = TRUE),
    peak_pos = pos_norm[which.max(pos_rank)],
    peak_year = year_num[which.max(pos_rank)],
    last_year = max(year_num),
    years_after = n_distinct(year_num),
    .groups = "drop"
  )

names_df <- df %>%
  select(staff_id, name) %>%
  distinct(staff_id, .keep_all = TRUE)

result <- transfers %>%
  left_join(career_after, by = "staff_id") %>%
  left_join(names_df, by = "staff_id") %>%
  mutate(rank_gain = max_rank_after - lag_rank) %>%
  arrange(desc(rank_gain), desc(max_rank_after), desc(years_after))

cat("=== TOP 20 WORKERS BY RANK GAIN (5-LEVEL SCALE) ===\n\n")
top <- result %>% head(20)
for (i in 1:nrow(top)) {
  r <- top[i,]
  cat(sprintf("%2d. %s (ID: %s)\n    Transfer: %d, %s (%s) -> %s (%s)\n    Pre-pos: %s (rank %d) -> Peak: %s (rank %d) in %d\n    Rank gain: %d, Years after transfer: %d, Last year: %d\n\n",
    i, r$name, r$staff_id, r$transfer_year,
    r$lag_kyoku, r$lag_ka, r$kyoku, r$ka,
    r$lag_pos, r$lag_rank, r$peak_pos, r$max_rank_after, r$peak_year,
    r$rank_gain, r$years_after, r$last_year))
}

# Full career of top worker
cat("\n=== FULL CAREER OF #1 ===\n")
top1 <- df %>%
  filter(staff_id == top$staff_id[1]) %>%
  arrange(year_num) %>%
  select(year_num, name, kyoku, ka, pos_norm, pos_rank)
print(as.data.frame(top1))

cat("\n=== FULL CAREER OF #2 ===\n")
top2 <- df %>%
  filter(staff_id == top$staff_id[2]) %>%
  arrange(year_num) %>%
  select(year_num, name, kyoku, ka, pos_norm, pos_rank)
print(as.data.frame(top2))

cat("\n=== FULL CAREER OF #3 ===\n")
top3 <- df %>%
  filter(staff_id == top$staff_id[3]) %>%
  arrange(year_num) %>%
  select(year_num, name, kyoku, ka, pos_norm, pos_rank)
print(as.data.frame(top3))
