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

# 6-level rank
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "局長") ~ 6L,
    str_detect(pos, "部長") ~ 5L,
    str_detect(pos, "課長|所長|院長|場長|館長|次長") ~ 4L,
    str_detect(pos, "係長") ~ 3L,
    str_detect(pos, "^主事$|^技師$|主事補|技師補") ~ 2L,
    str_detect(pos, "^雇$|^囑託$") ~ 0L,
    str_detect(pos, "^書記$|^技手$|事務官|技官|吏員") ~ 1L,
    TRUE ~ 1L
  )
}

df <- df %>% mutate(pos_rank = assign_rank(pos_norm))

# ============================================================
# CONSECUTIVE YEAR CHECK: only keep staff_ids observed in
# consecutive years (no gaps > 1 year) for a streak of 5+ years
# This ensures we're tracking the same person, not name collisions
# ============================================================

consecutive_check <- df %>%
  arrange(staff_id, year_num) %>%
  group_by(staff_id) %>%
  mutate(
    year_diff = year_num - lag(year_num),
    # A gap > 1 year breaks the streak
    streak_break = is.na(year_diff) | year_diff > 1,
    streak_id = cumsum(streak_break)
  ) %>%
  group_by(staff_id, streak_id) %>%
  summarise(
    streak_len = n(),
    streak_start = min(year_num),
    streak_end = max(year_num),
    .groups = "drop"
  )

# Keep only staff with at least one streak of 5+ consecutive years
reliable_staff <- consecutive_check %>%
  filter(streak_len >= 5) %>%
  distinct(staff_id)

cat("Reliable staff (5+ consecutive years):", nrow(reliable_staff), "\n")

df_reliable <- df %>% semi_join(reliable_staff, by = "staff_id")

# Identify first wartime transfer per worker
staff_lag <- df_reliable %>%
  select(staff_id, year_num, ka, kyoku, pos_norm, pos_rank) %>%
  rename(lag_ka = ka, lag_kyoku = kyoku, lag_pos = pos_norm, lag_rank = pos_rank) %>%
  mutate(year_num = year_num + 1)

transfers <- df_reliable %>%
  filter(year_num %in% 1938:1945) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_ka), !is.na(ka), (lag_ka != ka | lag_kyoku != kyoku)) %>%
  select(staff_id, transfer_year = year_num, lag_ka, lag_kyoku, ka, kyoku,
         lag_pos, lag_rank, dest_pos = pos_norm, dest_rank = pos_rank) %>%
  group_by(staff_id) %>%
  slice_min(transfer_year, n = 1) %>%
  ungroup()

# Career after transfer — only count ranks observed in consecutive years
# to avoid false peaks from name collisions
career_after <- transfers %>%
  select(staff_id, transfer_year) %>%
  inner_join(df_reliable, by = "staff_id", relationship = "many-to-many") %>%
  filter(year_num >= transfer_year) %>%
  arrange(staff_id, year_num) %>%
  group_by(staff_id) %>%
  mutate(
    year_diff = year_num - lag(year_num),
    streak_break = is.na(year_diff) | year_diff > 1,
    streak_id = cumsum(streak_break)
  ) %>%
  # Only use the first continuous streak after transfer
  filter(streak_id == 1) %>%
  summarise(
    max_rank_after = max(pos_rank, na.rm = TRUE),
    peak_pos = pos_norm[which.max(pos_rank)],
    peak_year = year_num[which.max(pos_rank)],
    last_year = max(year_num),
    consec_years = n(),
    .groups = "drop"
  )

names_df <- df_reliable %>%
  select(staff_id, name) %>%
  distinct(staff_id, .keep_all = TRUE)

result <- transfers %>%
  left_join(career_after, by = "staff_id") %>%
  left_join(names_df, by = "staff_id") %>%
  mutate(
    rank_gain = max_rank_after - lag_rank,
    years_to_peak = peak_year - transfer_year,
    promo_speed = if_else(years_to_peak > 0, rank_gain / years_to_peak, NA_real_),
    name_len = nchar(name)
  ) %>%
  # Full names (3+ chars), meaningful gain (3+), reached 課長+,
  # confirmed by consecutive year observation
  filter(name_len >= 3, rank_gain >= 3, max_rank_after >= 4, consec_years >= 5) %>%
  arrange(desc(promo_speed), desc(rank_gain), desc(max_rank_after))

cat("\n=== TOP 30 BY PROMOTION SPEED (consecutive-year verified) ===\n\n")
top <- result %>% head(30)
for (i in 1:nrow(top)) {
  r <- top[i,]
  cat(sprintf("%2d. %s | speed=%.2f | rank %d->%d in %d yrs | transfer %d (%s/%s -> %s/%s) | peak: %s in %d | consec_yrs: %d\n",
    i, r$name, r$promo_speed, r$lag_rank, r$max_rank_after, r$years_to_peak,
    r$transfer_year, r$lag_kyoku, r$lag_ka, r$kyoku, r$ka,
    r$peak_pos, r$peak_year, r$consec_years))
}

# Print full consecutive career for top 5
for (j in 1:min(5, nrow(top))) {
  cat(sprintf("\n=== CAREER: %s (ID: %s) ===\n", top$name[j], top$staff_id[j]))
  career <- df_reliable %>%
    filter(staff_id == top$staff_id[j]) %>%
    arrange(year_num) %>%
    select(year_num, name, kyoku, ka, pos_norm, pos_rank)
  print(as.data.frame(career))
}
