library(tidyverse)
library(fixest)

DATA_PATH <- file.path(Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv")

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))
df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))

years_of_interest <- 1938:1945

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

office_initial_year <- df %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")
staff_lag <- df %>%
  select(staff_id, year_num, lag_office = office_id, lag_ka = ka,
         lag_kyoku = kyoku, lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num"), relationship = "many-to-many") %>%
  mutate(
    is_new_hire = case_when(year_num == office_first_year ~ NA, TRUE ~ (year_num == first_year)),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    transfer_distance = case_when(
      !is_transfer_in ~ NA_integer_,
      !is.na(lag_ka) & !is.na(ka) & lag_ka == ka & lag_kyoku == kyoku ~ 1L,
      !is.na(lag_kyoku) & !is.na(kyoku) & lag_kyoku == kyoku ~ 2L,
      TRUE ~ 3L
    )
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    n_transfers_in = sum(is_transfer_in, na.rm = TRUE),
    n_transfers_same_ka = sum(transfer_distance == 1, na.rm = TRUE),
    n_transfers_diff_ka = sum(transfer_distance >= 2, na.rm = TRUE),
    n_transfers_dist2 = sum(transfer_distance == 2, na.rm = TRUE),
    n_transfers_dist3 = sum(transfer_distance == 3, na.rm = TRUE),
    .groups = "drop"
  )

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted = n(), n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

panel_ka <- position_panel %>% filter(!is.na(ka_id))

cat("\n===== OUTCOME MEANS (panel_ka, N =", nrow(panel_ka), ") =====\n")
cat("n_drafted_male:       mean =", round(mean(panel_ka$n_drafted_male), 4),
    " | sd =", round(sd(panel_ka$n_drafted_male), 4),
    " | max =", max(panel_ka$n_drafted_male),
    " | share>0:", round(mean(panel_ka$n_drafted_male > 0), 4), "\n")
cat("n_transfers_in:       mean =", round(mean(panel_ka$n_transfers_in), 4),
    " | sd =", round(sd(panel_ka$n_transfers_in), 4),
    " | max =", max(panel_ka$n_transfers_in),
    " | share>0:", round(mean(panel_ka$n_transfers_in > 0), 4), "\n")
cat("n_transfers_same_ka:  mean =", round(mean(panel_ka$n_transfers_same_ka), 4),
    " | sd =", round(sd(panel_ka$n_transfers_same_ka), 4),
    " | max =", max(panel_ka$n_transfers_same_ka),
    " | share>0:", round(mean(panel_ka$n_transfers_same_ka > 0), 4), "\n")
cat("n_transfers_diff_ka:  mean =", round(mean(panel_ka$n_transfers_diff_ka), 4),
    " | sd =", round(sd(panel_ka$n_transfers_diff_ka), 4),
    " | max =", max(panel_ka$n_transfers_diff_ka),
    " | share>0:", round(mean(panel_ka$n_transfers_diff_ka > 0), 4), "\n")
cat("n_transfers_dist2:    mean =", round(mean(panel_ka$n_transfers_dist2), 4),
    " | sd =", round(sd(panel_ka$n_transfers_dist2), 4),
    " | max =", max(panel_ka$n_transfers_dist2),
    " | share>0:", round(mean(panel_ka$n_transfers_dist2 > 0), 4), "\n")
cat("n_transfers_dist3:    mean =", round(mean(panel_ka$n_transfers_dist3), 4),
    " | sd =", round(sd(panel_ka$n_transfers_dist3), 4),
    " | max =", max(panel_ka$n_transfers_dist3),
    " | share>0:", round(mean(panel_ka$n_transfers_dist3 > 0), 4), "\n")
cat("n_new_hires:          mean =", round(mean(panel_ka$n_new_hires), 4),
    " | sd =", round(sd(panel_ka$n_new_hires), 4),
    " | max =", max(panel_ka$n_new_hires, na.rm=TRUE),
    " | share>0:", round(mean(panel_ka$n_new_hires > 0, na.rm=TRUE), 4), "\n")

cat("\n===== DISTRIBUTION OF n_transfers_in =====\n")
cat("Quantiles: ")
print(quantile(panel_ka$n_transfers_in, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))

cat("\n===== DISTRIBUTION OF n_drafted_male =====\n")
cat("Quantiles: ")
print(quantile(panel_ka$n_drafted_male, c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)))

# Check: among obs with drafts > 0
drafted_pos <- panel_ka %>% filter(n_drafted_male > 0)
cat("\n===== AMONG POSITIONS WITH DRAFTS (N =", nrow(drafted_pos), ") =====\n")
cat("n_drafted_male: mean =", round(mean(drafted_pos$n_drafted_male), 4), "\n")
cat("n_transfers_in: mean =", round(mean(drafted_pos$n_transfers_in), 4), "\n")
cat("n_new_hires:    mean =", round(mean(drafted_pos$n_new_hires, na.rm=TRUE), 4), "\n")

# Check for many-to-many inflation
cat("\n===== CHECK MANY-TO-MANY JOIN =====\n")
cat("staff_transitions rows:", nrow(staff_transitions), "\n")
dup_check <- staff_transitions %>% group_by(staff_id, year_num) %>% summarise(n = n(), .groups="drop")
cat("Unique staff-year pairs:", nrow(dup_check), "\n")
cat("Duplicated staff-years:", sum(dup_check$n > 1), "\n")
cat("Max duplicates:", max(dup_check$n), "\n")
