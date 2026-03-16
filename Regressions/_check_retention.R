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

# Individual-level: for each worker in year t, did they appear in year t+1?
staff_year <- df %>%
  filter(year_num %in% years_of_interest) %>%
  distinct(staff_id, year_num, office_id, pos_norm, is_female)

staff_appears_next <- df %>%
  distinct(staff_id, year_num) %>%
  rename(next_year = year_num) %>%
  mutate(year_num = next_year - 1)

staff_retention <- staff_year %>%
  left_join(staff_appears_next %>% select(staff_id, year_num, retained = next_year),
            by = c("staff_id", "year_num")) %>%
  mutate(retained = as.integer(!is.na(retained))) %>%
  filter(year_num < max(years_of_interest))  # exclude last year

# Draft info
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

pos_ka_map <- df %>%
  filter(year_num %in% years_of_interest, !is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

# Merge all
retention_panel <- staff_retention %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    any_drafted = as.integer(n_drafted_male > 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

panel_ka <- retention_panel %>% filter(!is.na(ka_id))

cat("===== RETENTION PANEL =====\n")
cat("Total obs:", nrow(panel_ka), "\n")
cat("Mean retained:", round(mean(panel_ka$retained), 4), "\n")
cat("Retained in drafted pos:", round(mean(panel_ka$retained[panel_ka$any_drafted == 1]), 4), "\n")
cat("Retained in non-drafted:", round(mean(panel_ka$retained[panel_ka$any_drafted == 0]), 4), "\n")
cat("N female:", sum(panel_ka$is_female), " N male:", sum(!panel_ka$is_female), "\n")

cat("\n--- All workers: continuous treatment ---\n")
m1 <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)
etable(m1, se.below = TRUE, fitstat = ~n + r2)

cat("\n--- All workers: any drafted ---\n")
m2 <- feols(retained ~ any_drafted + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)
etable(m2, se.below = TRUE, fitstat = ~n + r2)

cat("\n--- By gender: continuous ---\n")
m3f <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
               year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(is_female), cluster = ~office_id)
m3m <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
               year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(!is_female), cluster = ~office_id)
etable(m3f, m3m, se.below = TRUE, fitstat = ~n + r2,
       headers = c("Female", "Male"))

cat("\n--- Interaction: drafted x female ---\n")
m4 <- feols(retained ~ n_drafted_male * is_female + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)
etable(m4, se.below = TRUE, fitstat = ~n + r2)
