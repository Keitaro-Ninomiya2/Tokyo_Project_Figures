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
kanri_pattern <- "^主事$|^技師$|^視学$|^理事$"
df <- df %>% filter(!str_detect(pos_norm, kanri_pattern))
df_all <- df_all %>% filter(!str_detect(pos_norm, kanri_pattern))

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^局長$") ~ 5L,
    str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    TRUE ~ 2L
  )
}

office_initial_year <- df %>% group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    is_new_hire = case_when(year_num == office_first_year ~ NA, TRUE ~ (year_num == first_year)),
    pos_rank = assign_rank(pos_norm)
  )

pos_ka_map <- staff_transitions %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

all_workers <- staff_transitions %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_total = n(), n_female = sum(is_female), n_male = sum(!is_female),
            pos_rank = first(pos_rank), .groups = "drop")

new_hires <- staff_transitions %>%
  filter(is_new_hire == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_new_total = n(), n_new_female = sum(is_female), n_new_male = sum(!is_female),
            .groups = "drop")

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE), .groups = "drop")

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

panel <- all_workers %>%
  left_join(new_hires, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    n_new_female = replace_na(n_new_female, 0),
    n_new_male = replace_na(n_new_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

panel_ka <- panel %>% filter(!is.na(ka_id))

cat("\n===== SPLIT BY RANK: OLS =====\n")
cat("\n--- Rank 1 (yatoi) ---\n")
r1f_ols <- feols(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(pos_rank == 1), cluster = ~office_id)
r1m_ols <- feols(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(pos_rank == 1), cluster = ~office_id)
etable(r1f_ols, r1m_ols, se.below = TRUE, fitstat = ~n,
       headers = c("Female (R1)", "Male (R1)"))

cat("\n--- Rank 2 (regular) ---\n")
r2f_ols <- feols(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(pos_rank == 2), cluster = ~office_id)
r2m_ols <- feols(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka %>% filter(pos_rank == 2), cluster = ~office_id)
etable(r2f_ols, r2m_ols, se.below = TRUE, fitstat = ~n,
       headers = c("Female (R2)", "Male (R2)"))

cat("\n===== SPLIT BY RANK: POISSON =====\n")
cat("\n--- Rank 1 (yatoi) ---\n")
tryCatch({
  r1f_poi <- fepois(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
               data = panel_ka %>% filter(pos_rank == 1), cluster = ~office_id)
  r1m_poi <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
               data = panel_ka %>% filter(pos_rank == 1), cluster = ~office_id)
  etable(r1f_poi, r1m_poi, se.below = TRUE, fitstat = ~n,
         headers = c("Female (R1)", "Male (R1)"))
}, error = function(e) cat("Poisson R1 error:", e$message, "\n"))

cat("\n--- Rank 2 (regular) ---\n")
tryCatch({
  r2f_poi <- fepois(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
               data = panel_ka %>% filter(pos_rank == 2), cluster = ~office_id)
  r2m_poi <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
               data = panel_ka %>% filter(pos_rank == 2), cluster = ~office_id)
  etable(r2f_poi, r2m_poi, se.below = TRUE, fitstat = ~n,
         headers = c("Female (R2)", "Male (R2)"))
}, error = function(e) cat("Poisson R2 error:", e$message, "\n"))

cat("\n===== POOLED POISSON (all ranks) =====\n")
pf <- fepois(n_new_female ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)
pm <- fepois(n_new_male ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)
etable(pf, pm, se.below = TRUE, fitstat = ~n,
       headers = c("Female (pooled)", "Male (pooled)"))
