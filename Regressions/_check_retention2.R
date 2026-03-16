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

# ============================================================
# RETENTION OF PRE-EXISTING (NON-DRAFTED) WORKERS
# ============================================================
# For each year t with drafting:
#   - Take non-drafted workers observed in year t-1 at office x position
#   - Check if they appear in year t (retained through the draft year)
#   - Treatment: number of drafts at their office x position in year t

# Draft counts at office x pos x year level
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Drafted staff IDs (to exclude from retention sample)
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# Ka mapping
pos_ka_map <- df %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

# Cumulative male baseline
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# For each draft year t, get non-drafted workers from t-1
# and check if they appear in year t
retention_rows <- list()

for (t in years_of_interest) {
  # Workers in year t-1 (pre-existing), excluding anyone ever drafted
  pre_existing <- df %>%
    filter(year_num == t - 1, !(staff_id %in% drafted_ids)) %>%
    distinct(staff_id, office_id, pos_norm, is_female)
  
  # Who appears in year t? (at any office)
  appears_t <- df %>%
    filter(year_num == t) %>%
    distinct(staff_id) %>%
    mutate(retained = 1L)
  
  # Merge
  ret <- pre_existing %>%
    left_join(appears_t, by = "staff_id") %>%
    mutate(retained = replace_na(retained, 0L),
           draft_year = t)
  
  retention_rows[[as.character(t)]] <- ret
}

retention_panel <- bind_rows(retention_rows) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "draft_year" = "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "draft_year" = "year_num")) %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "draft_year" = "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    any_drafted = as.integer(n_drafted_male > 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    year_num = draft_year
  )

panel_ka <- retention_panel %>% filter(!is.na(ka_id))

cat("===== RETENTION OF NON-DRAFTED PRE-EXISTING WORKERS =====\n")
cat("Total obs:", nrow(panel_ka), "\n")
cat("Mean retained:", round(mean(panel_ka$retained), 4), "\n")
cat("Retained (drafted pos):", round(mean(panel_ka$retained[panel_ka$any_drafted == 1]), 4), "\n")
cat("Retained (non-drafted):", round(mean(panel_ka$retained[panel_ka$any_drafted == 0]), 4), "\n")
cat("N female:", sum(panel_ka$is_female), " N male:", sum(!panel_ka$is_female), "\n")
cat("\nBy year:\n")
panel_ka %>% group_by(year_num) %>%
  summarise(n = n(), mean_ret = round(mean(retained), 4),
            n_drafted_pos = sum(any_drafted),
            .groups = "drop") %>%
  print(n = 20)

cat("\n--- All workers: continuous ---\n")
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
