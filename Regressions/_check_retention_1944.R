library(tidyverse)
library(fixest)

DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Draft counts at office x pos for 1944
position_drafts <- df_all %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Exclude anyone ever drafted
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# Ka mapping
pos_ka_map <- df %>%
  filter(year_num == 1944, !is.na(ka)) %>%
  count(office_id, pos_norm, ka, kyoku) %>%
  group_by(office_id, pos_norm) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, ka, kyoku)

# Cumulative male baseline
cumul_male_stock <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop")

# Non-drafted workers ever observed 1938-1943, check if retained in 1944
# Keep ka/kyoku from their most recent year for FE
pre_existing <- df %>%
  filter(year_num %in% 1938:1943, !(staff_id %in% drafted_ids)) %>%
  group_by(staff_id) %>%
  slice_max(year_num, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  distinct(staff_id, office_id, pos_norm, is_female, ka, kyoku)

appears_1944 <- df %>%
  filter(year_num == 1944) %>%
  distinct(staff_id) %>%
  mutate(retained = 1L)

retention <- pre_existing %>%
  left_join(appears_1944, by = "staff_id") %>%
  mutate(retained = replace_na(retained, 0L)) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

cat("1944 retention panel (all):", nrow(retention), "obs\n")
cat("Mean retained:", round(mean(retention$retained), 4), "\n")
cat("N female:", sum(retention$is_female), " N male:", sum(!retention$is_female), "\n")
cat("Positions with drafts:", sum(retention$n_drafted_male > 0), "\n")
cat("With ka_id:", sum(!is.na(retention$ka_id)), "\n")

cat("\n===== RETENTION 1944 (no ka FE) =====\n")
m1 <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
              pos_norm,
            data = retention, cluster = ~office_id)

m2 <- feols(retained ~ n_drafted_male * is_female + log(cumul_n_male + 1) |
              pos_norm,
            data = retention, cluster = ~office_id)

etable(m1, m2, se.below = TRUE, fitstat = ~n + r2,
       headers = c("All", "Interaction"))

panel_ka <- retention %>% filter(!is.na(ka_id))
if (sum(panel_ka$n_drafted_male > 0) > 0) {
  cat("\n===== RETENTION 1944 (with ka FE) =====\n")
  m3 <- feols(retained ~ n_drafted_male + log(cumul_n_male + 1) |
                ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
  etable(m3, se.below = TRUE, fitstat = ~n + r2)
}
