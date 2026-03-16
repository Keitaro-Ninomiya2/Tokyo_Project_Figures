################################################################################
# Line 33 by rank: Does female share change at rank 1 positions?
################################################################################

library(tidyverse)
library(fixest)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

years_of_interest <- 1938:1945

# Enriched rank (pre-1948)
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^局長$") ~ 5L,
    str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    TRUE ~ 2L
  )
}

# Build panel (same as Table2 structure)
office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    pos_rank = assign_rank(pos_norm)
  )

pos_ka_map <- staff_transitions %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

# Position-level female share among ALL workers
all_workers <- staff_transitions %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total = n(),
    n_female = sum(is_female, na.rm = TRUE),
    n_male   = sum(!is_female, na.rm = TRUE),
    female_share_all = n_female / n_total,
    pos_rank = first(pos_rank),
    .groups = "drop"
  )

# Female share among NEW hires
new_hires <- staff_transitions %>%
  filter(is_new_hire == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_new_total  = n(),
    n_new_female = sum(is_female, na.rm = TRUE),
    female_share_new = n_new_female / n_new_total,
    .groups = "drop"
  )

# Draft counts
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_male = sum(!is_female, na.rm = TRUE),
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    .groups = "drop"
  )

# Cumulative male baseline
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
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
    n_drafted_male = replace_na(n_drafted_male, 0),
    draft_share    = replace_na(draft_share, 0),
    cumul_n_male   = replace_na(cumul_n_male, 0),
    any_drafted    = as.integer(n_drafted_male > 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

panel_ka <- panel %>% filter(!is.na(ka_id))

# ============================================================
# FEMALE SHARE (ALL WORKERS) BY RANK
# ============================================================

cat("================================================================\n")
cat("COMPOSITION EFFECT: female_share_all ~ drafting, BY RANK\n")
cat("================================================================\n\n")

for (r in 1:3) {
  sub <- panel_ka %>% filter(pos_rank == r)
  cat("--- Rank", r, "---\n")
  cat("  N obs:", nrow(sub), "\n")
  cat("  Mean female_share_all:", round(mean(sub$female_share_all, na.rm=TRUE), 4), "\n")
  cat("  Mean female_share_all (drafted):",
      round(mean(sub$female_share_all[sub$any_drafted == 1], na.rm=TRUE), 4), "\n")
  cat("  Mean female_share_all (non-drafted):",
      round(mean(sub$female_share_all[sub$any_drafted == 0], na.rm=TRUE), 4), "\n")

  # Extensive margin
  m1 <- tryCatch(
    feols(female_share_all ~ any_drafted + cumul_n_male | year_num + ka_id + pos_norm,
          data = sub, cluster = ~office_id),
    error = function(e) { cat("  Ext. OLS failed:", e$message, "\n"); NULL }
  )
  if (!is.null(m1)) {
    cat("  OLS(female_share_all ~ any_drafted): Coef =", round(coef(m1)["any_drafted"], 4),
        " SE =", round(se(m1)["any_drafted"], 4),
        " p =", round(pvalue(m1)["any_drafted"], 4),
        " N =", nobs(m1), "\n")
  }

  # Intensive margin
  m2 <- tryCatch(
    feols(female_share_all ~ draft_share + cumul_n_male | year_num + ka_id + pos_norm,
          data = sub, cluster = ~office_id),
    error = function(e) { cat("  Int. OLS failed:", e$message, "\n"); NULL }
  )
  if (!is.null(m2)) {
    cat("  OLS(female_share_all ~ draft_share): Coef =", round(coef(m2)["draft_share"], 4),
        " SE =", round(se(m2)["draft_share"], 4),
        " p =", round(pvalue(m2)["draft_share"], 4),
        " N =", nobs(m2), "\n")
  }
  cat("\n")
}

# ============================================================
# FEMALE SHARE (NEW HIRES) BY RANK
# ============================================================

cat("================================================================\n")
cat("COMPOSITION EFFECT: female_share_new ~ drafting, BY RANK\n")
cat("================================================================\n\n")

for (r in 1:3) {
  sub <- panel_ka %>% filter(pos_rank == r, !is.na(female_share_new))
  cat("--- Rank", r, "---\n")
  cat("  N obs (with new hires):", nrow(sub), "\n")
  cat("  Mean female_share_new:", round(mean(sub$female_share_new, na.rm=TRUE), 4), "\n")

  m1 <- tryCatch(
    feols(female_share_new ~ any_drafted + cumul_n_male | year_num + ka_id + pos_norm,
          data = sub, cluster = ~office_id),
    error = function(e) { cat("  Ext. OLS failed:", e$message, "\n"); NULL }
  )
  if (!is.null(m1)) {
    cat("  OLS(female_share_new ~ any_drafted): Coef =", round(coef(m1)["any_drafted"], 4),
        " SE =", round(se(m1)["any_drafted"], 4),
        " p =", round(pvalue(m1)["any_drafted"], 4),
        " N =", nobs(m1), "\n")
  }

  m2 <- tryCatch(
    feols(female_share_new ~ draft_share + cumul_n_male | year_num + ka_id + pos_norm,
          data = sub, cluster = ~office_id),
    error = function(e) { cat("  Int. OLS failed:", e$message, "\n"); NULL }
  )
  if (!is.null(m2)) {
    cat("  OLS(female_share_new ~ draft_share): Coef =", round(coef(m2)["draft_share"], 4),
        " SE =", round(se(m2)["draft_share"], 4),
        " p =", round(pvalue(m2)["draft_share"], 4),
        " N =", nobs(m2), "\n")
  }
  cat("\n")
}

cat("===== DONE =====\n")
