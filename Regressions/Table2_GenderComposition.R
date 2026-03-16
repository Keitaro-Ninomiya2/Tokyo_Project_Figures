################################################################################
# Table 2: Drafting → Gender Composition Shift (OLS)
# Unit: office_id × pos_norm × year
# Treatment: any_drafted (extensive), draft_share (intensive)
# Outcome: female_share among new hires, female_share among all workers
# Sample: Exclude 官吏 positions (主事, 技師, 視学, 理事)
# FE: Panel A = year + ka + pos_norm, Panel B = year + kyoku + pos_norm
################################################################################

library(tidyverse)
library(fixest)
library(here)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

df_all <- read_csv(DATA_PATH,
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", "")
  )

years_of_interest <- 1938:1945

# Exclude 官吏 (kanri) positions
kanri_pattern <- "^主事$|^技師$|^視学$|^理事$"

df <- df %>% filter(!str_detect(pos_norm, kanri_pattern))
df_all <- df_all %>% filter(!str_detect(pos_norm, kanri_pattern))

cat("After excluding 官吏: df has", nrow(df), "rows\n")

# ============================================================
# 1. NEW HIRE IDENTIFICATION
# ============================================================

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
    )
  )

# ============================================================
# 2. POSITION-LEVEL OUTCOMES
# ============================================================

# Modal ka/kyoku per (office_id, pos_norm, year)
pos_ka_map <- staff_transitions %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

# Female share among ALL workers at position level
all_workers <- staff_transitions %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total     = n(),
    n_female    = sum(is_female, na.rm = TRUE),
    female_share_all = n_female / n_total,
    .groups = "drop"
  )

# Female share among NEW HIRES at position level
new_hires <- staff_transitions %>%
  filter(is_new_hire == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_new_total  = n(),
    n_new_female = sum(is_female, na.rm = TRUE),
    female_share_new = n_new_female / n_new_total,
    .groups = "drop"
  )

# ============================================================
# 3. DRAFT COUNTS
# ============================================================

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_male         = sum(!is_female, na.rm = TRUE),
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share    = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    .groups = "drop"
  )

# Cumulative male baseline (for controls)
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# ============================================================
# 4. ASSEMBLE PANEL
# ============================================================

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
    ka_id          = if_else(!is.na(ka) & !is.na(kyoku),
                             paste(kyoku, ka, sep = "_"), NA_character_)
  )

cat("\n===== PANEL DIAGNOSTICS =====\n")
cat("Total obs:", nrow(panel), "\n")
cat("Positions with any drafting:", sum(panel$any_drafted), "\n")
cat("Obs with new hires:", sum(!is.na(panel$female_share_new)), "\n")
cat("Mean female_share_all:", round(mean(panel$female_share_all, na.rm = TRUE), 4), "\n")
cat("Mean female_share_new (where defined):", round(mean(panel$female_share_new, na.rm = TRUE), 4), "\n")

# ============================================================
# 5. OLS REGRESSIONS
# ============================================================

# --- Panel A: Ka FE ---
panel_ka <- panel %>% filter(!is.na(ka_id))

cat("\n===== Panel A: Ka FE =====\n")

# Extensive margin (any_drafted)
m1a <- feols(female_share_new ~ any_drafted + cumul_n_male |
               year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)
m2a <- feols(female_share_all ~ any_drafted + cumul_n_male |
               year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)

# Intensive margin (draft_share)
m3a <- feols(female_share_new ~ draft_share + cumul_n_male |
               year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)
m4a <- feols(female_share_all ~ draft_share + cumul_n_male |
               year_num + ka_id + pos_norm,
             data = panel_ka, cluster = ~office_id)

cat("\nPanel A Results (Ka FE):\n")
etable(m1a, m2a, m3a, m4a,
       headers = c("Share (New)", "Share (All)", "Share (New)", "Share (All)"),
       se.below = TRUE, fitstat = ~n + r2)

# --- Panel B: Kyoku FE ---
panel_ky <- panel %>% filter(!is.na(kyoku))

cat("\n===== Panel B: Kyoku FE =====\n")

m1b <- feols(female_share_new ~ any_drafted + cumul_n_male |
               year_num + kyoku + pos_norm,
             data = panel_ky, cluster = ~office_id)
m2b <- feols(female_share_all ~ any_drafted + cumul_n_male |
               year_num + kyoku + pos_norm,
             data = panel_ky, cluster = ~office_id)

m3b <- feols(female_share_new ~ draft_share + cumul_n_male |
               year_num + kyoku + pos_norm,
             data = panel_ky, cluster = ~office_id)
m4b <- feols(female_share_all ~ draft_share + cumul_n_male |
               year_num + kyoku + pos_norm,
             data = panel_ky, cluster = ~office_id)

cat("\nPanel B Results (Kyoku FE):\n")
etable(m1b, m2b, m3b, m4b,
       headers = c("Share (New)", "Share (All)", "Share (New)", "Share (All)"),
       se.below = TRUE, fitstat = ~n + r2)

# ============================================================
# 6. EXPORT LaTeX
# ============================================================

dict_t2 <- c(
  any_drafted    = "Any drafted (=1)",
  draft_share    = "Draft share",
  cumul_n_male   = "Male baseline (count)"
)

tex_a <- etable(
  m1a, m2a, m3a, m4a,
  dict = dict_t2,
  headers = c("New Hires", "All Workers", "New Hires", "All Workers"),
  se.below = TRUE, fitstat = ~n + r2,
  tex = TRUE
)

tex_b <- etable(
  m1b, m2b, m3b, m4b,
  dict = dict_t2,
  headers = c("New Hires", "All Workers", "New Hires", "All Workers"),
  se.below = TRUE, fitstat = ~n + r2,
  tex = TRUE
)

tex_out <- c(
  "% Table 2: Drafting -> Gender Composition (OLS)",
  "% Dep. var.: Female share. Sample excludes kanri (官吏) positions.",
  "% Panel A: Ka (Office) Fixed Effects",
  tex_a,
  "",
  "% Panel B: Kyoku (Department) Fixed Effects",
  tex_b
)

writeLines(tex_out, here("Table2_GenderComposition.tex"))
cat("\nTable exported to Table2_GenderComposition.tex\n")
