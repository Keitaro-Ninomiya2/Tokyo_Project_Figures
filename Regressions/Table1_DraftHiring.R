################################################################################
# Table 1: Drafting → New Hiring (Poisson)
# Unit: office_id × pos_norm × year
# Treatment: n_drafted_male (own position), adjacent_drafts (other positions in office)
# Outcome: n_new_hires, n_new_hires_female, n_new_hires_male
# FE: year + ka + pos_norm
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

# ============================================================
# 1. CUMULATIVE MALE BASELINE
# ============================================================

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# ============================================================
# 2. NEW HIRE IDENTIFICATION
# ============================================================

office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

# Lag: where was each person last year?
staff_lag <- df %>%
  select(staff_id, year_num,
         lag_office = office_id,
         lag_pos    = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    # Transfer in = appeared in a DIFFERENT office last year
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id)
  )

# Transfers OUT: count workers who LEFT each (office, pos) to go elsewhere
# For each transfer-in, the origin is (lag_office, lag_pos)
transfers_out <- staff_transitions %>%
  filter(is_transfer_in == TRUE) %>%
  group_by(office_id = lag_office, pos_norm = lag_pos, year_num) %>%
  summarise(n_transfers_out = n(), .groups = "drop")

# ============================================================
# 3. POSITION OUTCOMES (office × ka × pos_norm × year)
# ============================================================

# Keep ka for FE — use modal ka per (office_id, pos_norm, year)
pos_ka_map <- staff_transitions %>%
  filter(!is.na(ka)) %>%
  count(office_id, pos_norm, year_num, ka, kyoku) %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kyoku)

position_outcomes <- staff_transitions %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_new_hires        = sum(is_new_hire, na.rm = TRUE),
    n_new_hires_female = sum(is_new_hire & is_female, na.rm = TRUE),
    n_new_hires_male   = sum(is_new_hire & !is_female, na.rm = TRUE),
    n_transfers_in     = sum(is_transfer_in, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 4. DRAFT COUNTS
# ============================================================

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

# ============================================================
# 5. ASSEMBLE PANEL
# ============================================================

panel <- position_outcomes %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(transfers_out, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted_male, cumul_n_male, n_transfers_out), ~replace_na(., 0))
  )

# Adjacent drafts: other positions within same office
office_totals <- panel %>%
  group_by(office_id, year_num) %>%
  summarise(office_total_drafts = sum(n_drafted_male), .groups = "drop")

panel <- panel %>%
  left_join(office_totals, by = c("office_id", "year_num")) %>%
  mutate(
    adjacent_drafts = office_total_drafts - n_drafted_male,
    # Unique ka ID for FE (ka names may repeat across kyoku)
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

cat("\n===== PANEL DIAGNOSTICS =====\n")
cat("Total obs:", nrow(panel), "\n")
cat("With drafts:", sum(panel$n_drafted_male > 0), "\n")
cat("With transfers out:", sum(panel$n_transfers_out > 0), "\n")
cat("With ka_id:", sum(!is.na(panel$ka_id)), "\n")
cat("Unique ka_id:", n_distinct(panel$ka_id, na.rm = TRUE), "\n")
cat("Unique kyoku:", n_distinct(panel$kyoku, na.rm = TRUE), "\n")

# ============================================================
# 6. POISSON MODELS
# ============================================================

# --- Panel A: Ka FE ---
panel_ka <- panel %>% filter(!is.na(ka_id))

cat("\n===== Panel A: Ka FE =====\n")
cat("Obs for ka FE:", nrow(panel_ka), "\n")

# Own drafts only
m1a <- fepois(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
m2a <- fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
m3a <- fepois(n_new_hires_male ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)

# Own + adjacent drafts
m4a <- fepois(n_new_hires ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
m5a <- fepois(n_new_hires_female ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
m6a <- fepois(n_new_hires_male ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)

# Transfers (poaching from other offices)
m7a <- fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
m8a <- fepois(n_transfers_in ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)

cat("\nResults (Ka FE):\n")
etable(m1a, m2a, m3a, m7a, m4a, m5a, m6a, m8a,
       headers = c("All", "Female", "Male", "Transfers",
                    "All", "Female", "Male", "Transfers"),
       se.below = TRUE, fitstat = ~n)

# ============================================================
# 7. SECOND-ORDER: Offices that LOST workers to poaching
# ============================================================
# Treatment: n_transfers_out (workers poached away from this position)
# Outcome: new female hires at the losing office
# Restricted to positions with NO own drafting (isolates the poaching channel)

panel_ka_donor <- panel_ka %>% filter(n_drafted_male == 0)

cat("\n===== SECOND-ORDER: Donor offices (no own drafts) =====\n")
cat("Ka panel donor obs:", nrow(panel_ka_donor), "\n")
cat("  with transfers out:", sum(panel_ka_donor$n_transfers_out > 0), "\n")

s1a <- fepois(n_new_hires ~ n_transfers_out + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka_donor, cluster = ~office_id)
s2a <- fepois(n_new_hires_female ~ n_transfers_out + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka_donor, cluster = ~office_id)
s3a <- fepois(n_new_hires_male ~ n_transfers_out + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka_donor, cluster = ~office_id)

cat("\nSecond-order (Ka FE):\n")
etable(s1a, s2a, s3a,
       headers = c("All", "Female", "Male"),
       se.below = TRUE, fitstat = ~n)

# ============================================================
# 8. EXPORT LaTeX
# ============================================================

dict_t1 <- c(
  n_drafted_male   = "Own drafts",
  adjacent_drafts  = "Adjacent drafts",
  n_transfers_out  = "Workers poached away",
  "log(cumul_n_male + 1)" = "log(Male baseline + 1)",
  n_new_hires      = "All",
  n_new_hires_female = "Female",
  n_new_hires_male   = "Male",
  n_transfers_in     = "Transfers"
)

tex_a <- etable(
  m1a, m2a, m3a, m7a, m4a, m5a, m6a, m8a,
  dict = dict_t1,
  headers = c("All", "Female", "Male", "Transfers",
              "All", "Female", "Male", "Transfers"),
  se.below = TRUE, fitstat = ~n,
  tex = TRUE
)

# Second-order table
tex_s_a <- etable(
  s1a, s2a, s3a,
  dict = dict_t1,
  headers = c("All", "Female", "Male"),
  se.below = TRUE, fitstat = ~n,
  tex = TRUE
)

# Collapse duplicate depvar row (fixest outputs both depvar labels and headers)
tex_lines <- strsplit(paste(tex_a, collapse = "\n"), "\n")[[1]]
dup_idx <- grep("^\\s+& All\\s+& Female", tex_lines)
if (length(dup_idx) > 0) tex_lines <- tex_lines[-dup_idx[1]]

tex_out <- c(
  "% Table 1: Drafting -> New Hiring (Poisson)",
  "% Ka Fixed Effects",
  tex_lines
)

writeLines(tex_out, here("Table1_DraftHiring.tex"))
cat("\nTable 1 exported to Table1_DraftHiring.tex\n")

tex_out_s <- c(
  "% Table 1b: Second-order — Hiring at offices that lost workers to poaching",
  "% Sample: positions with NO own drafting. Treatment: workers transferred out.",
  "% Ka Fixed Effects",
  tex_s_a
)

writeLines(tex_out_s, here("Table1b_DonorHiring.tex"))
cat("Table 1b exported to Table1b_DonorHiring.tex\n")
