################################################################################
# Master Script: Reproduce every result statement in Introduction.tex
# Each section maps to one sentence (line) in the Introduction
# Output: console report with coefficients, SEs, and descriptive stats
################################################################################

library(tidyverse)
library(fixest)

# ============================================================
# 0. LOAD DATA (shared across all regressions)
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

cat("Loading data from:", DATA_PATH, "\n")
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

cat("Data loaded. df:", nrow(df), "rows. df_all:", nrow(df_all), "rows.\n\n")

# ============================================================
# 0b. RANK VARIABLE CONSTRUCTION
# ============================================================
# Pre-1948: rank 3 = 主事/技師, rank 2 = Others, rank 1 = 雇/嘱託
# Post-1948: rank 3 = 係長, rank 2 = Others, rank 1 = 雇/嘱託

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^嘱託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^嘱託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

df <- df %>% mutate(rank = assign_rank(pos_norm, year_num))
df_all <- df_all %>% mutate(rank = assign_rank(pos_norm, year_num))

# ============================================================
# 0c. SHARED PANEL CONSTRUCTION (Table 1 base)
# ============================================================

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  select(staff_id, year_num,
         lag_office = office_id, lag_ka = ka, lag_kyoku = kyoku,
         lag_pos = pos_norm) %>%
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
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id)
  )

# Network distance for each transfer
staff_transitions <- staff_transitions %>%
  mutate(
    transfer_distance = case_when(
      !is_transfer_in ~ NA_integer_,
      !is.na(lag_ka) & !is.na(ka) & lag_ka == ka & lag_kyoku == kyoku ~ 1L,
      !is.na(lag_kyoku) & !is.na(kyoku) & lag_kyoku == kyoku ~ 2L,
      TRUE ~ 3L
    )
  )

# Transfers OUT
transfers_out <- staff_transitions %>%
  filter(is_transfer_in == TRUE) %>%
  group_by(office_id = lag_office, pos_norm = lag_pos, year_num) %>%
  summarise(n_transfers_out = n(), .groups = "drop")

# Position outcomes
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
    n_transfers_dist1  = sum(transfer_distance == 1, na.rm = TRUE),
    n_transfers_dist2  = sum(transfer_distance == 2, na.rm = TRUE),
    n_transfers_dist3  = sum(transfer_distance == 3, na.rm = TRUE),
    .groups = "drop"
  )

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

panel <- position_outcomes %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(transfers_out, by = c("office_id", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted_male, cumul_n_male, n_transfers_out), ~replace_na(., 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

# Adjacent drafts
office_totals <- panel %>%
  group_by(office_id, year_num) %>%
  summarise(office_total_drafts = sum(n_drafted_male), .groups = "drop")

panel <- panel %>%
  left_join(office_totals, by = c("office_id", "year_num")) %>%
  mutate(adjacent_drafts = office_total_drafts - n_drafted_male)

# Rank of position
panel <- panel %>%
  mutate(
    pos_rank = case_when(
      str_detect(pos_norm, "^主事$|^技師$") ~ 3L,
      str_detect(pos_norm, "^雇$|^嘱託$")   ~ 1L,
      TRUE                                   ~ 2L
    )
  )

panel_ka <- panel %>% filter(!is.na(ka_id))
panel_ky <- panel %>% filter(!is.na(kyoku))

cat("Panel constructed:", nrow(panel), "obs\n")
cat("  ka panel:", nrow(panel_ka), "\n")
cat("  kyoku panel:", nrow(panel_ky), "\n\n")

# ==============================================================================
# ██████████████████████████████████████████████████████████████████████████████
# RESULT SECTION 1: INTERNAL TRANSFERS AND HIRING (Lines 21-27)
# ██████████████████████████████████████████████████████████████████████████████
# ==============================================================================

cat("================================================================\n")
cat("RESULT SECTION 1: INTERNAL TRANSFERS AND HIRING\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# LINE 22: "A draft-induced exit increases the number of internal transfers
#           into the office by 5% more than in the adjacent office."
# Regression: Poisson(n_transfers_in ~ n_drafted_male | FE), ka FE
# ------------------------------------------------------------------------------

cat("--- LINE 22: Transfers In ~ Own Drafts (Poisson, Ka FE) ---\n")
L22 <- fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
cat("Coef (n_drafted_male):", round(coef(L22)["n_drafted_male"], 4),
    "  SE:", round(se(L22)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L22)["n_drafted_male"], 4), "\n")
cat("Interpretation: A one-unit increase in own drafts increases transfers in by",
    round(100*(exp(coef(L22)["n_drafted_male"]) - 1), 1), "%\n")
cat("N:", nobs(L22), "\n\n")

# Descriptive: baseline transfers in
cat("  Descriptive: mean n_transfers_in =", round(mean(panel_ka$n_transfers_in), 3), "\n")
cat("  Descriptive: mean n_transfers_in (drafted positions) =",
    round(mean(panel_ka$n_transfers_in[panel_ka$n_drafted_male > 0]), 3), "\n")
cat("  Descriptive: mean n_transfers_in (non-drafted positions) =",
    round(mean(panel_ka$n_transfers_in[panel_ka$n_drafted_male == 0]), 3), "\n\n")

# Also with adjacent drafts
L22b <- fepois(n_transfers_in ~ n_drafted_male + adjacent_drafts + log(cumul_n_male + 1) |
                 year_num + ka_id + pos_norm,
               data = panel_ka, cluster = ~office_id)
cat("With adjacent drafts controlled:\n")
cat("  Coef (n_drafted_male):", round(coef(L22b)["n_drafted_male"], 4),
    "  SE:", round(se(L22b)["n_drafted_male"], 4), "\n")
cat("  Coef (adjacent_drafts):", round(coef(L22b)["adjacent_drafts"], 4),
    "  SE:", round(se(L22b)["adjacent_drafts"], 4), "\n\n")

# ------------------------------------------------------------------------------
# LINE 23: "This was driven by promoting workers in lower-ranked positions
#           and poaching workers from external offices."
# Evidence: transfers in are significant (L22), + decomposition by rank
# We show: new hires at low-rank positions increase when high-rank drafted
# ------------------------------------------------------------------------------

cat("--- LINE 23: Decomposition by rank ---\n")
# Low-rank positions (rank 1-2) receiving transfers when high-rank (rank 3) drafted
L23_low <- tryCatch(
  fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka %>% filter(pos_rank <= 2), cluster = ~office_id),
  error = function(e) { cat("  Low-rank Poisson failed:", e$message, "\n"); NULL }
)
L23_high <- tryCatch(
  fepois(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka %>% filter(pos_rank == 3), cluster = ~office_id),
  error = function(e) { cat("  High-rank Poisson failed:", e$message, "\n"); NULL }
)

if (!is.null(L23_low)) {
  cat("Transfers in at LOW-rank positions (rank 1-2):\n")
  cat("  Coef:", round(coef(L23_low)["n_drafted_male"], 4),
      "  SE:", round(se(L23_low)["n_drafted_male"], 4),
      "  N:", nobs(L23_low), "\n")
}
if (!is.null(L23_high)) {
  cat("Transfers in at HIGH-rank positions (rank 3):\n")
  cat("  Coef:", round(coef(L23_high)["n_drafted_male"], 4),
      "  SE:", round(se(L23_high)["n_drafted_male"], 4),
      "  N:", nobs(L23_high), "\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# LINE 24: "The offices selected to give up workers to the impacted office are
#           not immediate neighbors of the impacted office, as I do not find
#           effects on the chances of being selected by distance to the office
#           measured by network."
# Network distance: same ka = 1, same kyoku diff ka = 2, diff kyoku = 3
# Regression: Poisson(n_transfers_distK ~ n_drafted_male | FE) for K = 1,2,3
# ------------------------------------------------------------------------------

cat("--- LINE 24: Transfers by network distance ---\n")
cat("Distance definition: 1=same ka, 2=same kyoku diff ka, 3=diff kyoku\n\n")

for (d in 1:3) {
  dv <- paste0("n_transfers_dist", d)
  cat("  Distance", d, "(DV:", dv, "):\n")
  f <- as.formula(paste0(dv, " ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm"))
  m <- tryCatch(
    fepois(f, data = panel_ka, cluster = ~office_id),
    error = function(e) { cat("    Poisson failed:", e$message, "\n"); NULL }
  )
  if (is.null(m)) {
    # Try OLS fallback
    m <- tryCatch(
      feols(f, data = panel_ka, cluster = ~office_id),
      error = function(e) { cat("    OLS also failed:", e$message, "\n"); NULL }
    )
    if (!is.null(m)) cat("    (OLS fallback)\n")
  }
  if (!is.null(m)) {
    cat("    Coef:", round(coef(m)["n_drafted_male"], 4),
        "  SE:", round(se(m)["n_drafted_male"], 4),
        "  pval:", round(pvalue(m)["n_drafted_male"], 4),
        "  N:", nobs(m), "\n")
  }
}

# Descriptive: distribution of transfer distances
transfer_events <- staff_transitions %>% filter(is_transfer_in == TRUE)
cat("\n  Descriptive: transfer distance distribution\n")
cat("    Total transfers:", nrow(transfer_events), "\n")
dist_tab <- table(transfer_events$transfer_distance, useNA = "ifany")
for (nm in names(dist_tab)) {
  cat("    Distance", nm, ":", dist_tab[nm],
      "(", round(100*dist_tab[nm]/sum(dist_tab), 1), "%)\n")
}
cat("\n")

# ------------------------------------------------------------------------------
# LINE 25: "The selected offices did not change their number of new hires as a
#           result of having their workers removed to fill the impacted office's
#           vacancy."
# Regression: Poisson(n_new_hires ~ n_transfers_out | FE) on positions with
#             NO own drafting (donor office isolation)
# ------------------------------------------------------------------------------

cat("--- LINE 25: Donor offices — new hires ~ workers poached away ---\n")
panel_ka_donor <- panel_ka %>% filter(n_drafted_male == 0)

L25 <- fepois(n_new_hires ~ n_transfers_out + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka_donor, cluster = ~office_id)
cat("Coef (n_transfers_out):", round(coef(L25)["n_transfers_out"], 4),
    "  SE:", round(se(L25)["n_transfers_out"], 4),
    "  pval:", round(pvalue(L25)["n_transfers_out"], 4), "\n")
cat("N:", nobs(L25), "(positions with 0 own drafts)\n")
cat("  Obs with transfers out:", sum(panel_ka_donor$n_transfers_out > 0), "\n\n")

# ------------------------------------------------------------------------------
# LINE 27: "Office units that managed to find a replacement from external
#           offices had [] lower numbers of new hires."
# Regression: offices with transfers in vs without — diff in new hires
# Approach: include both own drafts and transfers_in as regressors
# ------------------------------------------------------------------------------

cat("--- LINE 27: Replacement via transfer → fewer new hires ---\n")
# Among drafted positions, did getting a transfer reduce new hiring?
panel_drafted <- panel_ka %>% filter(n_drafted_male > 0)

L27 <- tryCatch(
  fepois(n_new_hires ~ n_drafted_male + n_transfers_in + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_drafted, cluster = ~office_id),
  error = function(e) {
    cat("  Poisson failed, trying OLS\n")
    feols(n_new_hires ~ n_drafted_male + n_transfers_in + log(cumul_n_male + 1) |
            year_num + ka_id + pos_norm,
          data = panel_drafted, cluster = ~office_id)
  }
)
cat("Among positions with own drafts:\n")
if ("n_transfers_in" %in% names(coef(L27))) {
  cat("  Coef (n_transfers_in):", round(coef(L27)["n_transfers_in"], 4),
      "  SE:", round(se(L27)["n_transfers_in"], 4),
      "  pval:", round(pvalue(L27)["n_transfers_in"], 4), "\n")
}
cat("  Coef (n_drafted_male):", round(coef(L27)["n_drafted_male"], 4),
    "  SE:", round(se(L27)["n_drafted_male"], 4), "\n")
cat("  N:", nobs(L27), "\n")

# Also: descriptive comparison
got_transfer <- panel_drafted %>% filter(n_transfers_in > 0)
no_transfer  <- panel_drafted %>% filter(n_transfers_in == 0)
cat("  Descriptive: Drafted positions WITH transfer: mean new hires =",
    round(mean(got_transfer$n_new_hires), 3), "(N=", nrow(got_transfer), ")\n")
cat("  Descriptive: Drafted positions WITHOUT transfer: mean new hires =",
    round(mean(no_transfer$n_new_hires), 3), "(N=", nrow(no_transfer), ")\n")
cat("  Difference:", round(mean(got_transfer$n_new_hires) - mean(no_transfer$n_new_hires), 3), "\n\n")


# ==============================================================================
# ██████████████████████████████████████████████████████████████████████████████
# RESULT SECTION 2: GENDER COMPOSITION (Lines 29-33)
# ██████████████████████████████████████████████████████████████████████████████
# ==============================================================================

cat("================================================================\n")
cat("RESULT SECTION 2: GENDER COMPOSITION\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# LINE 30: "Impacted units induced an increase in the number of new hires by 4
#           percent more than their adjacent units, which translates to []
#           headcounts on average."
# Regression: Poisson(n_new_hires ~ n_drafted_male | FE)
# ------------------------------------------------------------------------------

cat("--- LINE 30: New hires ~ own drafts (Poisson, Ka FE) ---\n")
L30 <- fepois(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = panel_ka, cluster = ~office_id)
cat("Coef:", round(coef(L30)["n_drafted_male"], 4),
    "  SE:", round(se(L30)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L30)["n_drafted_male"], 4), "\n")
pct_change <- round(100*(exp(coef(L30)["n_drafted_male"]) - 1), 1)
cat("Percent change per draft:", pct_change, "%\n")

# Headcount translation
baseline_hires <- mean(panel_ka$n_new_hires)
headcount_change <- baseline_hires * (exp(coef(L30)["n_drafted_male"]) - 1)
cat("Baseline mean new hires:", round(baseline_hires, 3), "\n")
cat("Headcount change per draft:", round(headcount_change, 3), "\n")
cat("N:", nobs(L30), "\n\n")

# ------------------------------------------------------------------------------
# LINE 29: "I do not find a differential effect of drafting on the number of
#           male and female workers entering the workplace."
# + LINE 31: "The exiters were more likely to be replaced by a male new hire"
# Regression: Poisson(n_new_hires_female ~ draft) vs Poisson(n_new_hires_male ~ draft)
# ------------------------------------------------------------------------------

cat("--- LINES 29 & 31: Female vs Male new hires ~ drafts ---\n")
L29_f <- fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                  year_num + ka_id + pos_norm,
                data = panel_ka, cluster = ~office_id)
L29_m <- fepois(n_new_hires_male ~ n_drafted_male + log(cumul_n_male + 1) |
                  year_num + ka_id + pos_norm,
                data = panel_ka, cluster = ~office_id)

cat("Female new hires:\n")
cat("  Coef:", round(coef(L29_f)["n_drafted_male"], 4),
    "  SE:", round(se(L29_f)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L29_f)["n_drafted_male"], 4),
    "  N:", nobs(L29_f), "\n")
cat("Male new hires:\n")
cat("  Coef:", round(coef(L29_m)["n_drafted_male"], 4),
    "  SE:", round(se(L29_m)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L29_m)["n_drafted_male"], 4),
    "  N:", nobs(L29_m), "\n")

# Descriptive: gender split of new hires
cat("\n  Descriptive: Total new hires across all position-years:\n")
cat("    Female:", sum(panel_ka$n_new_hires_female), "\n")
cat("    Male:", sum(panel_ka$n_new_hires_male), "\n")
cat("    Female share:", round(sum(panel_ka$n_new_hires_female) /
      (sum(panel_ka$n_new_hires_female) + sum(panel_ka$n_new_hires_male)), 3), "\n\n")

# ------------------------------------------------------------------------------
# LINE 32: "Replacement with female new hires was concentrated in lower-ranked
#           positions, as a draft induced a []% increase in the number of female
#           new hires in low-ranked roles and a []% change in higher-ranked positions."
# Regression: Poisson(n_new_hires_female ~ draft | FE) by rank subsample
# ------------------------------------------------------------------------------

cat("--- LINE 32: Female new hires by rank ---\n")

# Low rank (rank 1-2) vs high rank (rank 3)
L32_low <- tryCatch(
  fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka %>% filter(pos_rank <= 2), cluster = ~office_id),
  error = function(e) { cat("  Low-rank Poisson failed:", e$message, "\n"); NULL }
)
L32_high <- tryCatch(
  fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka %>% filter(pos_rank == 3), cluster = ~office_id),
  error = function(e) {
    cat("  High-rank Poisson failed, trying OLS\n")
    tryCatch(
      feols(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka %>% filter(pos_rank == 3), cluster = ~office_id),
      error = function(e2) { cat("  OLS also failed:", e2$message, "\n"); NULL }
    )
  }
)

if (!is.null(L32_low)) {
  pct_low <- round(100*(exp(coef(L32_low)["n_drafted_male"]) - 1), 1)
  cat("Low-rank (rank 1-2): female new hires\n")
  cat("  Coef:", round(coef(L32_low)["n_drafted_male"], 4),
      "  SE:", round(se(L32_low)["n_drafted_male"], 4),
      "  pval:", round(pvalue(L32_low)["n_drafted_male"], 4), "\n")
  cat("  Percent change:", pct_low, "%\n")
  cat("  N:", nobs(L32_low), "\n")
}
if (!is.null(L32_high)) {
  cat("High-rank (rank 3): female new hires\n")
  cat("  Coef:", round(coef(L32_high)["n_drafted_male"], 4),
      "  SE:", round(se(L32_high)["n_drafted_male"], 4),
      "  pval:", round(pvalue(L32_high)["n_drafted_male"], 4), "\n")
  if (inherits(L32_high, "fixest")) {
    pct_high <- round(100*(exp(coef(L32_high)["n_drafted_male"]) - 1), 1)
    cat("  Percent change:", pct_high, "%\n")
  }
  cat("  N:", nobs(L32_high), "\n")
}

# Descriptive
cat("\n  Descriptive: female new hires by rank\n")
panel_ka %>%
  mutate(rank_group = if_else(pos_rank <= 2, "Low (1-2)", "High (3)")) %>%
  group_by(rank_group) %>%
  summarise(
    total_female_hires = sum(n_new_hires_female),
    total_male_hires   = sum(n_new_hires_male),
    mean_female_hires  = round(mean(n_new_hires_female), 3),
    n_obs = n(),
    .groups = "drop"
  ) %>% print()
cat("\n")

# ------------------------------------------------------------------------------
# LINE 33: "I do not find any differences in exposure to more females relative
#           to men in the impacted workplaces."
# Regression: OLS(female_share_all ~ any_drafted | FE)
# From Table2_GenderComposition.R
# ------------------------------------------------------------------------------

cat("--- LINE 33: Female share among all workers ~ drafting ---\n")

# Exclude kanri positions (as in Table2)
kanri_pattern <- "^主事$|^技師$|^視学$|^理事$"

panel_t2 <- staff_transitions %>%
  filter(!str_detect(pos_norm, kanri_pattern)) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total = n(),
    n_female = sum(is_female, na.rm = TRUE),
    female_share_all = n_female / n_total,
    .groups = "drop"
  ) %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm", "year_num")) %>%
  left_join(
    df_all %>%
      filter(year_num %in% years_of_interest, !str_detect(pos_norm, kanri_pattern)) %>%
      group_by(office_id, pos_norm, year_num) %>%
      summarise(
        n_male = sum(!is_female, na.rm = TRUE),
        n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("office_id", "pos_norm", "year_num")
  ) %>%
  left_join(
    map_dfr(years_of_interest, function(yr) {
      df %>%
        filter(!str_detect(pos_norm, kanri_pattern), year_num < yr, !is_female) %>%
        group_by(office_id, pos_norm) %>%
        summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
        mutate(year_num = yr)
    }),
    by = c("office_id", "pos_norm", "year_num")
  ) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    any_drafted = as.integer(n_drafted_male > 0),
    draft_share = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

L33 <- feols(female_share_all ~ any_drafted + cumul_n_male |
               year_num + ka_id + pos_norm,
             data = panel_t2 %>% filter(!is.na(ka_id)), cluster = ~office_id)
cat("Coef (any_drafted):", round(coef(L33)["any_drafted"], 4),
    "  SE:", round(se(L33)["any_drafted"], 4),
    "  pval:", round(pvalue(L33)["any_drafted"], 4), "\n")

L33b <- feols(female_share_all ~ draft_share + cumul_n_male |
                year_num + ka_id + pos_norm,
              data = panel_t2 %>% filter(!is.na(ka_id)), cluster = ~office_id)
cat("Coef (draft_share):", round(coef(L33b)["draft_share"], 4),
    "  SE:", round(se(L33b)["draft_share"], 4),
    "  pval:", round(pvalue(L33b)["draft_share"], 4), "\n")
cat("N:", nobs(L33), "\n")

cat("\n  Descriptive: mean female_share_all =", round(mean(panel_t2$female_share_all, na.rm=TRUE), 4), "\n")
cat("  Mean female_share_all (drafted):",
    round(mean(panel_t2$female_share_all[panel_t2$any_drafted == 1], na.rm=TRUE), 4), "\n")
cat("  Mean female_share_all (non-drafted):",
    round(mean(panel_t2$female_share_all[panel_t2$any_drafted == 0], na.rm=TRUE), 4), "\n\n")


# ==============================================================================
# ██████████████████████████████████████████████████████████████████████████████
# RESULT SECTION 3: PROMOTION AND RETENTION (Lines 35-38)
# ██████████████████████████████████████████████████████████████████████████████
# ==============================================================================

cat("================================================================\n")
cat("RESULT SECTION 3: PROMOTION AND RETENTION\n")
cat("================================================================\n\n")

# ------------------------------------------------------------------------------
# LINES 35-36: "military drafting accelerated the promotion of pre-existing staff"
#              "Workers from impacted offices were assigned to positions [] ranks
#               higher on average than those in adjacent offices."
# NEW REGRESSION: rank outcome for non-drafted 1944 workers
# DV: rank in postwar years (or max rank attained)
# Treatment: draft_share at their 1944 position
# ------------------------------------------------------------------------------

cat("--- LINES 35-36: Promotion / Rank outcome ---\n")

# Non-drafted workers in 1944
workers_1944 <- df %>%
  filter(year_num == 1944) %>%
  filter(drafted != TRUE | is.na(drafted)) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office_id_1944 = office_id, pos_norm_1944 = pos_norm,
         kyoku_1944 = kyoku, ka_1944 = ka, is_female, rank_1944 = rank)

# Draft exposure at their 1944 position
pos_draft_1944 <- df_all %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    n_male_1944 = sum(!is_female, na.rm = TRUE),
    n_drafted_1944 = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share = ifelse(n_male_1944 > 0, n_drafted_1944 / n_male_1944, 0),
    .groups = "drop"
  )

workers_1944 <- workers_1944 %>%
  left_join(pos_draft_1944, by = c("office_id_1944" = "office_id",
                                    "pos_norm_1944" = "pos_norm")) %>%
  mutate(
    any_drafted = as.integer(n_drafted_1944 > 0),
    draft_share = replace_na(draft_share, 0),
    n_drafted_1944 = replace_na(n_drafted_1944, 0),
    ka_id_1944 = if_else(!is.na(ka_1944) & !is.na(kyoku_1944),
                          paste(kyoku_1944, ka_1944, sep = "_"), NA_character_)
  )

# Track their rank in postwar years
postwar_rank <- df %>%
  filter(year_num %in% 1946:1953) %>%
  select(staff_id, year_num, rank_postwar = rank, pos_norm_postwar = pos_norm)

# Average postwar rank
avg_postwar_rank <- postwar_rank %>%
  group_by(staff_id) %>%
  summarise(
    avg_rank_postwar = mean(rank_postwar, na.rm = TRUE),
    max_rank_postwar = max(rank_postwar, na.rm = TRUE),
    n_postwar_years  = n_distinct(year_num),
    .groups = "drop"
  )

promo <- workers_1944 %>%
  left_join(avg_postwar_rank, by = "staff_id") %>%
  filter(!is.na(avg_rank_postwar))  # must appear postwar

# Rank change
promo <- promo %>%
  mutate(rank_change = avg_rank_postwar - rank_1944)

cat("Sample: non-drafted 1944 workers appearing postwar:", nrow(promo), "\n")
cat("Mean rank_1944:", round(mean(promo$rank_1944), 3), "\n")
cat("Mean avg_rank_postwar:", round(mean(promo$avg_rank_postwar), 3), "\n")
cat("Mean rank_change:", round(mean(promo$rank_change), 3), "\n\n")

# Regression: rank_change ~ draft_share | ka + pos FE
promo_ka <- promo %>% filter(!is.na(ka_id_1944))

L35a <- feols(rank_change ~ any_drafted | ka_id_1944 + pos_norm_1944,
              data = promo_ka, cluster = ~office_id_1944)
L35b <- feols(rank_change ~ draft_share | ka_id_1944 + pos_norm_1944,
              data = promo_ka, cluster = ~office_id_1944)
L35c <- feols(avg_rank_postwar ~ draft_share | ka_id_1944 + pos_norm_1944,
              data = promo_ka, cluster = ~office_id_1944)

cat("Ka FE: rank_change ~ any_drafted\n")
cat("  Coef:", round(coef(L35a)["any_drafted"], 4),
    "  SE:", round(se(L35a)["any_drafted"], 4),
    "  pval:", round(pvalue(L35a)["any_drafted"], 4),
    "  N:", nobs(L35a), "\n")

cat("Ka FE: rank_change ~ draft_share\n")
cat("  Coef:", round(coef(L35b)["draft_share"], 4),
    "  SE:", round(se(L35b)["draft_share"], 4),
    "  pval:", round(pvalue(L35b)["draft_share"], 4),
    "  N:", nobs(L35b), "\n")

cat("Ka FE: avg_rank_postwar ~ draft_share (controlling for 1944 rank via pos FE)\n")
cat("  Coef:", round(coef(L35c)["draft_share"], 4),
    "  SE:", round(se(L35c)["draft_share"], 4),
    "  pval:", round(pvalue(L35c)["draft_share"], 4),
    "  N:", nobs(L35c), "\n")

# Also Kyoku FE
promo_ky <- promo %>% filter(!is.na(kyoku_1944))

L35d <- feols(rank_change ~ draft_share | kyoku_1944 + pos_norm_1944,
              data = promo_ky, cluster = ~office_id_1944)
cat("\nKyoku FE: rank_change ~ draft_share\n")
cat("  Coef:", round(coef(L35d)["draft_share"], 4),
    "  SE:", round(se(L35d)["draft_share"], 4),
    "  pval:", round(pvalue(L35d)["draft_share"], 4),
    "  N:", nobs(L35d), "\n")

# Descriptive: rank change by treatment
cat("\n  Descriptive: rank change by draft exposure\n")
promo %>%
  mutate(group = if_else(any_drafted == 1, "Drafted position", "Non-drafted position")) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_rank_1944 = round(mean(rank_1944), 3),
    mean_rank_postwar = round(mean(avg_rank_postwar), 3),
    mean_rank_change = round(mean(rank_change), 3),
    .groups = "drop"
  ) %>% print()

# By gender
cat("\n  Descriptive: rank change by gender\n")
promo %>%
  group_by(is_female) %>%
  summarise(
    n = n(),
    mean_rank_1944 = round(mean(rank_1944), 3),
    mean_rank_postwar = round(mean(avg_rank_postwar), 3),
    mean_rank_change = round(mean(rank_change), 3),
    .groups = "drop"
  ) %>% print()
cat("\n")

# With gender interaction
L35e <- feols(rank_change ~ draft_share * is_female | ka_id_1944 + pos_norm_1944,
              data = promo_ka, cluster = ~office_id_1944)
cat("Ka FE: rank_change ~ draft_share * is_female\n")
etable(L35e, se.below = TRUE, fitstat = ~n + r2)
cat("\n")

# ------------------------------------------------------------------------------
# LINE 37: "I do not find any effects on retention between workers across
#           drafted and non-drafted offices."
# Regression: OLS(tenure_postwar ~ draft_share | ka + pos FE)
# From Table3_Retention.R
# ------------------------------------------------------------------------------

cat("--- LINE 37: Retention (postwar tenure) ---\n")

postwar_tenure <- df %>%
  filter(year_num %in% 1946:1953) %>%
  group_by(staff_id) %>%
  summarise(tenure_postwar = n_distinct(year_num), .groups = "drop")

retention <- workers_1944 %>%
  left_join(postwar_tenure, by = "staff_id") %>%
  mutate(tenure_postwar = replace_na(tenure_postwar, 0))

retention_ka <- retention %>% filter(!is.na(ka_id_1944))

L37a <- feols(tenure_postwar ~ any_drafted | ka_id_1944 + pos_norm_1944,
              data = retention_ka, cluster = ~office_id_1944)
L37b <- feols(tenure_postwar ~ draft_share | ka_id_1944 + pos_norm_1944,
              data = retention_ka, cluster = ~office_id_1944)

cat("Ka FE: tenure_postwar ~ any_drafted\n")
cat("  Coef:", round(coef(L37a)["any_drafted"], 4),
    "  SE:", round(se(L37a)["any_drafted"], 4),
    "  pval:", round(pvalue(L37a)["any_drafted"], 4),
    "  N:", nobs(L37a), "\n")

cat("Ka FE: tenure_postwar ~ draft_share\n")
cat("  Coef:", round(coef(L37b)["draft_share"], 4),
    "  SE:", round(se(L37b)["draft_share"], 4),
    "  pval:", round(pvalue(L37b)["draft_share"], 4),
    "  N:", nobs(L37b), "\n")

# Descriptive
cat("\n  Descriptive: mean postwar tenure =", round(mean(retention$tenure_postwar), 3), "\n")
cat("  Mean tenure (drafted pos):",
    round(mean(retention$tenure_postwar[retention$any_drafted == 1]), 3), "\n")
cat("  Mean tenure (non-drafted pos):",
    round(mean(retention$tenure_postwar[retention$any_drafted == 0]), 3), "\n")
cat("  Pct appearing postwar:", round(100*mean(retention$tenure_postwar > 0), 1), "%\n\n")

# ------------------------------------------------------------------------------
# LINE 38: "I do not find any impact on the office composition in future
#           assignments between workers from non-drafted and drafted offices."
# Regression: OLS(avg_female_share_dest ~ draft_share | ka + pos FE)
# From Table3_Retention.R
# ------------------------------------------------------------------------------

cat("--- LINE 38: Destination office gender composition ---\n")

office_female_share <- df %>%
  filter(year_num %in% 1946:1953) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total = n(), n_female = sum(is_female, na.rm = TRUE),
    female_share = n_female / n_total, .groups = "drop"
  )

worker_postwar <- df %>%
  filter(year_num %in% 1946:1953) %>%
  select(staff_id, office_id, pos_norm, year_num) %>%
  left_join(office_female_share, by = c("office_id", "pos_norm", "year_num"))

avg_dest <- worker_postwar %>%
  group_by(staff_id) %>%
  summarise(avg_female_share_dest = mean(female_share, na.rm = TRUE), .groups = "drop")

dest <- workers_1944 %>%
  left_join(avg_dest, by = "staff_id") %>%
  filter(!is.na(avg_female_share_dest))

dest_ka <- dest %>% filter(!is.na(ka_id_1944))

L38a <- feols(avg_female_share_dest ~ any_drafted | ka_id_1944 + pos_norm_1944,
              data = dest_ka, cluster = ~office_id_1944)
L38b <- feols(avg_female_share_dest ~ draft_share | ka_id_1944 + pos_norm_1944,
              data = dest_ka, cluster = ~office_id_1944)

cat("Ka FE: avg_female_share_dest ~ any_drafted\n")
cat("  Coef:", round(coef(L38a)["any_drafted"], 4),
    "  SE:", round(se(L38a)["any_drafted"], 4),
    "  pval:", round(pvalue(L38a)["any_drafted"], 4),
    "  N:", nobs(L38a), "\n")

cat("Ka FE: avg_female_share_dest ~ draft_share\n")
cat("  Coef:", round(coef(L38b)["draft_share"], 4),
    "  SE:", round(se(L38b)["draft_share"], 4),
    "  pval:", round(pvalue(L38b)["draft_share"], 4),
    "  N:", nobs(L38b), "\n")

cat("\n  Descriptive: mean avg_female_share_dest =",
    round(mean(dest$avg_female_share_dest, na.rm=TRUE), 4), "\n\n")


# ==============================================================================
# ██████████████████████████████████████████████████████████████████████████████
# DESCRIPTIVE STATISTICS SUMMARY
# ██████████████████████████████████████████████████████████████████████████████
# ==============================================================================

cat("================================================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("================================================================\n\n")

cat("--- Worker counts by year (wartime) ---\n")
df %>%
  filter(year_num %in% 1937:1945) %>%
  group_by(year_num) %>%
  summarise(
    n_workers = n_distinct(staff_id),
    n_female  = n_distinct(staff_id[is_female]),
    n_male    = n_distinct(staff_id[!is_female]),
    pct_female = round(100 * n_female / n_workers, 1),
    .groups = "drop"
  ) %>% print()

cat("\n--- Draft counts by year ---\n")
df_all %>%
  filter(year_num %in% 1938:1945, drafted == TRUE) %>%
  group_by(year_num) %>%
  summarise(n_drafted = n_distinct(staff_id), .groups = "drop") %>%
  print()

cat("\n--- Position rank distribution (wartime, 1938-1945) ---\n")
df %>%
  filter(year_num %in% 1938:1945) %>%
  count(rank, name = "n_person_years") %>%
  mutate(pct = round(100 * n_person_years / sum(n_person_years), 1)) %>%
  print()

cat("\n--- Position rank distribution by gender (wartime) ---\n")
df %>%
  filter(year_num %in% 1938:1945) %>%
  count(rank, is_female) %>%
  pivot_wider(names_from = is_female, values_from = n, names_prefix = "female_") %>%
  print()

cat("\n--- New hires per position-year (wartime panel) ---\n")
cat("Mean:", round(mean(panel$n_new_hires), 3), "\n")
cat("Median:", median(panel$n_new_hires), "\n")
cat("SD:", round(sd(panel$n_new_hires), 3), "\n")
cat("Mean (drafted positions):", round(mean(panel$n_new_hires[panel$n_drafted_male > 0]), 3), "\n")
cat("Mean (non-drafted):", round(mean(panel$n_new_hires[panel$n_drafted_male == 0]), 3), "\n")

cat("\n--- Transfers in per position-year ---\n")
cat("Mean:", round(mean(panel$n_transfers_in), 3), "\n")
cat("Pct with any transfer:", round(100*mean(panel$n_transfers_in > 0), 1), "%\n")

cat("\n--- Office structure ---\n")
cat("Unique offices (office_id):", n_distinct(df$office_id[df$year_num %in% 1938:1945]), "\n")
cat("Unique kyoku:", n_distinct(df$kyoku[df$year_num %in% 1938:1945], na.rm = TRUE), "\n")
cat("Unique ka:", n_distinct(paste(df$kyoku, df$ka)[df$year_num %in% 1938:1945 & !is.na(df$ka)]), "\n")
cat("Unique positions:", n_distinct(df$pos_norm[df$year_num %in% 1938:1945]), "\n")

cat("\n\n===== SCRIPT COMPLETE =====\n")
