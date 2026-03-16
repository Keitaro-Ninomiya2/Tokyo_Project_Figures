################################################################################
# Table 3: Postwar Outcomes of 1944 Colleagues
# Unit: individual (non-drafted worker observed in 1944)
# Treatment: draft_share at their position in 1944
# Outcome A: tenure_postwar (count of years appearing in 1946-1953)
# Outcome B: avg_female_share_dest (mean female share of worker's office 1946-1953)
# FE: Panel A = ka + pos_norm, Panel B = kyoku + pos_norm
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

# ============================================================
# 1. IDENTIFY 1944 WORKERS AND THEIR DRAFT EXPOSURE
# ============================================================

workers_1944 <- df %>%
  filter(year_num == 1944)

# Draft intensity at position level in 1944
pos_draft_1944 <- df_all %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    n_male_1944    = sum(!is_female, na.rm = TRUE),
    n_drafted_1944 = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share    = ifelse(n_male_1944 > 0, n_drafted_1944 / n_male_1944, 0),
    .groups = "drop"
  )

# ============================================================
# 2. POSTWAR OUTCOMES
# ============================================================

postwar_years <- 1946:1953

# --- Outcome A: Tenure (count of postwar years appearing) ---
postwar_tenure <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(tenure_postwar = n_distinct(year_num), .groups = "drop")

# --- Outcome B: Average female share of destination office ---
# Step 1: Compute female share at (office_id, pos_norm, year) level for postwar
office_female_share <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(
    n_total  = n(),
    n_female = sum(is_female, na.rm = TRUE),
    female_share = n_female / n_total,
    .groups = "drop"
  )

# Step 2: For each worker × postwar year, get the female share of their office-position
worker_postwar <- df %>%
  filter(year_num %in% postwar_years) %>%
  select(staff_id, office_id, pos_norm, year_num) %>%
  left_join(office_female_share, by = c("office_id", "pos_norm", "year_num"))

# Step 3: Average across all postwar years for each worker
avg_dest_female_share <- worker_postwar %>%
  group_by(staff_id) %>%
  summarise(avg_female_share_dest = mean(female_share, na.rm = TRUE), .groups = "drop")

# ============================================================
# 3. ASSEMBLE INDIVIDUAL CROSS-SECTION
# ============================================================

indiv <- workers_1944 %>%
  filter(drafted != TRUE | is.na(drafted)) %>%
  select(staff_id, office_id, kyoku, ka, pos_norm, is_female) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  left_join(pos_draft_1944, by = c("office_id", "pos_norm")) %>%
  left_join(postwar_tenure, by = "staff_id") %>%
  left_join(avg_dest_female_share, by = "staff_id") %>%
  mutate(
    any_drafted    = as.integer(n_drafted_1944 > 0),
    draft_share    = replace_na(draft_share, 0),
    n_drafted_1944 = replace_na(n_drafted_1944, 0),
    tenure_postwar = replace_na(tenure_postwar, 0),
    ka_id          = if_else(!is.na(ka) & !is.na(kyoku),
                             paste(kyoku, ka, sep = "_"), NA_character_)
  )

cat("\n===== SAMPLE DIAGNOSTICS =====\n")
cat("Non-drafted workers in 1944:", nrow(indiv), "\n")
cat("Appear in postwar:", sum(indiv$tenure_postwar > 0), "(",
    round(100 * mean(indiv$tenure_postwar > 0), 1), "%)\n")
cat("Mean tenure_postwar:", round(mean(indiv$tenure_postwar), 2), "\n")
cat("Mean tenure_postwar (if >0):",
    round(mean(indiv$tenure_postwar[indiv$tenure_postwar > 0]), 2), "\n")
cat("Has avg_female_share_dest:", sum(!is.na(indiv$avg_female_share_dest)), "\n")
cat("Mean avg_female_share_dest:",
    round(mean(indiv$avg_female_share_dest, na.rm = TRUE), 4), "\n")
cat("Female workers:", sum(indiv$is_female), "\n")
cat("Male workers:", sum(!indiv$is_female), "\n")
cat("Mean draft_share:", round(mean(indiv$draft_share), 4), "\n")

cat("\nTenure by treatment:\n")
indiv %>%
  group_by(any_drafted) %>%
  summarise(
    n = n(),
    mean_tenure = round(mean(tenure_postwar), 2),
    pct_appear  = round(100 * mean(tenure_postwar > 0), 1),
    .groups = "drop"
  ) %>% print()

cat("\nTenure by gender:\n")
indiv %>%
  group_by(is_female) %>%
  summarise(
    n = n(),
    mean_tenure = round(mean(tenure_postwar), 2),
    pct_appear  = round(100 * mean(tenure_postwar > 0), 1),
    .groups = "drop"
  ) %>% print()

# ============================================================
# 4A. REGRESSIONS: TENURE
# ============================================================

# --- Panel A: Ka FE ---
indiv_ka <- indiv %>% filter(!is.na(ka_id))

cat("\n===== Table 3A: Tenure — Panel A (Ka FE) =====\n")
cat("Obs:", nrow(indiv_ka), "\n")

t1a <- feols(tenure_postwar ~ any_drafted | ka_id + pos_norm,
             data = indiv_ka, cluster = ~office_id)
t2a <- feols(tenure_postwar ~ draft_share | ka_id + pos_norm,
             data = indiv_ka, cluster = ~office_id)
t3a <- feols(tenure_postwar ~ draft_share * is_female | ka_id + pos_norm,
             data = indiv_ka, cluster = ~office_id)
t4a <- feols(tenure_postwar ~ draft_share | ka_id + pos_norm,
             data = indiv_ka %>% filter(is_female), cluster = ~office_id)
t5a <- feols(tenure_postwar ~ draft_share | ka_id + pos_norm,
             data = indiv_ka %>% filter(!is_female), cluster = ~office_id)

etable(t1a, t2a, t3a, t4a, t5a,
       headers = c("All (Ext.)", "All (Int.)", "All (Interact.)", "Female", "Male"),
       se.below = TRUE, fitstat = ~n + r2)

# --- Panel B: Kyoku FE ---
indiv_ky <- indiv %>% filter(!is.na(kyoku))

cat("\n===== Table 3A: Tenure — Panel B (Kyoku FE) =====\n")

t1b <- feols(tenure_postwar ~ any_drafted | kyoku + pos_norm,
             data = indiv_ky, cluster = ~office_id)
t2b <- feols(tenure_postwar ~ draft_share | kyoku + pos_norm,
             data = indiv_ky, cluster = ~office_id)
t3b <- feols(tenure_postwar ~ draft_share * is_female | kyoku + pos_norm,
             data = indiv_ky, cluster = ~office_id)
t4b <- feols(tenure_postwar ~ draft_share | kyoku + pos_norm,
             data = indiv_ky %>% filter(is_female), cluster = ~office_id)
t5b <- feols(tenure_postwar ~ draft_share | kyoku + pos_norm,
             data = indiv_ky %>% filter(!is_female), cluster = ~office_id)

etable(t1b, t2b, t3b, t4b, t5b,
       headers = c("All (Ext.)", "All (Int.)", "All (Interact.)", "Female", "Male"),
       se.below = TRUE, fitstat = ~n + r2)

# ============================================================
# 4B. REGRESSIONS: DESTINATION FEMALE SHARE
# ============================================================

# Restrict to workers who appear postwar (avg_female_share_dest is defined)
indiv_ka_dest <- indiv_ka %>% filter(!is.na(avg_female_share_dest))
indiv_ky_dest <- indiv_ky %>% filter(!is.na(avg_female_share_dest))

cat("\n===== Table 3B: Destination Female Share — Panel A (Ka FE) =====\n")
cat("Obs:", nrow(indiv_ka_dest), "\n")
cat("Female obs:", sum(indiv_ka_dest$is_female), "\n")
cat("Male obs:", sum(!indiv_ka_dest$is_female), "\n")

# Note: Female-only subsample has only 6 obs with ka FE → all singletons.
# We show: All (ext), All (int), All (interaction), Male only.
d1a <- feols(avg_female_share_dest ~ any_drafted | ka_id + pos_norm,
             data = indiv_ka_dest, cluster = ~office_id)
d2a <- feols(avg_female_share_dest ~ draft_share | ka_id + pos_norm,
             data = indiv_ka_dest, cluster = ~office_id)
d3a <- feols(avg_female_share_dest ~ draft_share * is_female | ka_id + pos_norm,
             data = indiv_ka_dest, cluster = ~office_id)
d5a <- feols(avg_female_share_dest ~ draft_share | ka_id + pos_norm,
             data = indiv_ka_dest %>% filter(!is_female), cluster = ~office_id)

etable(d1a, d2a, d3a, d5a,
       headers = c("All (Ext.)", "All (Int.)", "All (Interact.)", "Male"),
       se.below = TRUE, fitstat = ~n + r2)

cat("\n===== Table 3B: Destination Female Share — Panel B (Kyoku FE) =====\n")

d1b <- feols(avg_female_share_dest ~ any_drafted | kyoku + pos_norm,
             data = indiv_ky_dest, cluster = ~office_id)
d2b <- feols(avg_female_share_dest ~ draft_share | kyoku + pos_norm,
             data = indiv_ky_dest, cluster = ~office_id)
d3b <- feols(avg_female_share_dest ~ draft_share * is_female | kyoku + pos_norm,
             data = indiv_ky_dest, cluster = ~office_id)
d4b <- tryCatch(
  feols(avg_female_share_dest ~ draft_share | kyoku + pos_norm,
        data = indiv_ky_dest %>% filter(is_female), cluster = ~office_id),
  error = function(e) { cat("  Female-only kyoku FE failed:", e$message, "\n"); NULL }
)
d5b <- feols(avg_female_share_dest ~ draft_share | kyoku + pos_norm,
             data = indiv_ky_dest %>% filter(!is_female), cluster = ~office_id)

if (!is.null(d4b)) {
  etable(d1b, d2b, d3b, d4b, d5b,
         headers = c("All (Ext.)", "All (Int.)", "All (Interact.)", "Female", "Male"),
         se.below = TRUE, fitstat = ~n + r2)
} else {
  etable(d1b, d2b, d3b, d5b,
         headers = c("All (Ext.)", "All (Int.)", "All (Interact.)", "Male"),
         se.below = TRUE, fitstat = ~n + r2)
}

# ============================================================
# 5. EXPORT LaTeX
# ============================================================

dict_t3 <- c(
  any_drafted  = "Any drafted (=1)",
  draft_share  = "Draft share",
  is_femaleTRUE = "Female (=1)",
  "draft_share:is_femaleTRUE" = "Draft share $\\times$ Female"
)

# --- Table 3A: Tenure ---
tex_t_a <- etable(
  t1a, t2a, t3a, t4a, t5a,
  dict = dict_t3,
  headers = c("All", "All", "All", "Female", "Male"),
  se.below = TRUE, fitstat = ~n + r2,
  tex = TRUE
)

tex_t_b <- etable(
  t1b, t2b, t3b, t4b, t5b,
  dict = dict_t3,
  headers = c("All", "All", "All", "Female", "Male"),
  se.below = TRUE, fitstat = ~n + r2,
  tex = TRUE
)

tex_out_tenure <- c(
  "% Table 3A: Postwar Tenure of 1944 Colleagues (OLS)",
  "% Dep. var.: Number of postwar years (1946-1953) appearing in civil service.",
  "% Sample: Non-drafted workers observed in 1944.",
  "% Panel A: Ka (Office) Fixed Effects",
  tex_t_a,
  "",
  "% Panel B: Kyoku (Department) Fixed Effects",
  tex_t_b
)

writeLines(tex_out_tenure, here("Table3A_Tenure.tex"))
cat("\nTable 3A exported to Table3A_Tenure.tex\n")

# --- Table 3B: Destination Female Share ---
# Ka panel: no female-only column (too few obs, all singletons)
tex_d_a <- etable(
  d1a, d2a, d3a, d5a,
  dict = dict_t3,
  headers = c("All", "All", "All", "Male"),
  se.below = TRUE, fitstat = ~n + r2,
  tex = TRUE
)

# Kyoku panel: include female if available
if (!is.null(d4b)) {
  tex_d_b <- etable(
    d1b, d2b, d3b, d4b, d5b,
    dict = dict_t3,
    headers = c("All", "All", "All", "Female", "Male"),
    se.below = TRUE, fitstat = ~n + r2,
    tex = TRUE
  )
} else {
  tex_d_b <- etable(
    d1b, d2b, d3b, d5b,
    dict = dict_t3,
    headers = c("All", "All", "All", "Male"),
    se.below = TRUE, fitstat = ~n + r2,
    tex = TRUE
  )
}

tex_out_dest <- c(
  "% Table 3B: Destination Office Gender Balance (OLS)",
  "% Dep. var.: Avg. female share of worker's office-position across 1946-1953.",
  "% Sample: Non-drafted workers observed in 1944 who appear postwar.",
  "% Panel A: Ka (Office) Fixed Effects",
  tex_d_a,
  "",
  "% Panel B: Kyoku (Department) Fixed Effects",
  tex_d_b
)

writeLines(tex_out_dest, here("Table3B_DestGenderBalance.tex"))
cat("Table 3B exported to Table3B_DestGenderBalance.tex\n")
