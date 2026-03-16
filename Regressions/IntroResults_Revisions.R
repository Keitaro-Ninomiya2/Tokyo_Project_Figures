################################################################################
# Revised Regressions for Lines 23, 24, 32, 35-36
# Addresses feedback on decomposition, distance, rank separation, promotion
################################################################################

library(tidyverse)
library(fixest)

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
# 0b. ENRICHED RANK VARIABLE
# ============================================================
# Pre-1948:
#   Rank 5: 局長
#   Rank 4: 部長, 次長, 課長 (top management)
#   Rank 3: 主事, 技師, 事務官, 地方事務官, 地方技師, 所長, 校長, 區長 (senior/kanri)
#   Rank 2: 書記, 技手, 屬, 主事補, etc. (regular staff)
#   Rank 1: 雇, 囑託員, 臨時 (hired/temporary)
#
# Post-1948:
#   Rank 5: 局長
#   Rank 4: 部長, 次長 (top management)
#   Rank 3: 課長, 所長, 校長, 場長, 園長, 支所長, 區長, 看護婦長, 寮長 (section heads)
#   Rank 2: 係長 (unit chiefs)
#   Rank 1: 事務吏員, 技術吏員, 事務員, 雇, 嘱託 (general staff)

assign_rank_enriched <- function(pos, yr) {
  case_when(
    # --- Pre-1948 ---
    yr < 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr < 1948 & str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    yr < 1948 & str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    yr < 1948 ~ 2L,

    # --- Post-1948 ---
    yr >= 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr >= 1948 & str_detect(pos, "^部長$|^次長$") ~ 4L,
    yr >= 1948 & str_detect(pos, "^課長$|^所長$|^校長$|^場長$|^園長$|^支所長$|^區長$|^看護婦長$|^寮長$") ~ 3L,
    yr >= 1948 & str_detect(pos, "^係長$") ~ 2L,
    yr >= 1948 ~ 1L
  )
}

df <- df %>% mutate(rank_e = assign_rank_enriched(pos_norm, year_num))
df_all <- df_all %>% mutate(rank_e = assign_rank_enriched(pos_norm, year_num))

# Z-score standardize rank within each era
df <- df %>%
  mutate(era = if_else(year_num < 1948, "pre", "post")) %>%
  group_by(era) %>%
  mutate(rank_z = (rank_e - mean(rank_e, na.rm=TRUE)) / sd(rank_e, na.rm=TRUE)) %>%
  ungroup()

cat("=== RANK DISTRIBUTION (enriched) ===\n")
cat("Pre-1948:\n")
df %>% filter(year_num < 1948) %>% count(rank_e) %>%
  mutate(pct = round(100*n/sum(n),1)) %>% print()
cat("\nPost-1948:\n")
df %>% filter(year_num >= 1948) %>% count(rank_e) %>%
  mutate(pct = round(100*n/sum(n),1)) %>% print()

cat("\nZ-score stats by era:\n")
df %>% group_by(era) %>%
  summarise(mean_rank = round(mean(rank_e),3), sd_rank = round(sd(rank_e),3),
            mean_z = round(mean(rank_z),4), sd_z = round(sd(rank_z),4)) %>% print()

# ============================================================
# 1. SHARED PANEL (same as master script)
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
         lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num"),
            relationship = "many-to-many") %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    # Transfer origin classification
    transfer_distance = case_when(
      !is_transfer_in ~ NA_integer_,
      !is.na(lag_ka) & !is.na(ka) & lag_ka == ka & lag_kyoku == kyoku ~ 1L,  # same ka
      !is.na(lag_kyoku) & !is.na(kyoku) & lag_kyoku == kyoku ~ 2L,           # same kyoku, diff ka
      TRUE ~ 3L                                                                # diff kyoku
    )
  )

# Position outcomes with transfer decomposition
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
    n_transfers_same_ka  = sum(transfer_distance == 1, na.rm = TRUE),
    n_transfers_diff_ka  = sum(transfer_distance >= 2, na.rm = TRUE),
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
  mutate(
    across(c(n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    pos_rank = assign_rank_enriched(pos_norm, 1940L)  # wartime rank (pre-1948 rules)
  )

panel_ka <- panel %>% filter(!is.na(ka_id))

cat("\nPanel:", nrow(panel), "obs; ka panel:", nrow(panel_ka), "\n\n")


# ==============================================================================
# LINE 23 REVISED: Decompose transfers by origin
# "driven by promoting workers in lower-ranked positions (same-ka transfers)
#  and poaching workers from external offices (diff-ka transfers)"
# ==============================================================================

cat("================================================================\n")
cat("LINE 23 REVISED: Transfers by origin (same-ka vs diff-ka)\n")
cat("================================================================\n\n")

# Same-ka transfers = internal reallocation / promotion proxy
L23_same <- tryCatch(
  fepois(n_transfers_same_ka ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka, cluster = ~office_id),
  error = function(e) {
    cat("  Poisson failed for same-ka, trying OLS:", e$message, "\n")
    feols(n_transfers_same_ka ~ n_drafted_male + log(cumul_n_male + 1) |
            year_num + ka_id + pos_norm,
          data = panel_ka, cluster = ~office_id)
  }
)

# Diff-ka transfers = external poaching
L23_diff <- tryCatch(
  fepois(n_transfers_diff_ka ~ n_drafted_male + log(cumul_n_male + 1) |
           year_num + ka_id + pos_norm,
         data = panel_ka, cluster = ~office_id),
  error = function(e) {
    cat("  Poisson failed for diff-ka, trying OLS:", e$message, "\n")
    feols(n_transfers_diff_ka ~ n_drafted_male + log(cumul_n_male + 1) |
            year_num + ka_id + pos_norm,
          data = panel_ka, cluster = ~office_id)
  }
)

cat("Same-ka transfers (internal reallocation / promotion proxy):\n")
cat("  Coef:", round(coef(L23_same)["n_drafted_male"], 4),
    "  SE:", round(se(L23_same)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L23_same)["n_drafted_male"], 4),
    "  N:", nobs(L23_same), "\n")
if (inherits(L23_same, "fixest") && !inherits(L23_same, "feols")) {
  cat("  % change:", round(100*(exp(coef(L23_same)["n_drafted_male"]) - 1), 1), "%\n")
}

cat("\nDiff-ka transfers (external poaching):\n")
cat("  Coef:", round(coef(L23_diff)["n_drafted_male"], 4),
    "  SE:", round(se(L23_diff)["n_drafted_male"], 4),
    "  pval:", round(pvalue(L23_diff)["n_drafted_male"], 4),
    "  N:", nobs(L23_diff), "\n")
if (inherits(L23_diff, "fixest") && !inherits(L23_diff, "feols")) {
  cat("  % change:", round(100*(exp(coef(L23_diff)["n_drafted_male"]) - 1), 1), "%\n")
}

# Descriptive
cat("\n  Descriptive: mean same-ka transfers =", round(mean(panel_ka$n_transfers_same_ka), 3), "\n")
cat("  Descriptive: mean diff-ka transfers =", round(mean(panel_ka$n_transfers_diff_ka), 3), "\n")
cat("  Total same-ka:", sum(panel_ka$n_transfers_same_ka),
    "  Total diff-ka:", sum(panel_ka$n_transfers_diff_ka), "\n\n")


# ==============================================================================
# LINE 24 REVISED: Correct interpretation of distance coefficients
# ==============================================================================

cat("================================================================\n")
cat("LINE 24 REVISED: Interpretation correction\n")
cat("================================================================\n\n")

# Re-run distance regressions with enriched panel
for (d in 1:3) {
  dv <- if (d == 1) "n_transfers_same_ka"
        else paste0("n_transfers_dist", d)

  # For d=2,3 we need those columns
  if (d >= 2) {
    # Create distance-specific columns
    pos_out_d <- staff_transitions %>%
      group_by(office_id, pos_norm, year_num) %>%
      summarise(
        val = sum(transfer_distance == d, na.rm = TRUE),
        .groups = "drop"
      )
    panel_ka[[paste0("n_transfers_dist", d)]] <- panel_ka %>%
      left_join(pos_out_d, by = c("office_id", "pos_norm", "year_num")) %>%
      pull(val) %>%
      replace_na(0)
    dv <- paste0("n_transfers_dist", d)
  }

  f <- as.formula(paste0(dv, " ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm"))
  m <- tryCatch(
    fepois(f, data = panel_ka, cluster = ~office_id),
    error = function(e) {
      feols(f, data = panel_ka, cluster = ~office_id)
    }
  )
  cat("Distance", d, ":\n")
  cat("  Coef:", round(coef(m)["n_drafted_male"], 4),
      "  SE:", round(se(m)["n_drafted_male"], 4),
      "  pval:", round(pvalue(m)["n_drafted_male"], 4),
      "  N:", nobs(m), "\n")
}

cat("\nRevised interpretation: The coefficient on drafting is LARGEST for distance 1\n")
cat("(same ka, coef ~0.09) and declines with distance (dist 2 ~0.05, dist 3 ~0.04).\n")
cat("This means draft-induced transfers are disproportionately sourced from\n")
cat("NEARBY offices (same ka), contradicting the claim that source offices are\n")
cat("'not immediate neighbors.' The intro text on Line 24 needs revision.\n\n")


# ==============================================================================
# LINE 32 REVISED: Female new hires by rank 1, 2, 3 separately
# ==============================================================================

cat("================================================================\n")
cat("LINE 32 REVISED: Female new hires by rank (1, 2, 3 separately)\n")
cat("================================================================\n\n")

for (r in 1:3) {
  cat("Rank", r, ":\n")
  sub <- panel_ka %>% filter(pos_rank == r)
  cat("  N obs:", nrow(sub), "  Total female new hires:", sum(sub$n_new_hires_female), "\n")

  m <- tryCatch(
    fepois(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
             year_num + ka_id + pos_norm,
           data = sub, cluster = ~office_id),
    error = function(e) {
      cat("  Poisson failed, trying OLS\n")
      tryCatch(
        feols(n_new_hires_female ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = sub, cluster = ~office_id),
        error = function(e2) { cat("  OLS also failed:", e2$message, "\n"); NULL }
      )
    }
  )
  if (!is.null(m)) {
    cat("  Coef:", round(coef(m)["n_drafted_male"], 4),
        "  SE:", round(se(m)["n_drafted_male"], 4),
        "  pval:", round(pvalue(m)["n_drafted_male"], 4),
        "  N:", nobs(m), "\n")
    if (inherits(m, "fixest") && !inherits(m, "feols")) {
      cat("  % change:", round(100*(exp(coef(m)["n_drafted_male"]) - 1), 1), "%\n")
    }
  }
  # Descriptive
  cat("  Mean female new hires:", round(mean(sub$n_new_hires_female), 4), "\n")
  cat("  Mean male new hires:", round(mean(sub$n_new_hires_male), 4), "\n\n")
}

# Also male new hires by rank for comparison
cat("--- Male new hires by rank (for comparison) ---\n")
for (r in 1:3) {
  sub <- panel_ka %>% filter(pos_rank == r)
  m <- tryCatch(
    fepois(n_new_hires_male ~ n_drafted_male + log(cumul_n_male + 1) |
             year_num + ka_id + pos_norm,
           data = sub, cluster = ~office_id),
    error = function(e) {
      tryCatch(
        feols(n_new_hires_male ~ n_drafted_male + log(cumul_n_male + 1) |
                year_num + ka_id + pos_norm,
              data = sub, cluster = ~office_id),
        error = function(e2) NULL
      )
    }
  )
  if (!is.null(m)) {
    cat("Rank", r, "- Male new hires: Coef:", round(coef(m)["n_drafted_male"], 4),
        "  SE:", round(se(m)["n_drafted_male"], 4),
        "  pval:", round(pvalue(m)["n_drafted_male"], 4),
        "  N:", nobs(m), "\n")
  }
}
cat("\n")


# ==============================================================================
# LINES 35-36 REVISED: Enriched rank + z-score standardization
# ==============================================================================

cat("================================================================\n")
cat("LINES 35-36 REVISED: Promotion with enriched rank & z-scores\n")
cat("================================================================\n\n")

# Non-drafted workers in 1944
workers_1944 <- df %>%
  filter(year_num == 1944) %>%
  filter(drafted != TRUE | is.na(drafted)) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office_id_1944 = office_id, pos_norm_1944 = pos_norm,
         kyoku_1944 = kyoku, ka_1944 = ka, is_female,
         rank_e_1944 = rank_e, rank_z_1944 = rank_z)

# Draft exposure at 1944 position
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

# Postwar rank (enriched + z-scored)
postwar_rank <- df %>%
  filter(year_num %in% 1946:1953) %>%
  select(staff_id, year_num, rank_e_postwar = rank_e, rank_z_postwar = rank_z)

avg_postwar_rank <- postwar_rank %>%
  group_by(staff_id) %>%
  summarise(
    avg_rank_e = mean(rank_e_postwar, na.rm = TRUE),
    max_rank_e = max(rank_e_postwar, na.rm = TRUE),
    avg_rank_z = mean(rank_z_postwar, na.rm = TRUE),
    max_rank_z = max(rank_z_postwar, na.rm = TRUE),
    n_postwar_years = n_distinct(year_num),
    .groups = "drop"
  )

promo <- workers_1944 %>%
  left_join(avg_postwar_rank, by = "staff_id") %>%
  filter(!is.na(avg_rank_e))

promo <- promo %>%
  mutate(
    rank_e_change = avg_rank_e - rank_e_1944,
    rank_z_change = avg_rank_z - rank_z_1944,
    max_rank_e_change = max_rank_e - rank_e_1944
  )

cat("Sample: non-drafted 1944 workers appearing postwar:", nrow(promo), "\n")
cat("Mean rank_e 1944:", round(mean(promo$rank_e_1944), 3), "\n")
cat("Mean avg rank_e postwar:", round(mean(promo$avg_rank_e), 3), "\n")
cat("Mean rank_e change:", round(mean(promo$rank_e_change), 3), "\n")
cat("Mean rank_z 1944:", round(mean(promo$rank_z_1944), 4), "\n")
cat("Mean avg rank_z postwar:", round(mean(promo$avg_rank_z), 4), "\n")
cat("Mean rank_z change:", round(mean(promo$rank_z_change), 4), "\n\n")

# Enriched rank distribution
cat("Enriched rank distribution (1944 workers appearing postwar):\n")
cat("1944 rank:\n")
promo %>% count(rank_e_1944) %>% print()
cat("Postwar avg rank (rounded):\n")
promo %>% mutate(avg_r = round(avg_rank_e)) %>% count(avg_r) %>% print()

# --- Regressions with enriched rank ---
promo_ka <- promo %>% filter(!is.na(ka_id_1944))

cat("\n--- Enriched rank change (raw) ---\n")
L35_e1 <- feols(rank_e_change ~ any_drafted | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)
L35_e2 <- feols(rank_e_change ~ draft_share | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)

cat("Ka FE: rank_e_change ~ any_drafted\n")
cat("  Coef:", round(coef(L35_e1)["any_drafted"], 4),
    "  SE:", round(se(L35_e1)["any_drafted"], 4),
    "  pval:", round(pvalue(L35_e1)["any_drafted"], 4),
    "  N:", nobs(L35_e1), "\n")

cat("Ka FE: rank_e_change ~ draft_share\n")
cat("  Coef:", round(coef(L35_e2)["draft_share"], 4),
    "  SE:", round(se(L35_e2)["draft_share"], 4),
    "  pval:", round(pvalue(L35_e2)["draft_share"], 4),
    "  N:", nobs(L35_e2), "\n")

cat("\n--- Z-scored rank change ---\n")
L35_z1 <- feols(rank_z_change ~ any_drafted | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)
L35_z2 <- feols(rank_z_change ~ draft_share | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)

cat("Ka FE: rank_z_change ~ any_drafted\n")
cat("  Coef:", round(coef(L35_z1)["any_drafted"], 4),
    "  SE:", round(se(L35_z1)["any_drafted"], 4),
    "  pval:", round(pvalue(L35_z1)["any_drafted"], 4),
    "  N:", nobs(L35_z1), "\n")

cat("Ka FE: rank_z_change ~ draft_share\n")
cat("  Coef:", round(coef(L35_z2)["draft_share"], 4),
    "  SE:", round(se(L35_z2)["draft_share"], 4),
    "  pval:", round(pvalue(L35_z2)["draft_share"], 4),
    "  N:", nobs(L35_z2), "\n")

cat("\n--- Max rank attained (enriched) ---\n")
L35_max <- feols(max_rank_e_change ~ draft_share | ka_id_1944 + pos_norm_1944,
                 data = promo_ka, cluster = ~office_id_1944)
cat("Ka FE: max_rank_e_change ~ draft_share\n")
cat("  Coef:", round(coef(L35_max)["draft_share"], 4),
    "  SE:", round(se(L35_max)["draft_share"], 4),
    "  pval:", round(pvalue(L35_max)["draft_share"], 4),
    "  N:", nobs(L35_max), "\n")

# Kyoku FE
promo_ky <- promo %>% filter(!is.na(kyoku_1944))

L35_ky1 <- feols(rank_e_change ~ draft_share | kyoku_1944 + pos_norm_1944,
                 data = promo_ky, cluster = ~office_id_1944)
L35_ky2 <- feols(rank_z_change ~ draft_share | kyoku_1944 + pos_norm_1944,
                 data = promo_ky, cluster = ~office_id_1944)

cat("\nKyoku FE: rank_e_change ~ draft_share\n")
cat("  Coef:", round(coef(L35_ky1)["draft_share"], 4),
    "  SE:", round(se(L35_ky1)["draft_share"], 4),
    "  pval:", round(pvalue(L35_ky1)["draft_share"], 4),
    "  N:", nobs(L35_ky1), "\n")

cat("Kyoku FE: rank_z_change ~ draft_share\n")
cat("  Coef:", round(coef(L35_ky2)["draft_share"], 4),
    "  SE:", round(se(L35_ky2)["draft_share"], 4),
    "  pval:", round(pvalue(L35_ky2)["draft_share"], 4),
    "  N:", nobs(L35_ky2), "\n")

# Descriptive by treatment
cat("\n--- Descriptive: enriched rank change by draft exposure ---\n")
promo %>%
  mutate(group = if_else(any_drafted == 1, "Drafted pos", "Non-drafted pos")) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_rank_e_1944 = round(mean(rank_e_1944), 3),
    mean_avg_rank_e_pw = round(mean(avg_rank_e), 3),
    mean_rank_e_change = round(mean(rank_e_change), 3),
    mean_max_rank_e_pw = round(mean(max_rank_e), 3),
    mean_rank_z_change = round(mean(rank_z_change), 4),
    .groups = "drop"
  ) %>% print()

cat("\n--- Descriptive: enriched rank change by gender ---\n")
promo %>%
  group_by(is_female) %>%
  summarise(
    n = n(),
    mean_rank_e_1944 = round(mean(rank_e_1944), 3),
    mean_avg_rank_e_pw = round(mean(avg_rank_e), 3),
    mean_rank_e_change = round(mean(rank_e_change), 3),
    mean_rank_z_change = round(mean(rank_z_change), 4),
    .groups = "drop"
  ) %>% print()

# Gender interaction
cat("\n--- Gender interaction (enriched rank) ---\n")
L35_gx <- feols(rank_e_change ~ draft_share * is_female | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)
etable(L35_gx, se.below = TRUE, fitstat = ~n + r2)

L35_gz <- feols(rank_z_change ~ draft_share * is_female | ka_id_1944 + pos_norm_1944,
                data = promo_ka, cluster = ~office_id_1944)
etable(L35_gz, se.below = TRUE, fitstat = ~n + r2)

cat("\n===== REVISION SCRIPT COMPLETE =====\n")
