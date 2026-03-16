################################################################################
# First Stage: 1944 Draft Shock → Female Kakari-cho Appointment (1950–1951)
#
# Unit: kakari (office_id × ka × kakari × year) — 係 level
# Treatment: Draft count at kakari level (from staff crosswalk)
# Outcome: Count of female kakari-cho (係長) per kakari (intensive margin)
#
# Sample: 1950 and 1951. Kyoku corrected via year-specific ground-truth crosswalk.
# Prerequisite: Run Assign_Kyoku_By_Order_1950.R and Assign_Kyoku_By_Order_1951.R
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
    year_num   = as.numeric(year),
    is_female  = gender_modern == "female",
    pos_norm   = str_replace_all(position, "\\s+", ""),
    # 係長: match actual chars or Unicode escape format <U+4FC2><U+9577>
    is_kakacho = str_detect(pos_norm, "\u4fc2\u9577|4FC2.*9577")
  )

# ============================================================
# 1. TREATMENT: Position-level draft share in 1944
# ============================================================

# 1a. Position-level measures in 1944
pos_1944 <- df %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    n_drafted_1944   = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    n_male_1944      = sum(!is_female, na.rm = TRUE),
    draft_share_1944 = ifelse(n_male_1944 > 0, n_drafted_1944 / n_male_1944, 0),
    pos_size_1944    = n(),
    .groups = "drop"
  )

cat("\n--- Treatment diagnostics (position level) ---\n")
cat("Total position cells in 1944:", nrow(pos_1944), "\n")
cat("Position cells with drafting:", sum(pos_1944$n_drafted_1944 > 0), "\n")
cat("Mean draft share (all cells):", round(mean(pos_1944$draft_share_1944), 4), "\n")
cat("Mean draft share (cells with drafts):",
    round(mean(pos_1944$draft_share_1944[pos_1944$n_drafted_1944 > 0]), 4), "\n")

# 1b. Staff-based crosswalk: track 1944 staff → postwar (office_id, pos_norm)
staff_1944_pos <- df %>%
  filter(year_num == 1944) %>%
  select(staff_id, war_office_id = office_id, war_pos = pos_norm) %>%
  distinct()

panel_years <- c(1950, 1951)

# Find staff who appear in both 1944 and postwar
postwar_staff_linked <- df %>%
  filter(year_num %in% panel_years) %>%
  inner_join(staff_1944_pos, by = "staff_id") %>%
  select(staff_id, office_id, pos_norm, war_office_id, war_pos) %>%
  distinct()

# For each postwar (office_id, pos_norm), compute the draft share from
# the 1944 positions its staff came from (weighted avg by n_male)
postwar_pos_treatment <- postwar_staff_linked %>%
  left_join(pos_1944 %>% select(office_id, pos_norm, draft_share_1944, n_drafted_1944, n_male_1944),
            by = c("war_office_id" = "office_id", "war_pos" = "pos_norm")) %>%
  filter(!is.na(draft_share_1944)) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    draft_share_1944 = {
      src <- distinct(select(cur_data(), war_office_id, war_pos, draft_share_1944, n_male_1944))
      if (sum(src$n_male_1944, na.rm = TRUE) > 0) {
        weighted.mean(src$draft_share_1944, src$n_male_1944, na.rm = TRUE)
      } else {
        mean(src$draft_share_1944, na.rm = TRUE)
      }
    },
    n_drafted_1944 = {
      src <- distinct(select(cur_data(), war_office_id, war_pos, n_drafted_1944))
      sum(src$n_drafted_1944, na.rm = TRUE)
    },
    n_male_1944 = {
      src <- distinct(select(cur_data(), war_office_id, war_pos, n_male_1944))
      sum(src$n_male_1944, na.rm = TRUE)
    },
    n_linking_staff = n_distinct(staff_id),
    .groups = "drop"
  )

cat("Postwar position cells with staff crosswalk:", nrow(postwar_pos_treatment), "\n")

# For positions that existed in 1944 with same office_id (direct match)
pos_1944_direct <- pos_1944 %>%
  select(office_id, pos_norm,
         draft_share_1944_direct = draft_share_1944,
         n_drafted_1944_direct = n_drafted_1944,
         n_male_1944_direct = n_male_1944)

# ============================================================
# 2. POSTWAR POSITION PANEL (1950)
# ============================================================

postwar_staff <- df %>%
  filter(year_num %in% panel_years)

# Kyoku: use order-based assignment (ground-truth ka order)
# Assign_Kyoku_By_Order_YYYY.R produces office_id → kyoku from document order
office_kyoku_map_1950 <- read_csv(here("Regressions", "Office_Kyoku_By_Order_1950.csv"), show_col_types = FALSE) %>%
  mutate(year_num = 1950L)
office_kyoku_map_1951 <- read_csv(here("Regressions", "Office_Kyoku_By_Order_1951.csv"), show_col_types = FALSE) %>%
  mutate(year_num = 1951L)
office_kyoku_map <- bind_rows(office_kyoku_map_1950, office_kyoku_map_1951)
kyoku_fallback <- tibble(
  kyoku_raw = c("京京都立高等保母学院", "世田谷産院", "築地産院", "荒川産院", "監察医務院", "養育院", "中央卸売市場"),
  kyoku_correct = c("民生局", "衛生局", "衛生局", "衛生局", "衛生局", "民生局", "経済局")
)
postwar_staff <- postwar_staff %>%
  left_join(office_kyoku_map, by = c("office_id", "year_num")) %>%
  left_join(kyoku_fallback, by = c("kyoku" = "kyoku_raw")) %>%
  mutate(kyoku = coalesce(kyoku_assigned, kyoku_correct, kyoku)) %>%
  select(-kyoku_assigned, -kyoku_correct)

# Outcome at KAKARI level: does this kakari have a female kakari-cho (係長)?
# Use "any" definition: has_female_kakacho = 1 if ANY 係長 in the kakari is female
has_kakari <- all(c("ka", "kakari") %in% names(postwar_staff))
if (!has_kakari) stop("Data must have ka and kakari for kakari-level analysis")

kakari_outcome <- postwar_staff %>%
  filter(!is.na(ka), !is.na(kakari)) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    n_female_kakacho  = sum(is_kakacho & is_female, na.rm = TRUE),
    has_female_kakacho = as.integer(any(is_kakacho & is_female, na.rm = TRUE)),
    has_kakacho       = any(is_kakacho, na.rm = TRUE),
    n_kakacho         = sum(is_kakacho, na.rm = TRUE),
    kakari_size       = n(),
    n_female          = sum(is_female, na.rm = TRUE),
    female_share      = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

# Modal kyoku for FE (office_id × year → kyoku)
office_kyoku <- postwar_staff %>%
  count(office_id, year_num, kyoku) %>%
  group_by(office_id, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, year_num, kyoku_modal = kyoku)

# Engineer share in the kakari (for controls)
kakari_engineer <- postwar_staff %>%
  filter(!is.na(ka), !is.na(kakari)) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    kakari_size   = n(),
    n_engineer    = sum(str_detect(pos_norm, "技師|技手|技術"), na.rm = TRUE),
    engineer_share_team = n_engineer / kakari_size,
    .groups = "drop"
  )

# Map position → kakari (for aggregating treatment)
pos_to_kakari <- postwar_staff %>%
  filter(!is.na(ka), !is.na(kakari)) %>%
  group_by(office_id, pos_norm, year_num, ka, kakari) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(office_id, pos_norm, year_num) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, year_num, ka, kakari)

# Treatment at kakari level: aggregate from position-level (staff crosswalk)
kakari_treatment <- pos_to_kakari %>%
  left_join(postwar_pos_treatment %>% select(office_id, pos_norm, n_drafted_1944, n_male_1944, draft_share_1944),
            by = c("office_id", "pos_norm")) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    n_drafted_1944 = sum(n_drafted_1944, na.rm = TRUE),
    n_male_1944    = sum(n_male_1944, na.rm = TRUE),
    draft_share_1944 = if_else(sum(n_male_1944, na.rm = TRUE) > 0,
                               sum(n_drafted_1944, na.rm = TRUE) / sum(n_male_1944, na.rm = TRUE),
                               0),
    .groups = "drop"
  )

# ============================================================
# 3. KAKARI PANEL ASSEMBLY
# ============================================================

kakari_panel <- kakari_outcome %>%
  left_join(kakari_treatment, by = c("office_id", "ka", "kakari", "year_num")) %>%
  left_join(office_kyoku, by = c("office_id", "year_num")) %>%
  left_join(kakari_engineer %>% select(office_id, ka, kakari, year_num, engineer_share_team),
            by = c("office_id", "ka", "kakari", "year_num")) %>%
  mutate(
    n_drafted_1944 = replace_na(n_drafted_1944, 0),
    n_male_1944    = replace_na(n_male_1944, 0),
    draft_share_1944 = replace_na(draft_share_1944, 0),
    kakari_id     = paste(office_id, ka, kakari, sep = "_")
  )

# --- DIAGNOSTIC: Female kakari-cho counts ---
cat("\n--- Female 係長 diagnostic (1950–1951) ---\n")
cat("Person-level: female with 係長:", sum(postwar_staff$is_female & postwar_staff$is_kakacho, na.rm = TRUE), "\n")
# Current definition: we pick ONE 係長 per kakari (primary or first); count kakari where that person is female
cat("Kakari with has_female_kakacho=1 (single 係長 per kakari):", sum(kakari_panel$has_female_kakacho == 1), "\n")
# Alternative: kakari has ANY female 係長
any_female <- postwar_staff %>%
  filter(!is.na(ka), !is.na(kakari), is_kakacho, is_female) %>%
  distinct(office_id, ka, kakari, year_num) %>%
  nrow()
cat("Kakari with ANY female 係長:", any_female, "\n")
cat("(Gap: we pick one 係長 per kakari; when multiple 係長 exist, we may pick male primary)\n")
cat("By kyoku - female kakari-cho:\n")
print(kakari_panel %>% filter(has_female_kakacho == 1) %>% count(kyoku_modal, name = "n_female_kakacho") %>% arrange(desc(n_female_kakacho)), n = Inf)

cat("\n--- Panel diagnostics (kakari level) ---\n")
cat("Total kakari-year observations (1950–1951):", nrow(kakari_panel), "\n")
cat("With nonzero draft share:", sum(kakari_panel$n_drafted_1944 > 0, na.rm = TRUE), "\n")
cat("With female kakacho:", sum(kakari_panel$n_female_kakacho > 0), "\n")
cat("Mean n_drafted:", round(mean(kakari_panel$n_drafted_1944), 4), "\n")
cat("Mean n_female_kakacho:", round(mean(kakari_panel$n_female_kakacho), 4), "\n")

# ============================================================
# 4. FIRST STAGE REGRESSIONS (Intensive margin)
# ============================================================
# Y: n_female_kakacho (count of female 係長 per kakari)
# X: n_drafted_1944 (count)
# FE added progressively: year → kyoku → controls
# Cluster: kakari_id (office_id × ka × kakari)

# Restrict to kakari with kakacho; include 区部 and 建築局
est <- kakari_panel %>%
  filter(has_kakacho == 1)

if (nrow(est) == 0) {
  stop("No kakari with has_kakacho==1. Check that position data contains \u5951\u9577 (kakari-cho). ",
       "If running from CLI, try running from RStudio where encoding may differ.")
}

cat("\n--- Estimation sample (kakari-years with kakacho, all kyoku) ---\n")
cat("N:", nrow(est), "\n")
cat("N distinct kakari:", n_distinct(est$kakari_id), "\n")
cat("Mean n_drafted:", round(mean(est$n_drafted_1944), 4), "\n")
cat("Mean n_female_kakacho:", round(mean(est$n_female_kakacho), 4), "\n")

# --- DIAGNOSTIC: n_drafted by kyoku, kakari per kyoku ---
by_kyoku <- est %>%
  group_by(kyoku_modal) %>%
  summarise(
    n_kakari = n(),
    mean_drafted = mean(n_drafted_1944, na.rm = TRUE),
    sd_drafted = sd(n_drafted_1944, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_kakari))
cat("\n--- n_drafted_1944 by kyoku (kakari-level sample) ---\n")
print(by_kyoku, n = Inf)

# Spec 1: No FE
fs1 <- feols(
  n_female_kakacho ~ n_drafted_1944,
  data = est, cluster = ~kakari_id
)

# Spec 2: Year FE
fs2 <- feols(
  n_female_kakacho ~ n_drafted_1944 | year_num,
  data = est, cluster = ~kakari_id
)

# Spec 3: Year + kyoku FE
fs3 <- feols(
  n_female_kakacho ~ n_drafted_1944 | year_num + kyoku_modal,
  data = est, cluster = ~kakari_id
)

# Spec 4: Year + kyoku FE + log(n_male_1944)
fs4 <- feols(
  n_female_kakacho ~ n_drafted_1944 + log(n_male_1944 + 1) |
    year_num + kyoku_modal,
  data = est, cluster = ~kakari_id
)

# Spec 5: Year + kyoku FE + log(n_male_1944) + engineer share in team
fs5 <- feols(
  n_female_kakacho ~ n_drafted_1944 + log(n_male_1944 + 1) + engineer_share_team |
    year_num + kyoku_modal,
  data = est, cluster = ~kakari_id
)

cat("\n========== FIRST STAGE (Intensive): 1944 Draft Count → N Female Kakari-cho ==========\n")
etable(
  fs1, fs2, fs3, fs4, fs5,
  headers = c(
    "(1)\nNo FE",
    "(2)\nYear",
    "(3)\nYear+Kyoku",
    "(4)\n+log(males)",
    "(5)\n+Eng. share"
  ),
  se.below = TRUE,
  fitstat = ~ n + r2 + f
)

# --- DIAGNOSTIC: Year-by-year to explain F-drop ---
est_1950 <- est %>% filter(year_num == 1950)
est_1951 <- est %>% filter(year_num == 1951)
overlap_ids <- intersect(est_1950$kakari_id, est_1951$kakari_id)
cat("\n--- Year-by-year diagnostic (why F dropped when adding 1951) ---\n")
cat("1950-only: N =", nrow(est_1950), ", distinct kakari =", n_distinct(est_1950$kakari_id), "\n")
cat("1951-only: N =", nrow(est_1951), ", distinct kakari =", n_distinct(est_1951$kakari_id), "\n")
cat("Kakari appearing in BOTH years:", length(overlap_ids), "(n_drafted identical across years = no within-unit variation)\n")
m_1950 <- feols(n_female_kakacho ~ n_drafted_1944 | kyoku_modal, data = est_1950, cluster = ~kakari_id)
m_1951 <- feols(n_female_kakacho ~ n_drafted_1944 | kyoku_modal, data = est_1951, cluster = ~kakari_id)
f_1950 <- as.numeric(fitstat(m_1950, "f")$stat)
f_1951 <- as.numeric(fitstat(m_1951, "f")$stat)
se_1950 <- sqrt(vcov(m_1950)["n_drafted_1944", "n_drafted_1944"])
se_1951 <- sqrt(vcov(m_1951)["n_drafted_1944", "n_drafted_1944"])
cat("\n1950-only (Kyoku FE): coef =", round(coef(m_1950), 5), ", SE =", round(se_1950, 5), ", F =", round(f_1950, 2), "\n")
cat("1951-only (Kyoku FE): coef =", round(coef(m_1951), 5), ", SE =", round(se_1951, 5), ", F =", round(f_1951, 2), "\n")
cat("\n-> If 1951 coef is smaller or F is lower: pooling dilutes the relationship.\n")
cat("-> n_drafted_1944 is time-invariant per kakari; within-kakari obs add noise, not signal.\n")

# ============================================================
# 5. EXPORT LaTeX TABLE
# ============================================================

dict_table <- c(
  n_drafted_1944         = "N drafted (1944)",
  "log(n_male_1944 + 1)" = "log(N males 1944 + 1)",
  engineer_share_team    = "Engineer share (team)"
)

tex_table <- etable(
  fs1, fs2, fs3, fs4, fs5,
  dict = dict_table,
  headers = c("(1)", "(2)", "(3)", "(4)", "(5)"),
  se.below = TRUE,
  fitstat = ~ n + r2 + f,
  tex = TRUE
)

tex_out <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{geometry}",
  "\\geometry{margin=1in}",
  "\\begin{document}",
  "\\section*{First Stage (Intensive): 1944 Draft Count $\\rightarrow$ N Female Kakari-cho}",
  "\\vspace{0.5em}",
  "{\\footnotesize Dep.\\ var.: count of female 係長 (kakari-cho) in the kakari.",
  "Unit of observation: kakari (office $\\times$ ka $\\times$ kakari $\\times$ year, 1950--1951).",
  "Treatment assigned via staff crosswalk. Kyoku corrected via year-specific ground-truth.",
  "Engineer share (team) = share of engineers in the kakari.",
  "Sample restricted to kakari with at least one kakari-cho. All kyoku included.",
  "Standard errors clustered at the kakari level.}",
  "\\vspace{1em}",
  tex_table,
  "\\end{document}"
)

writeLines(tex_out, here("FirstStage_FemaleManager.tex"))
cat("\nTable exported to FirstStage_FemaleManager.tex\n")
