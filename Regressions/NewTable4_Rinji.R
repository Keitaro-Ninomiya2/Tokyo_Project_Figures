################################################################################
# Table 4: Bonus Salary (Rinji Tokubetsu) Response to Military Drafting
# Did offices respond to drafting by offering bonuses to remaining workers?
# Unit: office x position (1944 cross-section)
# Outcome: number of workers receiving rinji bonus
# Treatment: number of male workers drafted from same office x position
# Columns: (1) Baseline, (2) × Rank, (3) × Engineer, (4) × Gender
# All columns use the full sample with interactions instead of split samples
################################################################################

library(tidyverse)
library(fixest)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# ============================================================
# 1. BUILD 1944 OFFICE x POSITION PANEL
# ============================================================

# Draft counts at office x position level for 1944
position_drafts <- df_all %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Cumulative male baseline (workers before 1944)
cumul_male_stock <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop")

# Ka mapping
pos_ka_map <- df %>%
  filter(year_num == 1944, !is.na(ka)) %>%
  count(office_id, pos_norm, ka, kyoku) %>%
  group_by(office_id, pos_norm) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(office_id, pos_norm, ka, kyoku)

# Exclude anyone who was drafted
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# Aggregate to office x position level: count of rinji recipients
panel <- df %>%
  filter(year_num == 1944, !(staff_id %in% drafted_ids)) %>%
  mutate(has_rinji = as.integer(!is.na(rinji) & rinji != "")) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    n_rinji = sum(has_rinji),
    n_workers = n(),
    n_female = sum(is_female),
    n_male = sum(!is_female),
    .groups = "drop"
  ) %>%
  left_join(position_drafts, by = c("office_id", "pos_norm")) %>%
  left_join(cumul_male_stock, by = c("office_id", "pos_norm")) %>%
  left_join(pos_ka_map, by = c("office_id", "pos_norm")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

# ============================================================
# 1b. CLASSIFY POSITIONS BY RANK AND ENGINEER STATUS
# ============================================================

# Rank assignment (pre-1948 scheme):
#   Rank 3: shuji / gishi (主事 / 技師)
#   Rank 1: yatoi / shokutaku (雇 / 囑託)
#   Rank 2: everything else (base category)
panel <- panel %>%
  mutate(
    pos_rank = case_when(
      str_detect(pos_norm, "^主事$|^技師$") ~ 3L,
      str_detect(pos_norm, "^雇$|^囑託$")   ~ 1L,
      TRUE                                   ~ 2L
    ),
    is_engineer = as.integer(str_detect(pos_norm, "技")),
    is_rank1    = as.integer(pos_rank == 1L),
    is_rank3    = as.integer(pos_rank == 3L),
    female_share = n_female / n_workers
  )

panel_ka <- panel %>% filter(!is.na(ka_id))
cat("1944 office x position panel:", nrow(panel_ka), "obs\n")
cat("Total rinji recipients:", sum(panel_ka$n_rinji), "\n")
cat("Positions with any rinji:", sum(panel_ka$n_rinji > 0), "\n")
cat("Positions with any drafted:", sum(panel_ka$n_drafted_male > 0), "\n")
cat("\nRank distribution:\n")
print(table(panel_ka$pos_rank))
cat("\nEngineer distribution:\n")
print(table(panel_ka$is_engineer))
cat("\nFemale share summary:\n")
print(summary(panel_ka$female_share))

# ============================================================
# 2. REGRESSIONS (interaction design, full sample throughout)
# ============================================================

# C1: Baseline
c1 <- feols(n_rinji ~ n_drafted_male + log(cumul_n_male + 1) | ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: × Rank (rank2 = base; rank main effects absorbed by pos_norm FE)
c2 <- feols(n_rinji ~ n_drafted_male + n_drafted_male:is_rank1 + n_drafted_male:is_rank3
            + log(cumul_n_male + 1) | ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: × Engineer (engineer main effect absorbed by pos_norm FE)
c3 <- feols(n_rinji ~ n_drafted_male + n_drafted_male:is_engineer
            + log(cumul_n_male + 1) | ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: × Gender (female_share is continuous)
c4 <- feols(n_rinji ~ n_drafted_male + n_drafted_male:female_share + female_share
            + log(cumul_n_male + 1) | ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

cat("\n===== C1: Baseline =====\n")
etable(c1, se.below = TRUE, fitstat = ~n + r2)
cat("\n===== C2: × Rank =====\n")
etable(c2, se.below = TRUE, fitstat = ~n + r2)
cat("\n===== C3: × Engineer =====\n")
etable(c3, se.below = TRUE, fitstat = ~n + r2)
cat("\n===== C4: × Gender =====\n")
etable(c4, se.below = TRUE, fitstat = ~n + r2)

# ============================================================
# 3. EXPORT LaTeX
# ============================================================

dict <- c(
  n_drafted_male            = "No. drafted workers",
  "n_drafted_male:is_rank1" = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3" = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer" = "No. drafted $\\times$ Engineer",
  "n_drafted_male:female_share" = "No. drafted $\\times$ Female share",
  female_share              = "Female share"
)

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else {
    tex_raw
  }
}

tex <- etable(c1, c2, c3, c4,
              dict = dict,
              headers = c("Baseline", "$\\times$ Rank",
                          "$\\times$ Engineer", "$\\times$ Gender"),
              order = c("No. drafted", "Female share"),
              drop = "log",
              se.below = TRUE, fitstat = ~n,
              tex = TRUE)

tc <- extract_tabular(tex)
# Remove any auto-generated dep-var row
tc <- tc[!grepl("Dependent Var|n\\_rinji", tc)]

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Bonus Salary (\\emph{Rinji Tokubetsu}) Response to Drafting}",
  "\\label{tab:rinji}",
  "\\begin{threeparttable}",
  paste0("\\multicolumn{5}{l}{\\textit{Dep.\\ var: No.\\ workers receiving ",
         "\\emph{rinji tokubetsu}}} \\\\[4pt]"),
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regression using 1944 cross-section. ",
    "Unit: position $\\times$ office. Dependent variable: number of non-drafted workers ",
    "receiving \\emph{rinji tokubetsu} (temporary special allowance). ",
    "Treatment: number of male workers drafted from the same office $\\times$ position. ",
    "Column (1) is the baseline specification. ",
    "Column (2) interacts the treatment with rank indicators (Rank 2 = regular civil servant is the base; ",
    "Rank 1 = \\emph{yatoi}/\\emph{shokutaku}; Rank 3 = \\emph{shuji}/\\emph{gishi}). ",
    "Column (3) interacts with an engineer indicator. ",
    "Column (4) interacts with the female share of workers in the position. ",
    "All specifications include ka and position fixed effects and control for log cumulative male baseline. ",
    "Standard errors clustered at the office level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable4_Rinji.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable4_Rinji.tex"))
cat("\nTable 4 exported to NewTable4_Rinji.tex and Tokyo_Project\n")
