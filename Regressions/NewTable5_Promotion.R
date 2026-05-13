################################################################################
# Table 5: Short-Run Promotion in Response to Military Drafting
# Unit: kakari x occupation x year
# Outcome: n_promoted (count of promoted workers in cell)
# Treatment: n_drafted_male at kakari level (continuous)
# FE: Ka (section) + occupation + year
# Comparison: across kakari within same ka, conditional on occupation
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

# Rank assignment (pre-1948 scheme)
assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

# Occupation classification (matches BalanceTest.R)
classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "技") ~ "engineer",
    str_detect(pos, "雇|傭") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

df <- df %>%
  mutate(pos_rank = assign_rank(pos_norm, year_num),
         occupation = classify_occ(pos_norm))

# Draft years with any drafting
draft_years <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(year_num) %>%
  pull(year_num) %>%
  sort()
cat("Years with draft info:", paste(draft_years, collapse = ", "), "\n")

# Exclude ever-drafted workers from the outcome panel
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# ============================================================
# 1. DRAFT COUNTS AT OFFICE LEVEL
#    Note: 'kakari' is not populated in wartime yearbooks.
#    office_id is the finest available team-level identifier.
# ============================================================

office_drafts <- df_all %>%
  filter(year_num %in% draft_years, drafted == TRUE, !is_female) %>%
  group_by(office_id, year_num) %>%
  summarise(n_drafted_office = n(), .groups = "drop")

cat("Office-year cells with any drafting:", nrow(office_drafts), "\n")

# ============================================================
# 2. WORKER-LEVEL TRANSITIONS FOR PROMOTION
#
# Definition: workers present in the drafted office at year t,
# whose rank at t is higher than their rank at t-1
# (wherever they were at t-1). Includes transferees who arrived
# at a higher rank. Excludes drafted workers.
# ============================================================

# Rank at t-1 for every worker (irrespective of office)
rank_lag <- df %>%
  select(staff_id, year_num, lag_rank = pos_rank) %>%
  mutate(year_num = year_num + 1)

promotion_rows <- list()
for (t in draft_years) {
  # Workers in the drafted office at year t (non-drafted)
  workers_curr <- df %>%
    filter(year_num == t,
           !(staff_id %in% drafted_ids),
           !is.na(kyoku), !is.na(ka)) %>%
    select(staff_id, office_id, kyoku, ka, pos_norm, pos_rank,
           occupation, is_female) %>%
    mutate(year_num = t)

  matched <- workers_curr %>%
    left_join(rank_lag, by = c("staff_id", "year_num")) %>%
    mutate(
      # promoted = rank at t higher than rank at t-1
      # NA if no prior year record (new entrant with no t-1 obs)
      promoted   = case_when(
        is.na(lag_rank) ~ NA_integer_,
        TRUE            ~ as.integer(pos_rank > lag_rank)
      ),
      draft_year = t
    )

  promotion_rows[[as.character(t)]] <- matched
}

worker_panel <- bind_rows(promotion_rows) %>%
  mutate(ka_id    = paste(kyoku, ka, sep = "_"),
         year_num = draft_year)

cat("\nWorker-year obs in drafted offices:", nrow(worker_panel), "\n")
cat("With prior year rank:", sum(!is.na(worker_panel$promoted)), "\n")

# ============================================================
# 3. AGGREGATE TO OFFICE x OCCUPATION x YEAR
# ============================================================

# Cumulative male baseline at office level (for size control)
cumul_office_male <- map_dfr(draft_years, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

panel_kakari <- worker_panel %>%
  group_by(office_id, ka_id, occupation, year_num) %>%
  summarise(
    n_workers  = n(),
    n_promoted = sum(promoted, na.rm = TRUE),
    n_female   = sum(is_female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(office_drafts, by = c("office_id", "year_num")) %>%
  left_join(cumul_office_male, by = c("office_id", "year_num")) %>%
  mutate(
    n_drafted_office = replace_na(n_drafted_office, 0L),
    cumul_n_male     = replace_na(cumul_n_male, 0L)
  )

cat("Office x occupation x year panel:", nrow(panel_kakari), "obs\n")
cat("Cells with any drafting:", sum(panel_kakari$n_drafted_office > 0), "\n")
cat("Mean n_promoted:", round(mean(panel_kakari$n_promoted), 3), "\n")
cat("Occupation distribution:\n")
print(table(panel_kakari$occupation))

# ============================================================
# 4. REGRESSIONS
# ============================================================

cat("\n===== TABLE 5: PROMOTION AT KAKARI x OCCUPATION LEVEL =====\n\n")

# C1: Baseline
m1 <- feols(n_promoted ~ n_drafted_office + log(cumul_n_male + 1) |
              year_num + ka_id + occupation,
            data = panel_kakari, cluster = ~office_id)

# C2: x Rank (interact with share of rank-1 workers in cell)
panel_kakari <- panel_kakari %>%
  left_join(
    worker_panel %>%
      mutate(is_rank1 = as.integer(pos_rank == 1),
             is_rank3 = as.integer(pos_rank == 3)) %>%
      group_by(office_id, occupation, year_num) %>%
      summarise(share_rank1 = mean(is_rank1), share_rank3 = mean(is_rank3),
                .groups = "drop"),
    by = c("office_id", "occupation", "year_num")
  )

m2 <- feols(n_promoted ~ n_drafted_office + n_drafted_office:share_rank1 +
              log(cumul_n_male + 1) |
              year_num + ka_id + occupation,
            data = panel_kakari, cluster = ~office_id)

# C3: x Engineer
panel_kakari <- panel_kakari %>%
  mutate(is_engineer = as.integer(occupation == "engineer"))

m3 <- feols(n_promoted ~ n_drafted_office + n_drafted_office:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + occupation,
            data = panel_kakari, cluster = ~office_id)

etable(m1, m2, m3,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Baseline", "x Rank", "x Engineer"))

# ============================================================
# 5. EXPORT LaTeX
# ============================================================

dict <- c(
  n_drafted_office               = "No. drafted (office)",
  "n_drafted_office:share_rank1" = "No. drafted $\\times$ Rank 1 share",
  "n_drafted_office:share_rank3" = "No. drafted $\\times$ Rank 3 share",
  "n_drafted_office:is_engineer" = "No. drafted $\\times$ Engineer"
)

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end   <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else {
    tex_raw
  }
}

tex <- etable(m1, m2, m3,
              dict = dict,
              headers = c("Baseline", "$\\times$ Rank", "$\\times$ Engineer"),
              order = c("No. drafted"),
              drop = "log",
              se.below = TRUE, fitstat = ~n + r2 + G,
              tex = TRUE)

tc <- extract_tabular(tex)
tc <- tc[!grepl("Dependent Var|n_promoted", tc)]

midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_line <- "\\multicolumn{4}{l}{\\textit{Dep.\\ var: No.\\ promoted workers (team $\\times$ occupation $\\times$ year)}} \\\\"
tc <- append(tc, depvar_line, after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Promotion Response to Drafting: Team $\\times$ Occupation Level}",
  "\\label{tab:promotion}",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regressions. Unit of observation: team (kakari) $\\times$ occupation $\\times$ year. ",
    "Outcome: number of promoted workers in the cell (rank increased from $t{-}1$ to $t$). ",
    "Sample: non-drafted workers present in the same team in consecutive years. ",
    "Treatment: number of male workers drafted from the same team. ",
    "Columns (2) interacts with the cell-level share of rank-1 workers; ",
    "Column (3) interacts with an engineer indicator. ",
    "All specifications include year, section (ka), and occupation fixed effects, ",
    "and control for log cumulative male baseline. ",
    "Standard errors clustered at the team level. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable5_Promotion.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable5_Promotion.tex"))
cat("\nTable 5 exported to NewTable5_Promotion.tex\n")
