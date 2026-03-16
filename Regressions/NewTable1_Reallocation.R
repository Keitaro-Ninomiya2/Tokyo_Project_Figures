################################################################################
# NEW Table 1: Worker Reallocation (Transfers In)
# Transfer defined as: worker's kyoku group changed from prior year
# (kyoku groups handle the 1943 Tokyo-Fu/Shi → Tokyo-To merger)
#
# C1: Baseline (total transfers in)
# C2: × Rank interactions (rank 2 = base)
# C3: × Engineer interaction
# C4: × Gender (female_share interaction, position-level)
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

years_of_interest <- 1938:1945

# ============================================================
# KYOKU NORMALIZATION + MERGER CROSSWALK
# ============================================================

# Fix OCR errors in kyoku names → canonical name
normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    # Subsidiaries → parent kyoku
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
    # OCR variants → canonical
    str_detect(k, "社会")       ~ "社会局",
    str_detect(k, "保健")       ~ "保健局",
    str_detect(k, "上木")       ~ "土木局",
    str_detect(k, "土木")       ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務")   ~ "府会事務局",
    str_detect(k, "發育")       ~ "教育局",
    str_detect(k, "教育")       ~ "教育局",
    str_detect(k, "水違")       ~ "水道局",
    str_detect(k, "水道")       ~ "水道局",
    str_detect(k, "電気")       ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務")       ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",
    str_detect(k, "経済")       ~ "経済局",
    str_detect(k, "厚生")       ~ "厚生局",
    str_detect(k, "市民")       ~ "市民局",
    str_detect(k, "中央卸売")   ~ "中央卸売市場",
    str_detect(k, "港準")       ~ "港湾局",
    str_detect(k, "港湾")       ~ "港湾局",
    str_detect(k, "監査")       ~ "監査局",
    str_detect(k, "産業")       ~ "産業局",
    str_detect(k, "城東病院")   ~ "健民局",
    str_detect(k, "健民")       ~ "健民局",
    str_detect(k, "戰時生活")   ~ "戰時生活局",
    str_detect(k, "経〓")       ~ "経理局",
    str_detect(k, "経理")       ~ "経理局",
    str_detect(k, "労働")       ~ "労働局",
    str_detect(k, "建築|御築")  ~ "建築局",
    str_detect(k, "道路")       ~ "道路局",
    str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通")       ~ "交通局",
    str_detect(k, "計[晝画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醍院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

# Map pre-1943 and post-1943 kyoku to common groups
# Based on worker flows (1942→1944) and 1944 Tokyo-To ground truth
assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("電気局", "交通局")             ~ "transport",
    nk == "水道局"                             ~ "water",
    nk == "港湾局"                             ~ "port",
    nk == "教育局"                             ~ "education",
    nk %in% c("土木局", "計画局", "経理局",
              "建築局", "道路局")               ~ "infrastructure",
    nk %in% c("戰時生活局", "経済局",
              "中央卸売市場", "産業局")         ~ "economy",
    nk %in% c("健民局", "厚生局", "社会局",
              "保健局", "民生局", "養育院",
              "労働局")                         ~ "welfare",
    nk %in% c("市会事務局", "府会事務局")      ~ "assembly",
    nk %in% c("財務局", "監査局", "長官官房")  ~ "finance_admin",
    nk == "復興事業局"                         ~ "reconstruction",
    nk == "市民局"                             ~ "citizen",
    nk == "防衛局"                             ~ "defense",
    TRUE ~ NA_character_
  )
}

# Apply normalization
df <- df %>%
  mutate(norm_kyoku   = normalize_kyoku(kyoku),
         kyoku_group  = assign_kyoku_group(norm_kyoku))

# --- Rank assignment (pre-1948) ---
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

# --- Cumulative male baseline (kakari level) ---
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# --- Transitions using kyoku_group ---
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

# Lag record: worker's prior-year kyoku_group
staff_lag <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
         lag_kyoku_group = kyoku_group, lag_norm_kyoku = norm_kyoku,
         lag_ka = ka, lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    # New hire: worker's first year in the dataset
    is_new_hire = (year_num == first_year),
    # Transfer in: worker existed last year but in a different kyoku group
    is_transfer_in = !is.na(lag_kyoku_group) & !is.na(kyoku_group) &
                     (lag_kyoku_group != kyoku_group),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

# --- Position outcomes (kakari level) ---
position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_transfers_in = sum(is_transfer_in, na.rm = TRUE),
    n_workers      = n(),
    n_female       = sum(is_female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(female_share = n_female / pmax(n_workers, 1))

# --- Draft counts (kakari level) ---
position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted = n(), n_drafted_male = sum(!is_female, na.rm = TRUE),
            .groups = "drop")

# --- Build panel ---
position_panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(n_drafted, n_drafted_male, cumul_n_male), ~replace_na(., 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

panel_ka <- position_panel %>% filter(!is.na(ka_id))
cat("Panel:", nrow(position_panel), "obs. With ka_id:", nrow(panel_ka), "\n")

# Diagnostics
cat("\nTransfer rate by year (position-level mean):\n")
panel_ka %>% group_by(year_num) %>%
  summarise(mean_transfers = mean(n_transfers_in), .groups = "drop") %>%
  print()

# ============================================================
# REGRESSIONS
# ============================================================

# C1: Baseline — total transfers in
c1 <- feols(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: × Rank interactions (rank 2 = base; rank main effects absorbed by pos_norm FE)
c2 <- feols(n_transfers_in ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: × Engineer interaction (engineer main effect absorbed by pos_norm FE)
c3 <- feols(n_transfers_in ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: × Gender (female_share interaction; main effect included)
c4 <- feols(n_transfers_in ~ n_drafted_male + n_drafted_male:female_share +
              female_share + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# PRINT RESULTS
# ============================================================

cat("\n===== TABLE 1: TRANSFERS IN =====\n\n")
etable(c1, c2, c3, c4, se.below = TRUE, fitstat = ~n)

# ============================================================
# EXPORT LaTeX
# ============================================================

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

clean_depvar <- function(tex_content) {
  drop_idx <- grep("n\\_transfers", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  drop_idx3 <- grep("^\\s*&\\s*(OLS|Poisson)", tex_content)
  if (length(drop_idx3) > 0) tex_content <- tex_content[-drop_idx3]
  tex_content
}

dict <- c(
  n_drafted_male                = "No. drafted workers",
  "n_drafted_male:is_rank1"     = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3"     = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer"  = "No. drafted $\\times$ Engineer",
  "n_drafted_male:female_share" = "No. drafted $\\times$ Female share",
  female_share                  = "Female share"
)

tex <- etable(c1, c2, c3, c4,
              dict = dict,
              order = c("No. drafted"),
              drop = "log|Female share$",
              headers = c("Baseline", "$\\times$ Rank", "$\\times$ Eng.",
                          "$\\times$ Gender"),
              se.below = TRUE, fitstat = ~n, tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Worker Reallocation: Transfers In}",
  "\\label{tab:reallocation}",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: position $\\times$ kakari $\\times$ year (1938--1945). ",
         "Dependent variable: number of workers who transferred in from a different bureau group. ",
         "Transfers are defined as workers whose normalized bureau (kyoku) group changed from the prior year, ",
         "using a crosswalk that accounts for the 1943 Tokyo-Fu/Shi to Tokyo-To merger. ",
         "Column~(2) interacts the treatment with rank indicators (rank~1 = \\emph{yatoi}/\\emph{shokutaku}; ",
         "rank~3 = \\emph{shuji}/\\emph{gishi}; rank~2 is the omitted category). ",
         "Column~(3) interacts with an engineer indicator. ",
         "Column~(4) interacts with the position's female employment share. ",
         "Rank and engineer main effects are absorbed by position fixed effects. ",
         "All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline (not shown). ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable1_Reallocation.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable1_Reallocation.tex"))
cat("\nTable 1 exported to NewTable1_Reallocation.tex\n")

# ============================================================
# APPENDIX: Adjacent Hires (unchanged from prior version)
# ============================================================

# --- Occupation classification for adjacency measures ---
position_panel2 <- position_panel %>%
  mutate(
    occupation = case_when(
      str_detect(pos_norm, "技") ~ "engineer",
      str_detect(pos_norm, "雇|傭") ~ "yato",
      TRUE ~ "non_engineer"
    )
  )

# New hires count
nh_outcomes <- staff_transitions %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_new_hires = sum(is_new_hire, na.rm = TRUE), .groups = "drop")

position_panel2 <- position_panel2 %>%
  left_join(nh_outcomes, by = c("office_id", "kakari", "pos_norm", "year_num"))

kakari_totals <- position_panel2 %>%
  group_by(office_id, kakari, year_num) %>%
  summarise(kakari_total_drafts = sum(n_drafted), .groups = "drop")

kakari_occ_totals <- position_panel2 %>%
  group_by(office_id, kakari, occupation, year_num) %>%
  summarise(kakari_occ_drafts = sum(n_drafted), .groups = "drop")

office_occ_totals <- position_panel2 %>%
  group_by(office_id, occupation, year_num) %>%
  summarise(office_occ_drafts = sum(n_drafted), .groups = "drop")

position_panel2 <- position_panel2 %>%
  left_join(kakari_totals, by = c("office_id", "kakari", "year_num")) %>%
  left_join(kakari_occ_totals, by = c("office_id", "kakari", "occupation", "year_num")) %>%
  left_join(office_occ_totals, by = c("office_id", "occupation", "year_num")) %>%
  mutate(
    adj_kakari_across_occ = kakari_total_drafts - kakari_occ_drafts,
    adj_ka_within_occ     = office_occ_drafts - n_drafted
  )

panel_ka2 <- position_panel2 %>% filter(!is.na(ka_id))

# Adjacent hire regressions
a1 <- feols(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka2, cluster = ~office_id)

a2 <- feols(n_new_hires ~ adj_ka_within_occ + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka2, cluster = ~office_id)

a3 <- feols(n_new_hires ~ adj_kakari_across_occ + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka2, cluster = ~office_id)

cat("\n===== APPENDIX: ADJACENT HIRES =====\n\n")
etable(a1, a2, a3, se.below = TRUE, fitstat = ~n)

dict_b <- c(
  n_drafted_male          = "No. drafted workers",
  adj_ka_within_occ       = "Adj. drafts (diff kakari, same occ)",
  adj_kakari_across_occ   = "Adj. drafts (diff occ, same kakari)",
  ka_id                   = "Ka"
)

tex_b <- etable(a1, a2, a3,
                dict = dict_b,
                order = c("No. drafted", "Adj\\."),
                drop = "log",
                headers = c("Own Drafts", "Adj: Diff Kakari", "Adj: Diff Occ"),
                se.below = TRUE, fitstat = ~n, tex = TRUE)

tc_b <- clean_depvar(extract_tabular(tex_b))

tex_out_b <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{New Hiring in Response to Adjacent Military Drafting}",
  "\\label{tab:reallocation-adjacent}",
  "\\begin{threeparttable}",
  tc_b,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} Unit of observation: position $\\times$ kakari $\\times$ year (1938--1945). OLS regressions. Dependent variable: number of new hires. Column (1) uses own-position drafts as treatment. Column (2) uses drafts at different kakari within the same occupation. Column (3) uses drafts at different occupation within the same kakari. All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline. Standard errors clustered at the office level in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out_b, here("NewTable1b_AdjacentHires.tex"))
writeLines(tex_out_b, here("..", "Tokyo_Project", "Appendix", "AppendixTable_AdjacentHires.tex"))
cat("Appendix table exported to AppendixTable_AdjacentHires.tex\n")
