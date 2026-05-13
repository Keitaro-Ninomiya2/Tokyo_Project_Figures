################################################################################
# Table: Transfer Decomposition by Organizational Distance
# Three dep vars: same kyoku & same ka, same kyoku diff ka, different kyoku
# ka_group from worker flows (build_ka_groups.py, 40% threshold)
# kyoku_group from historical crosswalk
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

KA_GROUP_PATH <- here("Regressions", "ka_group_map.csv")

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
# LOAD KA_GROUP MAP (from worker flows)
# ============================================================

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

cat("ka_group_map:", nrow(ka_group_raw), "rows\n")

# Join ka_group to df
df <- df %>%
  mutate(kyoku_clean = replace_na(kyoku, ""),
         ka_clean = replace_na(ka, "")) %>%
  left_join(ka_group_raw, by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka"))

cat("Workers with ka_group:", sum(!is.na(df$ka_group)), "/", nrow(df), "\n")

# ============================================================
# KYOKU NORMALIZATION + MERGER CROSSWALK
# ============================================================

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
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
    str_detect(k, "築地産院|荒産院") ~ "厚生局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

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

df <- df %>%
  mutate(norm_kyoku  = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku))

# --- Rank assignment ---
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

# --- Cumulative male baseline ---
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# --- Lag records (include ka_group and kyoku_group) ---
staff_first_year <- df %>% group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
         lag_kyoku_group = kyoku_group, lag_norm_kyoku = norm_kyoku,
         lag_ka = ka, lag_kakari = kakari, lag_pos = pos_norm,
         lag_ka_group = ka_group) %>%
  mutate(year_num = year_num + 1)

# --- Classify arrivals by distance ---
staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    arrival_type = case_when(
      # No lag record
      is.na(lag_kyoku_group) | is.na(kyoku_group) ~ NA_character_,
      # Same ka_group = stayer (same functional unit across years)
      !is.na(ka_group) & !is.na(lag_ka_group) &
        ka_group == lag_ka_group                    ~ "stayer",
      # Same kyoku group, same raw ka (fallback when ka_group missing)
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka ~ "stayer",
      # Same kyoku group, different ka = within-bureau transfer
      lag_kyoku_group == kyoku_group                ~ "same_kyoku_diff_ka",
      # Different kyoku group = cross-bureau transfer
      lag_kyoku_group != kyoku_group                ~ "diff_kyoku",
      TRUE ~ NA_character_
    ),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

# Diagnostics
cat("\nArrival type distribution:\n")
staff_transitions %>% filter(!is.na(arrival_type)) %>%
  count(arrival_type, sort = TRUE) %>% print()

cat("\nArrival type by year:\n")
staff_transitions %>% filter(!is.na(arrival_type)) %>%
  count(year_num, arrival_type) %>%
  pivot_wider(names_from = arrival_type, values_from = n, values_fill = 0) %>%
  print()

# --- Aggregate to position × kakari level ---
position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_ka            = sum(arrival_type == "stayer", na.rm = TRUE),
    n_same_kyoku_diff_ka = sum(arrival_type == "same_kyoku_diff_ka", na.rm = TRUE),
    n_diff_kyoku         = sum(arrival_type == "diff_kyoku", na.rm = TRUE),
    n_workers            = n(),
    .groups = "drop"
  )

# --- Draft counts ---
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
cat("\nPanel:", nrow(position_panel), "obs. With ka_id:", nrow(panel_ka), "\n")

# --- Totals ---
panel_ka <- panel_ka %>%
  mutate(
    n_transfers_in = n_same_kyoku_diff_ka + n_diff_kyoku
  )

# Diagnostics
cat("\nMean outcomes per position-year:\n")
panel_ka %>%
  summarise(across(c(n_same_ka, n_same_kyoku_diff_ka, n_diff_kyoku, n_transfers_in,
                     n_drafted_male), mean)) %>%
  print()

cat("\nBy year:\n")
panel_ka %>% group_by(year_num) %>%
  summarise(across(c(n_same_kyoku_diff_ka, n_diff_kyoku, n_transfers_in), mean),
            .groups = "drop") %>%
  print()

# ============================================================
# REGRESSIONS: 3 panels × 2 specs (baseline, engineer)
# Panel A: Same kyoku, same ka (retention)
# Panel B: Same kyoku, different ka (close transfer)
# Panel C: Different kyoku (distant transfer)
# ============================================================

# --- Panel A: Same kyoku, same ka (retention) ---
a1 <- feols(n_same_ka ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

a2 <- feols(n_same_ka ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- Panel B: Same kyoku, different ka (close transfer) ---
b1 <- feols(n_same_kyoku_diff_ka ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

b2 <- feols(n_same_kyoku_diff_ka ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# --- Panel C: Different kyoku (distant transfer) ---
c1 <- feols(n_diff_kyoku ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

c2 <- feols(n_diff_kyoku ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# ============================================================
# PRINT
# ============================================================

cat("\n===== TRANSFER DECOMPOSITION (ka_group from worker flows) =====\n\n")
cat("Panel A: Same kyoku, same ka (retention)\n")
etable(a1, a2, se.below = TRUE, fitstat = ~n)

cat("\nPanel B: Same kyoku, different ka (close transfer)\n")
etable(b1, b2, se.below = TRUE, fitstat = ~n)

cat("\nPanel C: Different kyoku (distant transfer)\n")
etable(c1, c2, se.below = TRUE, fitstat = ~n)

# Full 6-column table
cat("\n===== FULL TABLE =====\n\n")
etable(a1, a2, b1, b2, c1, c2, se.below = TRUE, fitstat = ~n + r2,
       headers = c("Base", "Eng",
                   "Base", "Eng",
                   "Base", "Eng"))

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
  drop_idx <- grep("n\\_same|n\\_diff|n\\_transfer", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  drop_idx3 <- grep("^\\s*&\\s*(OLS|Poisson)", tex_content)
  if (length(drop_idx3) > 0) tex_content <- tex_content[-drop_idx3]
  tex_content
}

dict <- c(
  n_drafted_male                = "No. drafted workers",
  "n_drafted_male:is_engineer"  = "No. drafted $\\times$ Engineer"
)

tex <- etable(a1, a2, b1, b2, c1, c2,
              dict = dict,
              order = c("No. drafted"),
              drop = "log",
              headers = c("Base", "$\\times$ Eng.",
                          "Base", "$\\times$ Eng.",
                          "Base", "$\\times$ Eng."),
              se.below = TRUE, fitstat = ~n + r2 + G, tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var header rows after the first \midrule
midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_row <- " & \\multicolumn{2}{c}{Retention (same section)} & \\multicolumn{2}{c}{Close transfer (same bureau)} & \\multicolumn{2}{c}{Distant transfer (diff.\\ bureau)} \\\\"
tc <- append(tc, c(depvar_row, "\\midrule"), after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Transfer Decomposition by Organizational Distance}",
  "\\label{tab:transfer-decomposition}",
  "\\scriptsize",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: position $\\times$ kakari $\\times$ year (1938--1945). ",
         "``Retention'' (columns~1--2): workers remaining in the same ka group (same bureau and section). ",
         "``Close transfer'' (columns~3--4): workers from the same kyoku group but a different ka group. ",
         "``Distant transfer'' (columns~5--6): workers from a different kyoku group. ",
         "Ka groups are constructed from worker flows across consecutive years: ",
         "two (kyoku, ka) cells in adjacent years are linked if $\\geq$40\\% of workers in the source cell appear in the destination cell. ",
         "Kyoku groups use a crosswalk that accounts for the 1943 Tokyo-Fu/Shi to Tokyo-To merger. ",
         "Engineer main effects are absorbed by position fixed effects. ",
         "All specifications include year, ka, and position fixed effects, and control for log cumulative male baseline (not shown). ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable1c_TransferType.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable1c_TransferType.tex"))
cat("\nTransfer decomposition table exported to NewTable1c_TransferType.tex\n")
