################################################################################
# Long-Run Outcomes of Transferred Workers
#
# Compare transferred workers to:
#   (A) Peers at the DESTINATION office (non-transferred workers already there)
#   (B) Peers at the DONOR office (workers who stayed behind)
#
# Unit of observation: individual worker
# Controls: peer composition at the relevant office (avg rank, avg tenure,
#           female share, headcount)
#
# Outcomes: tenure after transfer, postwar survival, postwar years, rank gain
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

years_of_interest <- 1938:1945
postwar_years <- 1947:1955

# ============================================================
# KYOKU NORMALIZATION
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
    str_detect(k, "計[畫画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醉院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

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

df <- df %>%
  mutate(
    norm_kyoku = normalize_kyoku(kyoku),
    pos_rank   = assign_rank(pos_norm, year_num)
  )

# ============================================================
# 1. IDENTIFY TRANSFERS (cross-ka or cross-kyoku moves)
# ============================================================

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, norm_kyoku, pos_norm, pos_rank) %>%
  rename_with(~paste0("lag_", .), -c(staff_id, year_num)) %>%
  mutate(year_num = year_num + 1)

worker_panel <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office_id), !is.na(lag_ka), !is.na(norm_kyoku), !is.na(lag_norm_kyoku)) %>%
  mutate(
    transferred = as.integer(lag_ka != ka | lag_norm_kyoku != norm_kyoku),
    transfer_year = year_num
  )

transferred_workers <- worker_panel %>%
  filter(transferred == 1) %>%
  select(staff_id, transfer_year,
         donor_office = lag_office_id, donor_ka = lag_ka,
         dest_office = office_id, dest_ka = ka,
         dest_pos = pos_norm, dest_rank = pos_rank,
         donor_pos = lag_pos_norm, donor_rank = lag_pos_rank,
         is_female, norm_kyoku, lag_norm_kyoku)

cat("Total transfers:", nrow(transferred_workers), "\n")

# ============================================================
# 2. COMPUTE LONG-RUN OUTCOMES FOR ALL WORKERS
# ============================================================

worker_outcomes <- df %>%
  group_by(staff_id) %>%
  summarise(
    max_rank     = max(pos_rank, na.rm = TRUE),
    last_year    = max(year_num),
    first_year   = min(year_num),
    total_years  = n_distinct(year_num),
    .groups = "drop"
  )

postwar_outcomes <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(
    postwar_max_rank  = max(pos_rank, na.rm = TRUE),
    postwar_years     = n_distinct(year_num),
    postwar_last_year = max(year_num),
    .groups = "drop"
  )

# ============================================================
# 3. COMPUTE PEER COMPOSITION AT OFFICE LEVEL
# ============================================================

# For each (office_id, ka, year_num): compute peer characteristics
office_composition <- df %>%
  filter(year_num %in% years_of_interest, !is.na(ka)) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    peer_headcount  = n(),
    peer_avg_rank   = mean(pos_rank, na.rm = TRUE),
    peer_female_share = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

# Compute peer tenure: mean years since first appearance
tenure_df <- df %>%
  group_by(staff_id) %>%
  summarise(first_yr = min(year_num), .groups = "drop")

office_tenure <- df %>%
  filter(year_num %in% years_of_interest, !is.na(ka)) %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(tenure = year_num - first_yr) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(peer_avg_tenure = mean(tenure, na.rm = TRUE), .groups = "drop")

office_composition <- office_composition %>%
  left_join(office_tenure, by = c("office_id", "ka", "year_num"))

# ============================================================
# 4. PANEL A: TRANSFERRED VS DESTINATION PEERS (employee level)
# ============================================================

cat("\n=== PANEL A: TRANSFERRED vs DESTINATION PEERS ===\n")

# Get all destination cells from transfers
dest_cells <- transferred_workers %>%
  distinct(dest_office, dest_ka, transfer_year)

# All workers at destination offices in transfer year
dest_all_workers <- dest_cells %>%
  inner_join(
    df %>% filter(year_num %in% years_of_interest, !is.na(ka)) %>%
      select(staff_id, office_id, ka, year_num, pos_norm, pos_rank, is_female),
    by = c("dest_office" = "office_id", "dest_ka" = "ka", "transfer_year" = "year_num")
  )

# Mark which are transferred
transferred_ids <- transferred_workers %>%
  select(staff_id, transfer_year, dest_office, dest_ka) %>%
  mutate(is_transferred = 1L)

dest_individual <- dest_all_workers %>%
  left_join(transferred_ids, by = c("staff_id", "transfer_year", "dest_office", "dest_ka")) %>%
  mutate(is_transferred = replace_na(is_transferred, 0L)) %>%
  # Add long-run outcomes
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  # Add peer composition at destination
  left_join(office_composition,
            by = c("dest_office" = "office_id", "dest_ka" = "ka", "transfer_year" = "year_num")) %>%
  mutate(
    years_after = last_year - transfer_year,
    postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
    postwar_years = replace_na(postwar_years, 0),
    rank_gain = max_rank - pos_rank,
    dest_ka_id = paste(dest_office, dest_ka, sep = "_")
  )

cat("Panel A observations:", nrow(dest_individual), "\n")
cat("  Transferred:", sum(dest_individual$is_transferred == 1), "\n")
cat("  Peers:", sum(dest_individual$is_transferred == 0), "\n")

# Regressions with peer composition controls
d1 <- feols(years_after ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              dest_ka_id + transfer_year + pos_norm,
            data = dest_individual %>% filter(!is.na(dest_ka_id), !is.na(pos_norm)),
            cluster = ~dest_ka_id)

d2 <- feols(postwar_survived ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              dest_ka_id + transfer_year + pos_norm,
            data = dest_individual %>% filter(!is.na(dest_ka_id), !is.na(pos_norm)),
            cluster = ~dest_ka_id)

d3 <- feols(postwar_years ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              dest_ka_id + transfer_year + pos_norm,
            data = dest_individual %>% filter(!is.na(dest_ka_id), !is.na(pos_norm)),
            cluster = ~dest_ka_id)

d4 <- feols(rank_gain ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              dest_ka_id + transfer_year + pos_norm,
            data = dest_individual %>% filter(!is.na(dest_ka_id), !is.na(rank_gain), !is.na(pos_norm)),
            cluster = ~dest_ka_id)

cat("\n=== PANEL A REGRESSIONS ===\n")
etable(d1, d2, d3, d4,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"))

# ============================================================
# 5. PANEL B: TRANSFERRED VS DONOR PEERS (employee level)
# ============================================================

cat("\n=== PANEL B: TRANSFERRED vs DONOR PEERS ===\n")

# Get all donor cells
donor_cells <- transferred_workers %>%
  distinct(donor_office, donor_ka, transfer_year) %>%
  mutate(donor_year = transfer_year - 1)

# All workers at donor offices in year before transfer
donor_all_workers <- donor_cells %>%
  inner_join(
    df %>% filter(year_num %in% (min(years_of_interest) - 1):max(years_of_interest), !is.na(ka)) %>%
      select(staff_id, office_id, ka, year_num, pos_norm, pos_rank, is_female),
    by = c("donor_office" = "office_id", "donor_ka" = "ka", "donor_year" = "year_num")
  )

# Mark transferred
donor_transferred_ids <- transferred_workers %>%
  select(staff_id, transfer_year, donor_office, donor_ka) %>%
  mutate(is_transferred = 1L)

donor_individual <- donor_all_workers %>%
  left_join(donor_transferred_ids, by = c("staff_id", "transfer_year", "donor_office", "donor_ka")) %>%
  mutate(is_transferred = replace_na(is_transferred, 0L)) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  # Add peer composition at donor office (in pre-transfer year)
  left_join(office_composition,
            by = c("donor_office" = "office_id", "donor_ka" = "ka", "donor_year" = "year_num")) %>%
  mutate(
    years_after = last_year - transfer_year,
    postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
    postwar_years = replace_na(postwar_years, 0),
    rank_gain = max_rank - pos_rank,
    donor_ka_id = paste(donor_office, donor_ka, sep = "_")
  )

cat("Panel B observations:", nrow(donor_individual), "\n")
cat("  Transferred:", sum(donor_individual$is_transferred == 1), "\n")
cat("  Donor peers:", sum(donor_individual$is_transferred == 0), "\n")

s1 <- feols(years_after ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              donor_ka_id + transfer_year + pos_norm,
            data = donor_individual %>% filter(!is.na(donor_ka_id), !is.na(pos_norm)),
            cluster = ~donor_ka_id)

s2 <- feols(postwar_survived ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              donor_ka_id + transfer_year + pos_norm,
            data = donor_individual %>% filter(!is.na(donor_ka_id), !is.na(pos_norm)),
            cluster = ~donor_ka_id)

s3 <- feols(postwar_years ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              donor_ka_id + transfer_year + pos_norm,
            data = donor_individual %>% filter(!is.na(donor_ka_id), !is.na(pos_norm)),
            cluster = ~donor_ka_id)

s4 <- feols(rank_gain ~ is_transferred + peer_avg_rank + peer_avg_tenure +
              peer_female_share + log(peer_headcount + 1) |
              donor_ka_id + transfer_year + pos_norm,
            data = donor_individual %>% filter(!is.na(donor_ka_id), !is.na(rank_gain), !is.na(pos_norm)),
            cluster = ~donor_ka_id)

cat("\n=== PANEL B REGRESSIONS ===\n")
etable(s1, s2, s3, s4,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"))

# ============================================================
# 6. EXPORT LaTeX TABLE
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
  drop_idx <- grep("Dependent Var", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  tex_content
}

dict <- c(
  is_transferred = "Transferred (=1)",
  dest_ka_id = "Dest. Kakari",
  donor_ka_id = "Donor Kakari",
  transfer_year = "Year",
  pos_norm = "Position"
)

# Panel A table
tex_a <- etable(d1, d2, d3, d4,
                dict = dict,
                headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"),
                se.below = TRUE, fitstat = ~n + r2 + G,
                tex = TRUE)
tc_a <- clean_depvar(extract_tabular(tex_a))

# Panel B table
tex_b <- etable(s1, s2, s3, s4,
                dict = dict,
                headers = c("Tenure After", "Postwar Surv.", "Postwar Yrs", "Rank Gain"),
                se.below = TRUE, fitstat = ~n + r2 + G,
                tex = TRUE)
tc_b <- clean_depvar(extract_tabular(tex_b))

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Long-Run Outcomes of Transferred Workers}",
  "\\label{tab:transfer-outcomes}",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: Transferred vs.\\ Destination Office Peers}",
  "\\medskip",
  "",
  tc_a,
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Transferred vs.\\ Donor Office Peers}",
  "\\medskip",
  "",
  tc_b,
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regressions comparing transferred workers to peers. ",
    "Unit of observation: individual worker. ",
    "Panel~A compares transferred workers to non-transferred workers already at the ",
    "destination office in the transfer year; Panel~B compares to workers who remained ",
    "at the donor office in the year before transfer. ",
    "``Tenure After'' = years observed after the transfer year. ",
    "``Postwar Surv.'' = 1 if worker appears in any year 1947--1955. ",
    "``Postwar Yrs'' = number of years observed 1947--1955. ",
    "``Rank Gain'' = maximum rank achieved minus rank at time of transfer. ",
    "All specifications control for peer composition at the relevant office ",
    "(average rank, average tenure, female share, log headcount). ",
    "Panel~A includes destination kakari, year, and position FE; ",
    "Panel~B includes donor kakari, year, and position FE. ",
    "Standard errors clustered at the kakari level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("TransferOutcomes.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "TransferOutcomes.tex"))
cat("\nTable exported to TransferOutcomes.tex\n")
