################################################################################
# Career dynamics of transferees, by distance of origin (same vs diff kyoku)
# Side-by-side with baseline (no interaction).
#
# Logic: if transferees from DISTANT departments are less subject to
# ability-based poaching, their later-career promotion should be lower
# relative to same-department transferees.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df <- df_raw %>% filter(is_name == TRUE)

classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "技") ~ "engineer",
    str_detect(pos, "雇|傭") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

# Drafted offices by year
office_drafts <- df_raw %>%
  filter(drafted == TRUE) %>%
  distinct(office_id, year_num) %>%
  mutate(drafted_office_year = 1L)

years_of_interest <- 1938:1945
postwar_years <- 1947:1955

# ---- kyoku normalization (copied verbatim from TransferOutcomes.R) ----
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

assign_rank_enriched <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr < 1948 & str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    yr < 1948 & str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr >= 1948 & str_detect(pos, "^部長$|^次長$") ~ 4L,
    yr >= 1948 & str_detect(pos, "^課長$|^所長$|^校長$|^場長$|^園長$|^支所長$|^區長$|^看護婦長$|^寮長$") ~ 3L,
    yr >= 1948 & str_detect(pos, "^係長$") ~ 2L,
    yr >= 1948 ~ 1L
  )
}

# Court rank (位階): 正八=1, 正七=2, ..., 正一=8 (higher = more productive)
parse_court_rank <- function(r) {
  case_when(
    is.na(r) ~ NA_integer_,
    str_detect(r, "^正八")  ~ 1L,
    str_detect(r, "^正七")  ~ 2L,
    str_detect(r, "^正六")  ~ 3L,
    str_detect(r, "^正五")  ~ 4L,
    str_detect(r, "^正四")  ~ 5L,
    str_detect(r, "^正三")  ~ 6L,
    str_detect(r, "^正二")  ~ 7L,
    str_detect(r, "^正一")  ~ 8L,
    str_detect(r, "^正十")  ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Decoration (勲等): 勳八=1, 勳七=2, ..., 勳一=8 (higher = more distinguished)
parse_decoration <- function(d) {
  case_when(
    is.na(d)                  ~ NA_integer_,
    str_detect(d, "^勳八|^勳十") ~ 1L,
    str_detect(d, "^勳七")    ~ 2L,
    str_detect(d, "^勳六")    ~ 3L,
    str_detect(d, "^勳五")    ~ 4L,
    str_detect(d, "^勳四")    ~ 5L,
    str_detect(d, "^勳三")    ~ 6L,
    str_detect(d, "^勳二")    ~ 7L,
    str_detect(d, "^勳一")    ~ 8L,
    TRUE                      ~ NA_integer_
  )
}

df <- df %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         pos_rank   = assign_rank(pos_norm, year_num),
         rank_e     = assign_rank_enriched(pos_norm, year_num),
         court_rank = parse_court_rank(rank),
         decor_rank = parse_decoration(decoration))

# ---- Identify transfers ----
staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, norm_kyoku, pos_norm, pos_rank, rank_e, court_rank, decor_rank) %>%
  rename_with(~paste0("lag_", .), -c(staff_id, year_num)) %>%
  mutate(year_num = year_num + 1)

worker_panel <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office_id), !is.na(lag_ka),
         !is.na(norm_kyoku), !is.na(lag_norm_kyoku)) %>%
  mutate(transferred = as.integer(lag_ka != ka | lag_norm_kyoku != norm_kyoku),
         transfer_year = year_num)

transferred_workers <- worker_panel %>%
  filter(transferred == 1) %>%
  mutate(diff_dept = as.integer(lag_norm_kyoku != norm_kyoku)) %>%
  select(staff_id, transfer_year, diff_dept,
         donor_office = lag_office_id, donor_ka = lag_ka,
         dest_office = office_id, dest_ka = ka)

cat("Total transfers:", nrow(transferred_workers), "\n")
cat("  Same-dept (within kyoku):", sum(transferred_workers$diff_dept == 0), "\n")
cat("  Diff-dept (cross kyoku):", sum(transferred_workers$diff_dept == 1), "\n")

# ---- Worker outcomes ----
worker_outcomes <- df %>%
  group_by(staff_id) %>%
  summarise(max_rank = max(pos_rank, na.rm = TRUE),
            last_year = max(year_num),
            .groups = "drop")

postwar_outcomes <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(postwar_years = n_distinct(year_num),
            .groups = "drop")

# ---- Peer composition at destination office ----
office_composition <- df %>%
  filter(year_num %in% years_of_interest, !is.na(ka)) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(peer_headcount = n(),
            peer_avg_rank  = mean(pos_rank, na.rm = TRUE),
            peer_female_share = mean(is_female, na.rm = TRUE),
            .groups = "drop")

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

# ---- Panel: transferred vs destination peers ----
dest_cells <- transferred_workers %>%
  distinct(dest_office, dest_ka, transfer_year)

dest_all_workers <- dest_cells %>%
  inner_join(
    df %>% filter(year_num %in% years_of_interest, !is.na(ka)) %>%
      select(staff_id, office_id, ka, year_num, pos_norm, pos_rank, is_female),
    by = c("dest_office" = "office_id", "dest_ka" = "ka",
           "transfer_year" = "year_num")
  )

transferred_ids <- transferred_workers %>%
  select(staff_id, transfer_year, dest_office, dest_ka, diff_dept) %>%
  mutate(is_transferred = 1L)

dest_individual <- dest_all_workers %>%
  left_join(transferred_ids,
            by = c("staff_id", "transfer_year", "dest_office", "dest_ka")) %>%
  mutate(is_transferred = replace_na(is_transferred, 0L),
         diff_dept = replace_na(diff_dept, 0L)) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(entry_year = first_yr,
         own_tenure = transfer_year - first_yr,
         occupation = classify_occ(pos_norm),
         dest_occ_id = paste(dest_office, occupation, sep = "_")) %>%
  left_join(office_drafts,
            by = c("dest_office" = "office_id", "transfer_year" = "year_num")) %>%
  filter(!is.na(drafted_office_year)) %>%
  left_join(office_composition,
            by = c("dest_office" = "office_id", "dest_ka" = "ka",
                   "transfer_year" = "year_num")) %>%
  mutate(years_after = last_year - transfer_year,
         postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
         postwar_years = replace_na(postwar_years, 0),
         rank_gain = max_rank - pos_rank,
         dest_ka_id = paste(dest_office, dest_ka, sep = "_"))

dat <- dest_individual %>%
  filter(!is.na(dest_ka_id), !is.na(pos_norm))

dat_rg <- dat %>% filter(!is.na(rank_gain))

cat("\nPanel A obs:", nrow(dat),
    " transferred:", sum(dat$is_transferred),
    " (of which diff_dept:", sum(dat$is_transferred == 1 & dat$diff_dept == 1), ")\n\n")

# ============================================================
# Regressions: baseline vs. interaction with diff_dept
# Outcomes: years_after, postwar_survived, postwar_years, rank_gain
# ============================================================

ctrls <- "own_tenure + peer_avg_rank + peer_avg_tenure + peer_female_share + log(peer_headcount + 1)"
fe    <- "dest_occ_id + transfer_year"

run_base <- function(y, d) {
  feols(as.formula(paste(y, "~ is_transferred +", ctrls, "|", fe)),
        data = d, cluster = ~dest_office)
}
run_intx <- function(y, d) {
  feols(as.formula(paste(y, "~ is_transferred + is_transferred:diff_dept +", ctrls, "|", fe)),
        data = d, cluster = ~dest_office)
}

b1 <- run_base("years_after", dat)
b2 <- run_base("postwar_survived", dat)
b3 <- run_base("postwar_years", dat)
b4 <- run_base("rank_gain", dat_rg)

i1 <- run_intx("years_after", dat)
i2 <- run_intx("postwar_survived", dat)
i3 <- run_intx("postwar_years", dat)
i4 <- run_intx("rank_gain", dat_rg)

dict <- c(
  is_transferred               = "Transferred",
  "is_transferred:diff_dept"   = "Transferred $\\times$ Diff. Dept.",
  peer_avg_rank                = "Peer avg rank",
  peer_avg_tenure              = "Peer avg tenure",
  peer_female_share            = "Peer female share",
  own_tenure                   = "Own tenure (yrs since entry)"
)

cat("\n========== SIDE-BY-SIDE: BASELINE vs x DIFF-DEPT ==========\n\n")
etable(b1, i1, b2, i2, b3, i3, b4, i4,
       dict = dict,
       drop = "log",
       headers = c("Tenure After","Tenure After",
                   "PW Surv.","PW Surv.",
                   "PW Years","PW Years",
                   "Rank Gain","Rank Gain"),
       se.below = TRUE, fitstat = ~n + r2)

# ---- Export LaTeX ----
extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else tex_raw
}

tex <- etable(b1, i1, b2, i2, b3, i3, b4, i4,
              dict = dict,
              drop = "log",
              headers = c("Tenure After","Tenure After",
                          "PW Surv.","PW Surv.",
                          "PW Years","PW Years",
                          "Rank Gain","Rank Gain"),
              se.below = TRUE, fitstat = ~n + r2 + G, tex = TRUE)

tc <- extract_tabular(tex)
tc <- tc[!grepl("Dependent Var", tc)]

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Career Dynamics of Transferees: Same- vs.\\ Different-Department Origin}",
  "\\label{tab:transfer-distdept}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  "\\item \\textit{Notes:} OLS. Sample: workers at destination offices that received a transferee in 1938--1945 (transferees plus incumbent peers). Odd columns show the baseline effect of being a transferee; even columns add an interaction with an indicator for whether the transferee arrived from a \\emph{different department (kyoku)}. A transfer is defined as a change of ka or kyoku across years. ``Diff.\\ Dept.'' is coded 0 for incumbent peers and for same-kyoku transferees. All specifications control for peer average rank, peer average tenure, peer female share, and log peer headcount, and include destination-kakari, year, and position fixed effects. Standard errors clustered at the destination-kakari level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)

writeLines(tex_out, here("TransferOutcomes_DistDept.tex"))
cat("\nExported Panel A to TransferOutcomes_DistDept.tex\n")

# ============================================================
# PANEL B: AMONG TRANSFEREES FROM THE SAME DONOR OFFICE,
#          DOES DESTINATION DISTANCE PREDICT CAREER OUTCOMES?
#
# Unit: individual transferred worker
# FE: donor_office x occupation + transfer_year
# Treatment: diff_dept (=1 if dest kyoku != origin kyoku)
# Controls: own_tenure, peer composition at donor office (pre-transfer)
# ============================================================

cat("\n========== PANEL B: TRANSFEREES ONLY, BY DESTINATION TYPE ==========\n\n")

# Peer composition at DONOR office in the year before transfer
donor_composition <- df %>%
  filter(year_num %in% (min(years_of_interest) - 1):max(years_of_interest), !is.na(ka)) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    donor_peer_headcount    = n(),
    donor_peer_avg_rank     = mean(pos_rank, na.rm = TRUE),
    donor_peer_female_share = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

donor_tenure <- df %>%
  filter(year_num %in% (min(years_of_interest) - 1):max(years_of_interest), !is.na(ka)) %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(tenure = year_num - first_yr) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(donor_peer_avg_tenure = mean(tenure, na.rm = TRUE), .groups = "drop")

donor_composition <- donor_composition %>%
  left_join(donor_tenure, by = c("office_id", "ka", "year_num"))

# Build transferee-level panel
transferee_panel <- worker_panel %>%
  filter(transferred == 1) %>%
  mutate(
    diff_dept = as.integer(lag_norm_kyoku != norm_kyoku),
    occupation = classify_occ(lag_pos_norm),
    donor_occ_id = paste(lag_office_id, occupation, sep = "_"),
    donor_year = transfer_year - 1
  ) %>%
  # Flag whether destination office was drafted that year
  left_join(office_drafts,
            by = c("office_id" = "office_id", "transfer_year" = "year_num")) %>%
  mutate(dest_drafted = replace_na(drafted_office_year, 0L)) %>%
  select(-drafted_office_year) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(
    own_tenure = transfer_year - first_yr,
    years_after = last_year - transfer_year,
    postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
    postwar_years = replace_na(postwar_years, 0),
    rank_gain = max_rank - pos_rank
  ) %>%
  left_join(donor_composition,
            by = c("lag_office_id" = "office_id", "lag_ka" = "ka",
                   "donor_year" = "year_num"))

dat_b <- transferee_panel %>%
  filter(!is.na(donor_occ_id), !is.na(donor_peer_avg_tenure))
dat_b_rg <- dat_b %>% filter(!is.na(rank_gain))

cat("Panel B (transferees only):", nrow(dat_b), "\n")
cat("  Same-dept:", sum(dat_b$diff_dept == 0), "\n")
cat("  Diff-dept:", sum(dat_b$diff_dept == 1), "\n")
cat("  Dest drafted:", sum(dat_b$dest_drafted == 1), "\n")
cat("  Dest not drafted:", sum(dat_b$dest_drafted == 0), "\n\n")

ctrls_b <- "donor_peer_avg_rank + donor_peer_avg_tenure + donor_peer_female_share + log(donor_peer_headcount + 1)"
fe_b    <- "donor_occ_id + transfer_year + first_yr"

# C1-C4: Destination drafted (vacancy from drafting vs other opening)
run_drafted <- function(y, d) {
  feols(as.formula(paste(y, "~ dest_drafted +", ctrls_b, "|", fe_b)),
        data = d, cluster = ~lag_office_id)
}
# C5-C8: Destination drafted + diff_dept interaction
run_drafted_intx <- function(y, d) {
  feols(as.formula(paste(y, "~ dest_drafted + dest_drafted:diff_dept +", ctrls_b, "|", fe_b)),
        data = d, cluster = ~lag_office_id)
}

bb1 <- run_drafted("years_after", dat_b)
bb2 <- run_drafted("postwar_survived", dat_b)
bb3 <- run_drafted("postwar_years", dat_b)
bb4 <- run_drafted("rank_gain", dat_b_rg)

bi1 <- run_drafted_intx("years_after", dat_b)
bi2 <- run_drafted_intx("postwar_survived", dat_b)
bi3 <- run_drafted_intx("postwar_years", dat_b)
bi4 <- run_drafted_intx("rank_gain", dat_b_rg)

dict_b <- c(
  dest_drafted                   = "Dest. drafted (=1)",
  diff_dept                      = "Diff. Dept. (=1)",
  "dest_drafted:diff_dept"       = "Dest. drafted $\\times$ Diff. Dept.",
  donor_peer_avg_rank            = "Donor peer avg rank",
  donor_peer_avg_tenure          = "Donor peer avg tenure",
  donor_peer_female_share        = "Donor peer female share"
)

etable(bb1, bi1, bb2, bi2, bb3, bi3, bb4, bi4,
       dict = dict_b,
       drop = "log",
       headers = c("Tenure After","Tenure After",
                    "PW Surv.","PW Surv.",
                    "PW Years","PW Years",
                    "Rank Gain","Rank Gain"),
       se.below = TRUE, fitstat = ~n + r2)

# ---- Export Panel B LaTeX ----
tex_b <- etable(bb1, bi1, bb2, bi2, bb3, bi3, bb4, bi4,
                dict = dict_b,
                drop = "log",
                headers = c("Tenure After","Tenure After",
                            "PW Surv.","PW Surv.",
                            "PW Years","PW Years",
                            "Rank Gain","Rank Gain"),
                se.below = TRUE, fitstat = ~n + r2 + G, tex = TRUE)

tc_b <- extract_tabular(tex_b)
tc_b <- tc_b[!grepl("Dependent Var", tc_b)]

tex_out_b <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Career Outcomes of Transferees by Destination Type (Donor-Office Comparison)}",
  "\\label{tab:transfer-dest-type}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  tc_b,
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS. Sample: transferred workers (changed ka or kyoku across years, 1938--1945). ",
    "Unit of observation: individual transferee. ",
    "``Dest.\\ drafted'' = 1 if the destination office had $\\geq$1 drafted worker that year. ",
    "Odd columns: baseline effect of being sent to a drafted vacancy. ",
    "Even columns: interaction with whether the destination was in a different department (kyoku). ",
    "Comparison: workers who left the \\emph{same donor office $\\times$ occupation} but went to different destinations. ",
    "All specifications control for donor-office peer composition (avg rank, avg tenure, female share, log headcount). ",
    "Fixed effects: donor-office $\\times$ occupation + transfer year + entry year. ",
    "Standard errors clustered at the donor-office level. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)

writeLines(tex_out_b, here("TransferOutcomes_DestDistance.tex"))
cat("\nExported Panel B to TransferOutcomes_DestDistance.tex\n")

# ============================================================
# PANEL C: DOES PRODUCTIVITY PREDICT TRANSFER DESTINATION?
#
# Among all workers who could be transferred, does prior-year
# rank (productivity proxy) predict being transferred to a
# DIFFERENT dept vs SAME dept?
#
# Unit: worker x year (all workers with prior-year record)
# Outcomes: transferred_diff_dept (=1), transferred_same_dept (=1)
# Treatment: lag_pos_rank (prior-year rank)
# FE: origin_office x occupation + year + entry_year
# ============================================================

cat("\n========== PANEL C: PRODUCTIVITY SELECTION INTO TRANSFER TYPE ==========\n\n")

# Build the full worker-year panel (all workers with lag, not just transferees)
selection_panel <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office_id), !is.na(lag_ka),
         !is.na(norm_kyoku), !is.na(lag_norm_kyoku)) %>%
  mutate(
    transferred = as.integer(lag_ka != ka | lag_norm_kyoku != norm_kyoku),
    diff_dept   = as.integer(lag_norm_kyoku != norm_kyoku),
    same_dept   = as.integer(transferred == 1 & diff_dept == 0),
    diff_dept_t = as.integer(transferred == 1 & diff_dept == 1),
    occupation  = classify_occ(lag_pos_norm),
    origin_ka_occ = paste(lag_norm_kyoku, lag_ka, occupation, sep = "_")
  ) %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(own_tenure = year_num - first_yr)

cat("Panel C sample:", nrow(selection_panel), "worker-year obs\n")
cat("  Stayed:", sum(selection_panel$transferred == 0, na.rm = TRUE), "\n")
cat("  Transferred same-dept:", sum(selection_panel$same_dept == 1, na.rm = TRUE), "\n")
cat("  Transferred diff-dept:", sum(selection_panel$diff_dept_t == 1, na.rm = TRUE), "\n\n")

sel_court <- selection_panel %>% filter(!is.na(lag_court_rank))

cat("  With court rank:", nrow(sel_court), "\n\n")

fe_c <- "origin_ka_occ + year_num + first_yr"

# C1: court rank -> transferred at all
c1 <- feols(as.formula(paste("transferred ~ lag_court_rank |", fe_c)),
            data = sel_court, cluster = ~lag_office_id)

# C2: court rank -> transferred to same dept
c2 <- feols(as.formula(paste("same_dept ~ lag_court_rank |", fe_c)),
            data = sel_court, cluster = ~lag_office_id)

# C3: court rank -> transferred to diff dept
c3 <- feols(as.formula(paste("diff_dept_t ~ lag_court_rank |", fe_c)),
            data = sel_court, cluster = ~lag_office_id)

# C4-C6: decoration rank
sel_decor <- selection_panel %>% filter(!is.na(lag_decor_rank))
cat("  With decoration:", nrow(sel_decor), "\n\n")

c4 <- feols(as.formula(paste("transferred ~ lag_decor_rank |", fe_c)),
            data = sel_decor, cluster = ~lag_office_id)

c5 <- feols(as.formula(paste("same_dept ~ lag_decor_rank |", fe_c)),
            data = sel_decor, cluster = ~lag_office_id)

c6 <- feols(as.formula(paste("diff_dept_t ~ lag_decor_rank |", fe_c)),
            data = sel_decor, cluster = ~lag_office_id)

dict_c <- c(
  lag_court_rank = "Court rank (1--8)",
  lag_decor_rank = "Decoration (1--8)"
)

etable(c1, c2, c3, c4, c5, c6,
       dict = dict_c,
       headers = c("Any Transfer", "Same Dept", "Diff Dept",
                    "Any Transfer", "Same Dept", "Diff Dept"),
       se.below = TRUE, fitstat = ~n + r2)

# ---- Export Panel C LaTeX ----
tex_c <- etable(c1, c2, c3, c4, c5, c6,
                dict = dict_c,
                headers = c("Any Transfer", "Same Dept", "Diff Dept",
                            "Any Transfer", "Same Dept", "Diff Dept"),
                se.below = TRUE, fitstat = ~n + r2 + G, tex = TRUE)

tc_c <- extract_tabular(tex_c)
tc_c <- tc_c[!grepl("Dependent Var", tc_c)]

tex_out_c <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Does Productivity Predict Transfer Destination?}",
  "\\label{tab:selection-productivity}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  tc_c,
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Linear probability models. ",
    "Sample: all worker $\\times$ year observations with prior-year records, 1938--1945. ",
    "Dependent variable in Columns (1) and (4): transferred to any office (=1). ",
    "Columns (2) and (5): transferred within same department/kyoku (=1). ",
    "Columns (3) and (6): transferred to a different department/kyoku (=1). ",
    "``Rank'' is the prior-year position rank (1--3 scale; higher = more senior). ",
    "Columns (4)--(6) add own tenure (years since first observed). ",
    "Fixed effects: origin-office $\\times$ occupation + year + entry year. ",
    "Standard errors clustered at the origin-office level. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)

writeLines(tex_out_c, here("TransferSelection_Productivity.tex"))
cat("\nExported Panel C to TransferSelection_Productivity.tex\n")
