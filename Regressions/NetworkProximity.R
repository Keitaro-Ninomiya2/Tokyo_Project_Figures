################################################################################
# Network Proximity: Past Co-Worker Connections and Transfer Probability
#
# For each worker transferred in year t, measure whether anyone in the DONOR
# office had previously worked with anyone in the DESTINATION office.
# This proxies for how "close" the transferred worker was to the destination
# office's manager/team via shared workplace history.
#
# Panel A: Descriptive stats on the network proximity measure
# Panel B: Regression: does past connection predict getting transferred?
#
# Unit: individual worker x year (workers at risk of being transferred)
# Outcome: transferred (=1 if worker moved to a different ka/kyoku in year t)
# Treatment: has_connection (=1 if anyone in worker's current office previously
#            worked with anyone at the destination office)
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

# ============================================================
# KYOKU NORMALIZATION (same as other scripts)
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

df <- df %>% mutate(norm_kyoku = normalize_kyoku(kyoku))

# ============================================================
# 1. BUILD FULL WORK HISTORY NETWORK
# For each pair of workers, record if they ever shared an
# (office_id, ka) in any prior year
# ============================================================

cat("Building workplace co-occurrence network...\n")

# Create lagged location for each worker
staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, norm_kyoku) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_kyoku = norm_kyoku) %>%
  mutate(year_num = year_num + 1)

# Identify transfers: worker moved to different ka or kyoku
transfers <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office), !is.na(lag_ka), !is.na(norm_kyoku), !is.na(lag_kyoku)) %>%
  mutate(
    transferred = as.integer(lag_ka != ka | lag_kyoku != norm_kyoku)
  )

cat("Workers with lag info:", nrow(transfers), "\n")
cat("Transferred:", sum(transfers$transferred), "\n")
cat("Not transferred:", sum(transfers$transferred == 0), "\n")

# ============================================================
# 2. FOR EACH TRANSFER, CHECK IF ANYONE IN THE DONOR OFFICE
#    PREVIOUSLY WORKED WITH ANYONE IN THE DESTINATION OFFICE
# ============================================================

# Build a lookup: for each (office_id, ka, year), who worked there?
office_roster <- df %>%
  filter(!is.na(ka)) %>%
  select(staff_id, office_id, ka, year_num) %>%
  distinct()

# For each year t, build the set of past co-workers:
# Two workers are "connected" if they shared an (office_id, ka) in any year < t

compute_connections <- function(t) {
  cat(sprintf("Processing year %d...\n", t))

  # Workers who transferred in year t
  transfers_t <- transfers %>%
    filter(year_num == t, transferred == 1)

  if (nrow(transfers_t) == 0) return(tibble())

  # All workers and their locations BEFORE year t
  past_locations <- office_roster %>%
    filter(year_num < t)

  # People in each destination office in year t
  dest_rosters <- df %>%
    filter(year_num == t, !is.na(ka)) %>%
    select(dest_staff = staff_id, dest_office = office_id, dest_ka = ka) %>%
    distinct()

  # People in each donor office at t-1
  donor_rosters <- df %>%
    filter(year_num == t - 1, !is.na(ka)) %>%
    select(donor_staff = staff_id, donor_office = office_id, donor_ka = ka) %>%
    distinct()

  relevant_workers <- unique(c(donor_rosters$donor_staff, dest_rosters$dest_staff))

  past_relevant <- past_locations %>%
    filter(staff_id %in% relevant_workers) %>%
    select(staff_id, office_id, ka) %>%
    distinct()

  # Self-join to find co-worker pairs
  coworker_pairs <- past_relevant %>%
    inner_join(past_relevant, by = c("office_id", "ka"),
               relationship = "many-to-many") %>%
    filter(staff_id.x != staff_id.y) %>%
    select(worker_a = staff_id.x, worker_b = staff_id.y) %>%
    distinct()

  # For each transferred worker, check if any donor colleague is connected to any dest member
  # Use a vectorized approach: join transfers_t with donor/dest rosters, then check coworker_pairs
  transfer_donor <- transfers_t %>%
    select(staff_id, lag_office, lag_ka, office_id, ka) %>%
    left_join(donor_rosters, by = c("lag_office" = "donor_office", "lag_ka" = "donor_ka")) %>%
    filter(donor_staff != staff_id) %>%
    rename(donor_colleague = donor_staff)

  transfer_dest <- transfers_t %>%
    select(staff_id, office_id, ka) %>%
    left_join(dest_rosters, by = c("office_id" = "dest_office", "ka" = "dest_ka")) %>%
    rename(dest_member = dest_staff)

  # For each transferred worker, check if any (donor_colleague, dest_member) pair
  # exists in coworker_pairs
  connected_transfers <- transfer_donor %>%
    inner_join(transfer_dest %>% select(staff_id, dest_member),
               by = "staff_id", relationship = "many-to-many") %>%
    semi_join(coworker_pairs, by = c("donor_colleague" = "worker_a", "dest_member" = "worker_b")) %>%
    distinct(staff_id) %>%
    pull(staff_id)

  # Also check reverse direction
  connected_transfers2 <- transfer_donor %>%
    inner_join(transfer_dest %>% select(staff_id, dest_member),
               by = "staff_id", relationship = "many-to-many") %>%
    semi_join(coworker_pairs, by = c("dest_member" = "worker_a", "donor_colleague" = "worker_b")) %>%
    distinct(staff_id) %>%
    pull(staff_id)

  all_connected <- unique(c(connected_transfers, connected_transfers2))

  transfers_t %>%
    select(staff_id, year_num, office_id, ka, lag_office, lag_ka, norm_kyoku, lag_kyoku,
           is_female, pos_norm) %>%
    mutate(has_past_connection = as.integer(staff_id %in% all_connected))
}

# Run for each year
transferred_with_connections <- map_dfr(years_of_interest, compute_connections)

cat("\n=== NETWORK PROXIMITY DESCRIPTIVE STATS ===\n")
cat("Total transfers:", nrow(transferred_with_connections), "\n")
cat("With past connection:", sum(transferred_with_connections$has_past_connection), "\n")
cat("Without:", sum(!transferred_with_connections$has_past_connection), "\n")
cat("Share with connection:",
    round(mean(transferred_with_connections$has_past_connection), 4), "\n")

# ============================================================
# 3. NOW BUILD THE FULL PANEL: ALL WORKERS AT RISK OF TRANSFER
#    Test if having a connection to a drafted office predicts transfer
# ============================================================

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Draft counts by office x year
office_drafts <- df_all %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(n_drafted = sum(drafted == TRUE, na.rm = TRUE),
            n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Offices that had drafts
drafted_offices <- office_drafts %>%
  filter(n_drafted_male > 0) %>%
  select(office_id, ka, year_num)

compute_transfer_risk <- function(t) {
  cat(sprintf("Building transfer risk panel for year %d...\n", t))

  workers_prev <- df %>%
    filter(year_num == t - 1, !is.na(ka), !is.na(norm_kyoku)) %>%
    select(staff_id, office_id, ka, pos_norm, is_female, norm_kyoku)

  workers_curr2 <- df %>%
    filter(year_num == t, !is.na(ka)) %>%
    select(staff_id, office_id_t = office_id, ka_t = ka, kyoku_t = kyoku) %>%
    mutate(norm_kyoku_t = normalize_kyoku(kyoku_t))

  panel_t <- workers_prev %>%
    left_join(workers_curr2, by = "staff_id") %>%
    mutate(
      transferred = case_when(
        is.na(office_id_t) ~ 0L,
        ka != ka_t | norm_kyoku != norm_kyoku_t ~ 1L,
        TRUE ~ 0L
      ),
      year_num = as.integer(t)
    )

  past_locations <- office_roster %>% filter(year_num < t)
  drafted_offices_t <- drafted_offices %>% filter(year_num == t)

  if (nrow(drafted_offices_t) == 0) {
    panel_t$has_connection_to_drafted <- 0L
    panel_t$n_connections_to_drafted <- 0L
    panel_t$self_connected <- 0L
    return(panel_t %>% select(staff_id, year_num, office_id, ka, pos_norm,
                               is_female, norm_kyoku, transferred,
                               has_connection_to_drafted, n_connections_to_drafted,
                               self_connected))
  }

  dest_workers <- df %>%
    filter(year_num == t, !is.na(ka)) %>%
    semi_join(drafted_offices_t, by = c("office_id", "ka")) %>%
    select(staff_id, office_id, ka) %>%
    distinct()

  dest_past <- past_locations %>%
    filter(staff_id %in% dest_workers$staff_id) %>%
    select(staff_id, past_office = office_id, past_ka = ka) %>%
    distinct()

  all_past <- past_locations %>%
    filter(staff_id %in% workers_prev$staff_id) %>%
    select(staff_id, past_office = office_id, past_ka = ka) %>%
    distinct()

  connected_workers <- all_past %>%
    inner_join(dest_past %>% rename(dest_staff = staff_id),
               by = c("past_office", "past_ka")) %>%
    filter(staff_id != dest_staff) %>%
    distinct(staff_id) %>%
    pull(staff_id)

  connected_offices <- workers_prev %>%
    filter(staff_id %in% connected_workers) %>%
    distinct(office_id, ka) %>%
    mutate(office_has_connected_worker = 1L)

  n_connected_per_office <- workers_prev %>%
    mutate(is_connected = as.integer(staff_id %in% connected_workers)) %>%
    group_by(office_id, ka) %>%
    summarise(n_connected_colleagues = sum(is_connected), .groups = "drop")

  panel_t <- panel_t %>%
    left_join(connected_offices, by = c("office_id", "ka")) %>%
    left_join(n_connected_per_office, by = c("office_id", "ka")) %>%
    mutate(
      has_connection_to_drafted = replace_na(office_has_connected_worker, 0L),
      n_connections_to_drafted  = replace_na(n_connected_colleagues, 0L),
      self_connected = as.integer(staff_id %in% connected_workers)
    )

  panel_t %>% select(staff_id, year_num, office_id, ka, pos_norm,
                      is_female, norm_kyoku, transferred,
                      has_connection_to_drafted, n_connections_to_drafted,
                      self_connected)
}

transfer_risk_panel <- map_dfr(years_of_interest, compute_transfer_risk)

# ============================================================
# 4. REGRESSIONS
# ============================================================

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

reg_panel <- transfer_risk_panel %>%
  filter(!is.na(ka), !is.na(norm_kyoku)) %>%
  mutate(
    ka_id = paste(norm_kyoku, ka, sep = "_"),
    pos_rank = assign_rank(pos_norm, year_num),
    is_rank1 = as.integer(pos_rank == 1),
    is_rank3 = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

cat("\n=== PANEL A: DESCRIPTIVE STATS ON NETWORK MEASURE ===\n")
cat("Total worker-years:", nrow(reg_panel), "\n")
cat("Transferred:", sum(reg_panel$transferred), "\n")
cat("Has connection to drafted office:", sum(reg_panel$has_connection_to_drafted), "\n")
cat("Self connected:", sum(reg_panel$self_connected), "\n")
cat("Share with connection:", round(mean(reg_panel$has_connection_to_drafted), 4), "\n")
cat("Share self connected:", round(mean(reg_panel$self_connected), 4), "\n")
cat("\nAmong transferred:\n")
cat("  Share with connection:", round(mean(reg_panel$transferred == 1 & reg_panel$has_connection_to_drafted == 1) / mean(reg_panel$transferred == 1), 4), "\n")
cat("  Share self connected:", round(mean(reg_panel$transferred == 1 & reg_panel$self_connected == 1) / mean(reg_panel$transferred == 1), 4), "\n")
cat("\nAmong NOT transferred:\n")
cat("  Share with connection:", round(mean(reg_panel$transferred == 0 & reg_panel$has_connection_to_drafted == 1) / mean(reg_panel$transferred == 0), 4), "\n")

# Informativeness check: what fraction has non-zero connections?
cat("\n=== INFORMATIVENESS CHECK ===\n")
cat("Workers with has_connection_to_drafted > 0:", sum(reg_panel$has_connection_to_drafted > 0),
    "(", round(mean(reg_panel$has_connection_to_drafted > 0) * 100, 1), "%)\n")
cat("Workers with self_connected > 0:", sum(reg_panel$self_connected > 0),
    "(", round(mean(reg_panel$self_connected > 0) * 100, 1), "%)\n")
cat("Workers with n_connections > 0:", sum(reg_panel$n_connections_to_drafted > 0),
    "(", round(mean(reg_panel$n_connections_to_drafted > 0) * 100, 1), "%)\n")
cat("Mean n_connections:", round(mean(reg_panel$n_connections_to_drafted), 3), "\n")
cat("SD n_connections:", round(sd(reg_panel$n_connections_to_drafted), 3), "\n")
cat("Max n_connections:", max(reg_panel$n_connections_to_drafted), "\n")

cat("\n=== PANEL B: REGRESSIONS ===\n")

# C1: Does having a connection to a drafted office predict transfer?
m1 <- feols(transferred ~ has_connection_to_drafted |
              year_num + ka_id + pos_norm,
            data = reg_panel, cluster = ~office_id)

# C2: Self connection (worker themselves previously worked with dest office workers)
m2 <- feols(transferred ~ self_connected |
              year_num + ka_id + pos_norm,
            data = reg_panel, cluster = ~office_id)

# C3: Both measures
m3 <- feols(transferred ~ has_connection_to_drafted + self_connected |
              year_num + ka_id + pos_norm,
            data = reg_panel, cluster = ~office_id)

# C4: Intensive margin (number of connected colleagues)
m4 <- feols(transferred ~ n_connections_to_drafted |
              year_num + ka_id + pos_norm,
            data = reg_panel, cluster = ~office_id)

# C5: x Gender interaction
m5 <- feols(transferred ~ self_connected * is_female |
              year_num + ka_id + pos_norm,
            data = reg_panel, cluster = ~office_id)

cat("\n===== TABLE: NETWORK PROXIMITY AND TRANSFER =====\n\n")
etable(m1, m2, m3, m4, m5,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Office Connected", "Self Connected", "Both",
                    "N Connections", "Self x Gender"))

# ============================================================
# 5. EXPORT LaTeX TABLE
# ============================================================

dict <- c(
  has_connection_to_drafted   = "Office has connection to drafted dest.",
  self_connected              = "Self connected to dest. office",
  n_connections_to_drafted    = "No. connected colleagues",
  "is_femaleTRUE"             = "Female",
  "self_connected:is_femaleTRUE" = "Self connected $\\times$ Female"
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

clean_depvar <- function(tex_content) {
  drop_idx <- grep("\\btransferred\\b", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  tex_content
}

tex <- etable(m1, m2, m3, m4, m5,
              dict = dict,
              order = c("Office has", "Self connected", "No\\. connected", "Female"),
              headers = c("Office", "Self", "Both", "Intensive", "$\\times$ Gender"),
              se.below = TRUE, fitstat = ~n,
              tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_line <- "\\multicolumn{6}{l}{\\textit{Dep.\\ var: Transferred (=1 if worker moved to different ka/kyoku in year $t$)}} \\\\"
tc <- append(tc, depvar_line, after = midrule_idx)

# Add informativeness panel
info_lines <- c(
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel A: Informativeness of Network Measure}} \\\\",
  "\\midrule",
  sprintf("Share with any connection & \\multicolumn{5}{c}{%s} \\\\",
          round(mean(reg_panel$has_connection_to_drafted > 0), 3)),
  sprintf("Share self-connected & \\multicolumn{5}{c}{%s} \\\\",
          round(mean(reg_panel$self_connected > 0), 3)),
  sprintf("Mean connections & \\multicolumn{5}{c}{%s} \\\\",
          round(mean(reg_panel$n_connections_to_drafted), 3))
)

# Insert before the last \bottomrule
bottomrule_idx <- grep("\\\\bottomrule", tc)
tc <- append(tc, info_lines, after = bottomrule_idx[length(bottomrule_idx)] - 1)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Network Proximity and Transfer Probability}",
  "\\label{tab:network}",
  "\\small",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS linear probability model. ",
    "Dependent variable: transferred (=1 if worker moved to a different ka or kyoku in year $t$). ",
    "Sample: all workers observed in year $t{-}1$ (1938--1945). ",
    "``Office has connection'' = 1 if any colleague in the worker's office at $t{-}1$ ",
    "previously shared a workplace (office $\\times$ ka) with anyone currently at an office ",
    "that experienced military drafts in year $t$. ",
    "``Self connected'' = 1 if the worker themselves had such a past workplace link. ",
    "Panel~A reports the share of workers for whom each measure is non-zero. ",
    "All specifications include year, ka, and position fixed effects. ",
    "Standard errors clustered at the office level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NetworkProximity.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NetworkProximity.tex"))
cat("\nTable exported to NetworkProximity.tex\n")
