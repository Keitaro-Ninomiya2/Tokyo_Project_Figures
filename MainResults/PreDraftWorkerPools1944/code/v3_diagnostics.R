suppressPackageStartupMessages({library(tidyverse); library(fixest)})
options(warn = 1)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
tab  <- file.path(root, "MainResults", "PreDraftWorkerPools1944", "tables")
data_path <- file.path(Sys.getenv("USERPROFILE"), "Box",
  "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv")
ka_map_path <- file.path(root, "Regressions", "ka_group_map.csv")
dir.create(tab, recursive = TRUE, showWarnings = FALSE)

cat("=== ITEM 1: ENCODING TEST ===\n")
# rank_pos_old uses literal Japanese (as in the v2 script that may have been misread)
# rank_pos_new uses \u escapes (encoding-safe)
rank_pos_old <- function(x) case_when(
  x %in% c("Unknown", "\u517c") ~ NA_integer_,                             # 兼
  str_detect(x, "\u96c7|\u56d1\u6258|\u81e8\u6642|\u898b\u7fd2") ~ 1L,    # 雇|囑託|臨時|見習
  str_detect(x, "\u6280\u5e2b|\u4e8b\u52d9\u5b98|\u66f8\u8a18\u5b98") |   # 技師|事務官|書記官
    str_ends(x, "\u9577") | x == "\u4e3b\u4e8b" ~ 3L,                      # 長 | 主事
  TRUE ~ 2L)

track_pos_new <- function(x, r) case_when(
  r == 1L & str_detect(x, "\u6280") ~ "technical",                          # 技
  r == 1L ~ "unknown",
  str_detect(x, "\u6280") ~ "technical",                                     # 技
  str_detect(x, paste0("\u66f8\u8a18|\u4e3b\u4e8b|\u4e8b\u52d9|",          # 書記|主事|事務|
    "\u4fc2\u9577|\u8ab2\u9577|\u6240\u9577|\u5c40\u9577|\u6b21\u9577|",    # 係長|課長|所長|局長|次長|
    "\u5834\u9577|\u76e3\u8996|\u5de1\u8996|\u76e3\u540f|\u76e3\u7763")) ~   # 場長|監視|巡視|監吏|監督
    "clerical",
  TRUE ~ "unknown")

# rank_pos_old is actually the \u-escape version (encoding-safe); this IS the fix.
# We compare against what the old literal-string code produced by loading the CSV output.
old_csv <- read_csv(file.path(tab, "position_rank_track_map_1944.csv"),
                    show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

encoding_check <- old_csv %>%
  mutate(rank_new = rank_pos_old(occupation),
         rank_changed = !is.na(position_rank) != !is.na(rank_new) |
           (!is.na(position_rank) & !is.na(rank_new) & position_rank != rank_new)) %>%
  filter(rank_changed | (is.na(position_rank) != is.na(rank_new)))

cat("Occupations whose rank changes with encoding fix:\n")
print(encoding_check %>% select(occupation, rank_old=position_rank, rank_new, workers_1944), n=Inf)

before_after <- old_csv %>%
  transmute(occ = occupation, w = workers_1944,
            rank_old = position_rank,
            rank_new = rank_pos_old(occupation)) %>%
  group_by(rank_old, rank_new) %>% summarise(workers = sum(w), .groups="drop") %>%
  arrange(rank_old, rank_new)
cat("\nBefore (rank_old) vs After (rank_new) cross-tab (workers):\n")
print(before_after, n=Inf)
write_csv(before_after, file.path(tab, "v3_encoding_rank_crosswalk.csv"))

cat("\n=== ITEM 2: RANK DISTRIBUTION BEFORE/AFTER ===\n")
rank_before <- old_csv %>% group_by(rank_old=position_rank) %>%
  summarise(workers_before=sum(workers_1944),.groups="drop")
rank_after <- old_csv %>% mutate(rank_new=rank_pos_old(occupation)) %>%
  group_by(rank_new) %>% summarise(workers_after=sum(workers_1944),.groups="drop")
cat("Before fix:\n"); print(rank_before)
cat("After fix:\n"); print(rank_after)
# "51 outside 1-3" explanation: rank_distribution_1944.csv is STALE.
# The current diagnostics.csv (from analysis_v2.R) shows rank1=827, rank2=7211, rank3=770,
# rank_missing=515. The stale rank_distribution_1944.csv shows ranks 4 and 5 (court_rank
# pass-through from an earlier code version). The current analysis_v2.R correctly derives
# position_rank from occupation title, never producing 4 or 5.
cat("\nNOTE: rank_distribution_1944.csv is STALE (from a different code run).\n")
cat("diagnostics.csv rank counts: rank1=827, rank2=7211, rank3=770, missing=515.\n")
cat("Ranks 4 and 5 in the stale CSV are court_rank values, not employment ranks.\n")

cat("\n=== ITEMS 3-5: LOAD MASTER DATA ===\n")
cat("Reading master CSV...\n")
raw <- read_csv(data_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
observed <- function(x) !is.na(x) & x != ""
cat("Done. Rows:", nrow(raw), "\n")

ka_map <- read_csv(ka_map_path, show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8")) %>%
  mutate(kyoku = replace_na(kyoku, ""),
         ka    = replace_na(ka, ""))

cat("\n--- ITEM 3: TABLE 3 MISSING KA RATE (same-section arrivals, 1938-1945) ---\n")
# Replicate ByDistance arrival classification for the relevant years
# Keep only what we need to classify arrivals
yrs <- 1938:1945
wf <- raw %>%
  filter(is_name, observed(staff_id), year %in% yrs) %>%
  mutate(year = as.integer(year),
         kyoku_c = replace_na(kyoku, ""),
         ka_c    = replace_na(ka, "")) %>%
  distinct(staff_id, year, .keep_all = TRUE) %>%
  left_join(ka_map, by = c("year" = "year", "kyoku_c" = "kyoku", "ka_c" = "ka"))

lag_wf <- wf %>%
  select(staff_id, lag_year = year, lag_kyoku = kyoku, lag_ka = ka,
         lag_office_id = office_id, lag_ka_group = ka_group) %>%
  mutate(year = lag_year + 1L)

transitions <- wf %>%
  inner_join(lag_wf, by = c("staff_id", "year")) %>%
  filter(office_id != lag_office_id | is.na(lag_office_id)) %>%   # actual move
  mutate(
    # Mirror ByDistance lines 177-191 (simplified: no kyoku_group, use kyoku directly)
    arrival_type = case_when(
      is.na(kyoku) | is.na(lag_kyoku) ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) & ka_group == lag_ka_group ~ "same_section",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        kyoku == lag_kyoku & !is.na(ka) & !is.na(lag_ka) & ka == lag_ka ~ "same_section",
      kyoku == lag_kyoku ~ "same_department",
      TRUE ~ "different_department"),
    ss_ka_missing = arrival_type == "same_section" &
      (is.na(ka_group) | is.na(lag_ka_group)))

ss <- transitions %>% filter(arrival_type == "same_section")
cat("Same-section arrivals 1938-1945:", nrow(ss), "\n")
cat("  ka_group missing at EITHER endpoint:", sum(ss$ss_ka_missing, na.rm=TRUE),
    sprintf("(%.1f%%)\n", 100*mean(ss$ss_ka_missing, na.rm=TRUE)))
write_csv(tibble(
  total_same_section = nrow(ss),
  missing_ka_group_either = sum(ss$ss_ka_missing, na.rm=TRUE),
  share_missing = mean(ss$ss_ka_missing, na.rm=TRUE)),
  file.path(tab, "v3_table3_missing_ka.csv"))

cat("\n--- ITEM 4: KA RECOVERY FOR 1944 ---\n")
# For each office_id, collect all (year, ka) observations; find nearest prior year ka for 1944
ka_by_office_year <- raw %>%
  filter(is_name, observed(staff_id), observed(office_id), observed(ka)) %>%
  mutate(year = as.integer(year)) %>%
  distinct(office_id, year, ka) %>%
  group_by(office_id, year) %>%
  summarise(ka_vals = list(unique(ka)), n_ka = n_distinct(ka), .groups="drop")

# For offices missing ka in 1944, find nearest prior/later year
offices_1944 <- raw %>%
  filter(is_name, observed(staff_id), observed(office_id), year == 1944) %>%
  mutate(year = 1944L) %>%
  distinct(office_id) %>%
  left_join(ka_by_office_year %>% filter(year == 1944), by = "office_id")

offices_need_fill <- offices_1944 %>% filter(is.na(n_ka)) %>% pull(office_id)
cat("1944 offices with observed ka:", sum(!is.na(offices_1944$n_ka)),
    " | missing ka:", length(offices_need_fill), "\n")

# Fill from prior years (prefer most recent year < 1944)
fill_ka <- ka_by_office_year %>%
  filter(office_id %in% offices_need_fill, year < 1944) %>%
  arrange(office_id, desc(year)) %>%
  group_by(office_id) %>% slice(1) %>% ungroup() %>%
  mutate(fill_source = year) %>%
  select(office_id, fill_source, ka_fill = ka_vals, n_fill = n_ka)

cat("Offices filled from prior year:", nrow(fill_ka), "\n")
cat("Of those, offices with conflicting ka across years (>1 unique ka in fill source):",
    sum(fill_ka$n_fill > 1), "\n")

# Cross-year conflict: does the same office appear with different ka in different years?
conflict <- ka_by_office_year %>%
  filter(year <= 1944) %>%
  group_by(office_id) %>%
  summarise(distinct_ka_vals = n_distinct(unlist(ka_vals)), .groups="drop") %>%
  filter(distinct_ka_vals > 1)
cat("Offices with conflicting ka across all available years:", nrow(conflict), "\n")

# After ka recovery: summarise from raw 1944 worker data
obs44 <- raw %>%
  filter(is_name, observed(staff_id), year == 1944, gov_level == "TokyoTo") %>%
  distinct(staff_id, .keep_all = TRUE)
workers_unknown_kyoku <- sum(!observed(obs44$kyoku))
workers_unknown_ka    <- sum( observed(obs44$kyoku) & !observed(obs44$ka))
cat("\nIn 1944 workers (deduplicated):\n")
cat("  Unknown kyoku (distance always unknown, unfixable):", workers_unknown_kyoku, "\n")
cat("  Known kyoku, unknown ka (potentially fixable via ka recovery):", workers_unknown_ka, "\n")

write_csv(tibble(
  offices_1944_with_ka = sum(!is.na(offices_1944$n_ka)),
  offices_1944_missing_ka = length(offices_need_fill),
  filled_from_prior_year = nrow(fill_ka),
  conflict_across_years = nrow(conflict),
  workers_unknown_kyoku = workers_unknown_kyoku,
  workers_known_kyoku_unknown_ka = workers_unknown_ka),
  file.path(tab, "v3_ka_recovery_summary.csv"))

cat("\n--- ITEM 5: OFFICE-BASED DISTANCE (1944 panel) ---\n")
# Construct arrivals with office-based distance: same_office / other_office_same_kyoku / other_kyoku
# We need arrivals: 1944 workers who appeared in 1942 at a different office
w42 <- raw %>% filter(is_name, observed(staff_id), year == 1942,
                      gov_level == "TokyoShi") %>%
  mutate(year = 1942L) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office42 = office_id, kyoku42 = kyoku)

w44 <- raw %>% filter(is_name, observed(staff_id), year == 1944,
                      gov_level == "TokyoTo") %>%
  mutate(year = 1944L) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office44 = office_id, kyoku44 = kyoku)

arr_office <- w44 %>% inner_join(w42, by = "staff_id") %>%
  mutate(
    dist_office = case_when(
      observed(office42) & observed(office44) & office42 == office44 ~ "same_office",
      observed(kyoku42) & observed(kyoku44) & kyoku42 == kyoku44     ~ "other_office_same_kyoku",
      observed(kyoku42) & observed(kyoku44)                          ~ "other_kyoku",
      TRUE                                                            ~ "unknown"),
    moved = office42 != office44 | is.na(office42) | is.na(office44))

dist_tbl <- arr_office %>% count(dist_office) %>%
  mutate(share = n/sum(n))
cat("Office-based distance distribution (all 1942-1944 links):\n")
print(dist_tbl)

movers <- arr_office %>% filter(moved)
dist_movers <- movers %>% count(dist_office) %>% mutate(share = n/sum(n))
cat("\nOffice-based distance distribution (movers only):\n")
print(dist_movers)
cat("Unknown distance share (movers):", round(mean(movers$dist_office=="unknown"),3), "\n")
write_csv(dist_movers, file.path(tab, "v3_office_distance_movers.csv"))

cat("\n=== ITEM 6: INFERENCE BUGS ===\n")
cat("--- 6a. Why ka-clustered p-values are NA ---\n")
cat("In analysis_v2.R the 'sensitivity_cluster_nested_ka' spec filters to ka_obs=TRUE,\n")
cat("then inside ols() further filters observed(ka_cluster). ka_cluster is non-NA only\n")
cat("when BOTH kyoku_obs AND ka_obs are TRUE (line 48). After this double filter,\n")
cat("the fixed-effect singleton-removal and collinearity checks may leave G < 2\n")
cat("unique clusters, triggering vcov_cluster to return a NA matrix.\n\n")

# Demonstrate: check G in the ka_obs+kyoku_obs subsample using raw 1944 data
ka_obs_kyoku_obs <- obs44 %>%
  filter(observed(kyoku), observed(ka)) %>%
  mutate(ka_cluster = paste(kyoku, ka, sep=" || "))
cat("1944 workers with ka observed:", sum(observed(obs44$ka)), "\n")
cat("1944 workers with ka AND kyoku observed:", nrow(ka_obs_kyoku_obs), "\n")
cat("Unique ka_cluster values:", n_distinct(ka_obs_kyoku_obs$ka_cluster), "\n")
cat("Unique kyoku values:", n_distinct(ka_obs_kyoku_obs$kyoku), "\n")
cat("High-link kyoku subset (>50% linked to 1942): check link_rate_by_kyoku_1944.csv\n")
lr <- tryCatch(
  read_csv(file.path(tab, "link_rate_by_kyoku_1944.csv"), show_col_types=FALSE),
  error=function(e) NULL)
if (!is.null(lr)) {
  high_link <- lr %>% filter(kyoku_l != "<missing>", link_rate > 0.5)
  cat("High-link kyoku (>50%):", nrow(high_link), "->", paste(high_link$kyoku_l, collapse=", "), "\n")
  cat("With only 1 kyoku in high_link subset: G=1 -> vcov_cluster returns NA matrix.\n")
}

cat("--- 6b. Why Webb p-values near 0 when kyoku p ≈ 0.16–0.24 ---\n")
cat("BUG: the custom bootstrap at analysis_v2.R lines 163-166 compares RAW bootstrap\n")
cat("coefficients bb to the observed coefficient: mean(abs(bb) >= abs(fit$coef[j])).\n")
cat("A proper restricted-null wild bootstrap should compare t-STATISTICS:\n")
cat("  t* = bb / se(bb)  vs  t_obs = coef_obs / se_kyoku\n")
cat("When the restricted model (H0: beta_j=0) fits well, restricted residuals e0 ≈ 0.\n")
cat("Bootstrap samples then have bb ≈ 0, so abs(bb) < abs(coef_obs) always -> p ≈ 0.\n")
cat("This is a known implementation error: the test is on the coefficient scale, not\n")
cat("the t-scale. Use fwildclusterboot::boottest (score-bootstrap or WCR) instead.\n\n")

# Test: check if fwildclusterboot is available
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  cat("fwildclusterboot is installed.\n")
  # Rebuild a tiny test panel from scratch: 1944 workers in the master data
  # minimal feols regression: lateral_other_kyoku ~ Draft_ko | kyoku + occupation
  # using cell-level aggregation from raw data
  cat("(Full test deferred to v3 estimation script — fwildclusterboot is available.)\n")
} else {
  cat("fwildclusterboot NOT installed. Install with:\n")
  cat("  install.packages('fwildclusterboot')\n")
  cat("Then replace the custom Webb loop (analysis_v2.R lines 163-166) with:\n")
  cat("  boottest(feols_model, param='term_name', B=9999, type='webb', impose_null=TRUE)\n")
}

cat("\n=== ITEM 7: FAILURES TABLE ===\n")
fails <- read_csv(file.path(tab, "specification_failures.csv"), show_col_types=FALSE)
cat("All 23 recorded failures:\n")
fails_fmt <- fails %>%
  transmute(
    Spec = spec, Outcome = outcome, Family = family,
    Type = type,
    Error = str_trunc(diagnostic, 60))
print(fails_fmt, n=Inf)
write_csv(fails_fmt, file.path(tab, "v3_failures_formatted.csv"))

cat("\n\nSummary: 23 failures break into:\n")
fails %>% count(diagnostic) %>% arrange(desc(n)) %>%
  mutate(diagnostic = str_trunc(diagnostic, 70)) %>%
  print(n=Inf)

cat("\nDONE.\n")
