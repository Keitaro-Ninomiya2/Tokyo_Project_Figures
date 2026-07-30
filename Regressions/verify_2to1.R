suppressMessages({
  library(tidyverse)
  library(fixest)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

cat("Loading data...\n")
df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

df <- df_raw %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- df_raw %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "社会") ~ "社会局",
    str_detect(k, "保健") ~ "保健局",
    str_detect(k, "上木|土木") ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務") ~ "府会事務局",
    str_detect(k, "發育|教育") ~ "教育局",
    str_detect(k, "水違|水道") ~ "水道局",
    str_detect(k, "電気") ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務") ~ "財務局",
    str_detect(k, "繰済|發経済|経済") ~ "経済局",
    str_detect(k, "厚生") ~ "厚生局",
    str_detect(k, "市民") ~ "市民局",
    str_detect(k, "中央卸売") ~ "中央卸売市場",
    str_detect(k, "港準|港湾") ~ "港湾局",
    str_detect(k, "監査") ~ "監査局",
    str_detect(k, "産業") ~ "産業局",
    str_detect(k, "城東病院|健民") ~ "健民局",
    str_detect(k, "戰時生活") ~ "戰時生活局",
    str_detect(k, "経〓|経理") ~ "経理局",
    str_detect(k, "労働") ~ "労働局",
    str_detect(k, "建築|御築") ~ "建築局",
    str_detect(k, "道路") ~ "道路局",
    str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通") ~ "交通局",
    str_detect(k, "計[晝画]") ~ "計画局",
    str_detect(k, "民局") ~ "健民局",
    str_detect(k, "後醍院") ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛") ~ "防衛局",
    str_detect(k, "民生") ~ "民生局",
    str_detect(k, "長官官房") ~ "長官官房",
    TRUE ~ NA_character_
  )
}

assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("電気局", "交通局") ~ "transport",
    nk == "水道局" ~ "water",
    nk == "港湾局" ~ "port",
    nk == "教育局" ~ "education",
    nk %in% c("土木局", "計画局", "経理局", "建築局", "道路局") ~ "infrastructure",
    nk %in% c("戰時生活局", "経済局", "中央卸売市場", "産業局") ~ "economy",
    nk %in% c("健民局", "厚生局", "社会局", "保健局", "民生局", "養育院", "労働局") ~ "welfare",
    nk %in% c("市会事務局", "府会事務局") ~ "assembly",
    nk %in% c("財務局", "監査局", "長官官房") ~ "finance_admin",
    nk == "復興事業局" ~ "reconstruction",
    nk == "市民局" ~ "citizen",
    nk == "防衛局" ~ "defense",
    TRUE ~ NA_character_
  )
}

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$") ~ 1L,
    TRUE ~ 2L
  )
}

df <- df %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku))

# Cumulative male baseline
cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# First year per office and staff (needed for new hire definition)
office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

# Lag kyoku group for transfer classification
staff_lag <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
         lag_kyoku_group = kyoku_group) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    # New hire: exclude the office's first year (Table7 convention)
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    # Transfer in: cross-bureau (kyoku_group changed)
    is_transfer_in = !is.na(lag_kyoku_group) & !is.na(kyoku_group) &
                     (lag_kyoku_group != kyoku_group),
    pos_rank    = assign_rank(pos_norm),
    is_rank1    = as.integer(pos_rank == 1),
    is_rank3    = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_transfers_in = sum(is_transfer_in, na.rm = TRUE),
    n_new_hires    = sum(is_new_hire, na.rm = TRUE),
    n_workers      = n(),
    n_female       = sum(is_female, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(female_share = n_female / pmax(n_workers, 1))

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(n_drafted = n(), n_drafted_male = sum(!is_female, na.rm = TRUE), .groups = "drop")

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
cat("Panel obs (with ka_id):", nrow(panel_ka), "\n")
cat("Unique office_id clusters:", n_distinct(panel_ka$office_id), "\n")

cat("\n==========================================================================\n")
cat("VERIFY TABLE 4 (tab:reallocation): Cross-bureau transfers in\n")
cat("==========================================================================\n")
t4_c1 <- feols(n_transfers_in ~ n_drafted_male + log(cumul_n_male + 1) |
                 year_num + ka_id + pos_norm,
               data = panel_ka, cluster = ~office_id)
tr_coef <- coef(t4_c1)["n_drafted_male"]
tr_se   <- se(t4_c1)["n_drafted_male"]
tr_p    <- pvalue(t4_c1)["n_drafted_male"]
cat(sprintf("  Coef: %.4f  SE: %.4f  p: %.4f  N: %d\n",
    tr_coef, tr_se, tr_p, nobs(t4_c1)))
cat("  Paper claims 0.1438 ** -- MATCH?", ifelse(abs(tr_coef - 0.1438) < 0.001, "YES", "NO"), "\n\n")

t4_c2 <- feols(n_transfers_in ~ n_drafted_male + n_drafted_male:is_rank1 +
                 n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
                 year_num + ka_id + pos_norm,
               data = panel_ka, cluster = ~office_id)
etable(t4_c1, t4_c2, fitstat = ~n + r2 + G)

cat("\n==========================================================================\n")
cat("VERIFY TABLE 4b (tab:reallocation-newhires): New hires\n")
cat("==========================================================================\n")
t4b_c1 <- feols(n_new_hires ~ n_drafted_male + log(cumul_n_male + 1) |
                  year_num + ka_id + pos_norm,
                data = panel_ka, cluster = ~office_id)
nh_coef <- coef(t4b_c1)["n_drafted_male"]
nh_se   <- se(t4b_c1)["n_drafted_male"]
nh_p    <- pvalue(t4b_c1)["n_drafted_male"]
cat(sprintf("  Coef: %.4f  SE: %.4f  p: %.4f  N: %d\n",
    nh_coef, nh_se, nh_p, nobs(t4b_c1)))
cat("  Paper claims 0.2057 ** -- MATCH?", ifelse(abs(nh_coef - 0.2057) < 0.01, "YES", "NO"), "\n\n")

cat("\n==========================================================================\n")
cat("2-TO-1 RATIO AUDIT\n")
cat("==========================================================================\n")
cat(sprintf("  Cross-bureau transfers (Table 4 C1):  %.4f\n", tr_coef))
cat(sprintf("  New hires (Table 4b C1):              %.4f\n", nh_coef))
cat(sprintf("  Ratio (transfers / hires):             %.3f\n", tr_coef / nh_coef))
cat("\n")
cat("  The 0.41 total transfers claimed in Introduction = SUM of Table 5 cols:\n")
cat("    Col 1 (same-section retention):     0.2269\n")
cat("    Col 3 (close transfer, same bureau): 0.1001\n")
cat("    Col 5 (distant transfer, diff bureau): 0.0809\n")
cat("    Sum:                                 0.4079  ~= 0.41\n\n")
cat(sprintf("  If true 'transfers in' (cols 3+5 only): %.4f\n", 0.1001 + 0.0809))
cat(sprintf("  Same-section retention (col 1):         0.2269\n"))
cat(sprintf("  New hires (Table 4b):                   %.4f\n", nh_coef))
cat("\n")
cat("  Paper says 'about twice as many internal transfers (0.41) as external hires (0.22)'\n")
cat("  But 0.41 includes 0.2269 same-section stayers + 0.1810 actual cross-section transfers\n")
cat("  If we exclude same-section, ratio is:", round((0.1001 + 0.0809) / nh_coef, 3),
    "-- LESS than 1-to-1\n")
cat("\n")
cat("  ALSO: Results.tex says 'We omit within-ka transfers' but Table 5 NOW includes them.\n")
cat("  The breakdown 0.29/0.14 in Introduction does NOT match current Table 5 (0.1001/0.0809).\n")

cat("\n==========================================================================\n")
cat("DESCRIPTIVE MEANS\n")
cat("==========================================================================\n")
cat("  mean n_drafted_male:", round(mean(panel_ka$n_drafted_male), 4), "\n")
cat("  mean n_transfers_in:", round(mean(panel_ka$n_transfers_in), 4), "\n")
cat("  mean n_new_hires:   ", round(mean(panel_ka$n_new_hires), 4), "\n")
cat("  obs with any draft: ", sum(panel_ka$n_drafted_male > 0), "\n")
cat("  obs with transfers: ", sum(panel_ka$n_transfers_in > 0), "\n")
