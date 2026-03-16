################################################################################
# Extract records for Page047 for kyoku mis-assignment inspection
################################################################################

library(tidyverse)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

# Try page = 47, "47", "047", or "Page047" depending on column format
page_col <- "page"
if (!page_col %in% names(df)) {
  page_col <- names(df)[str_detect(names(df), "page")]
  if (length(page_col) == 0) stop("No page column found")
  page_col <- page_col[1]
}

# Filter for Page047 - try multiple formats
# Option: restrict to specific year (e.g. 1950) for kyoku inspection
year_filter <- 1950  # set to NULL to include all years
page047 <- df %>%
  filter(
    page == 47 |
    page == "47" |
    page == "047" |
    page == "Page047" |
    str_detect(as.character(page), "^047$|^47$|Page047")
  )
if (!is.null(year_filter)) {
  page047 <- page047 %>% filter(as.numeric(year) == year_filter)
}

# If no match, show unique page values to help debug
if (nrow(page047) == 0) {
  cat("No records found for Page047 in year", year_filter, ". Sample of page values:\n")
  print(head(unique(df[[page_col]]), 20))
  cat("\nTrying page containing '47' (all years)...\n")
  page047 <- df %>% filter(str_detect(as.character(!!sym(page_col)), "47"))
  if (!is.null(year_filter)) page047 <- page047 %>% filter(as.numeric(year) == year_filter)
}

cat("Records for Page047:", nrow(page047), "\n\n")

# Select key columns for inspection
cols_keep <- c("year", "page", "image", "office_id", "kyoku", "bu", "ka", "kakari",
               "office", "office_norm", "position", "name", "rank")
cols_keep <- intersect(cols_keep, names(page047))
page047_display <- page047 %>% select(all_of(cols_keep))

# Print to console
print(page047_display, n = Inf)

# Export CSV for inspection
out_path <- here::here("Regressions", "Page047_Records.csv")
write_csv(page047_display, out_path)
cat("\nExported to:", out_path, "\n")

# Summary: unique kyoku × ka × office_id (for inspection)
cat("\n=== UNIQUE KYOKU × KA × office_id (Page047) - RAW ===\n")
summary_tbl <- page047 %>%
  distinct(kyoku, ka, office_id, office, .keep_all = FALSE) %>%
  arrange(kyoku, ka, office_id)
print(summary_tbl, n = Inf)

# Apply same ground-truth kyoku correction as FirstStage_FemaleManager.R
kyoku_corrections <- tibble(
  kyoku_data = c(
    "マ〓議普導務局", "区議会事務局", "区議〓〓務局", "区議留経務局", "区難留導務局",
    "区智事務局", "会事務局", "区会事務局", "谷区選擧管理委員会事務局",
    "京京都立高等保母学院", "世田谷産院", "築地産院", "荒川産院", "監察医務院",
    "養育院", "中央卸売市場"
  ),
  kyoku_correct = c(
    "総務局", "総務局", "総務局", "総務局", "総務局", "総務局", "総務局", "総務局",
    "総務局", "民生局", "衛生局", "衛生局", "衛生局", "衛生局", "民生局", "経済局"
  )
)
page047_corrected <- page047 %>%
  left_join(kyoku_corrections, by = c("kyoku" = "kyoku_data")) %>%
  mutate(kyoku_raw = kyoku, kyoku = coalesce(kyoku_correct, kyoku)) %>%
  select(-kyoku_correct)

cat("\n=== AFTER GROUND-TRUTH CORRECTION: kyoku_raw -> kyoku ===\n")
summary_corrected <- page047_corrected %>%
  distinct(kyoku_raw, kyoku, ka, office_id, office, .keep_all = FALSE) %>%
  arrange(kyoku, ka, office_id)
print(summary_corrected, n = Inf)

write_csv(summary_tbl, here::here("Regressions", "Page047_Summary.csv"))
write_csv(summary_corrected, here::here("Regressions", "Page047_Summary_Corrected.csv"))
cat("\nSummary exported to: Regressions/Page047_Summary.csv")
cat("\nCorrected summary to: Regressions/Page047_Summary_Corrected.csv\n")
