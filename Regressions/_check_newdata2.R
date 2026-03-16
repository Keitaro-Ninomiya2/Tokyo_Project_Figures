library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year))

# Check if duplicates differ on any column
dupes <- df %>% count(staff_id, year_num) %>% filter(n > 1)
sample_ids <- head(dupes$staff_id, 5)

cat("=== Sample duplicate records (all columns) ===\n")
for (sid in sample_ids) {
  records <- df %>% filter(staff_id == sid) %>% arrange(year_num)
  cat("\nstaff_id:", sid, "\n")
  for (yr in unique(records$year_num)) {
    yr_recs <- records %>% filter(year_num == yr)
    if (nrow(yr_recs) > 1) {
      cat("  year:", yr, " n_rows:", nrow(yr_recs), "\n")
      # Check which columns differ
      for (col in names(yr_recs)) {
        vals <- unique(yr_recs[[col]])
        if (length(vals) > 1) cat("    DIFFERS on", col, ":", paste(vals, collapse=" | "), "\n")
      }
    }
  }
}

# Count exact duplicates
cat("\n=== Deduplication check ===\n")
cat("Before dedup:", nrow(df), "\n")
df_dedup <- df %>% distinct(staff_id, year_num, office_id, position, .keep_all = TRUE)
cat("After dedup (staff_id, year, office, position):", nrow(df_dedup), "\n")
df_dedup2 <- df %>% distinct(staff_id, year_num, .keep_all = TRUE)
cat("After dedup (staff_id, year only):", nrow(df_dedup2), "\n")

# Check what columns typically differ
cat("\n=== Which columns differ in duplicates? ===\n")
sample_dupes <- head(dupes, 100)
diff_cols <- list()
for (i in 1:nrow(sample_dupes)) {
  recs <- df %>% filter(staff_id == sample_dupes$staff_id[i], year_num == sample_dupes$year_num[i])
  for (col in names(recs)) {
    if (length(unique(recs[[col]])) > 1) {
      diff_cols[[col]] <- (diff_cols[[col]] %||% 0) + 1
    }
  }
}
for (col in names(diff_cols)) {
  cat(col, ":", diff_cols[[col]], "/ 100\n")
}
