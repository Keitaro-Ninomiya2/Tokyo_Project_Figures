library(readr)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
years <- sort(unique(as.numeric(df$year)))
cat("All years:", paste(years, collapse = ", "), "\n")

drafted <- df[!is.na(df$drafted) & df$drafted == TRUE, ]
dy <- sort(unique(as.numeric(drafted$year)))
cat("Drafted years:", paste(dy, collapse = ", "), "\n")

is_name_df <- df[df$is_name == TRUE, ]
for (y in years[years < 1944]) {
  n <- sum(as.numeric(is_name_df$year) == y)
  cat("Year", y, ":", n, "name rows\n")
}
