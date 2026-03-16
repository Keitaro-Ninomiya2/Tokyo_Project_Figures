# Quick diagnostic: what positions exist in 1950-1951 with is_name==TRUE?
library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(as.numeric(year) %in% c(1950, 1951), is_name == TRUE)
df <- df %>% mutate(pos_norm = str_replace_all(position, "\\s+", ""))
cat("Sample of pos_norm containing 5951 (係) or 9577 (長):\n")
has_kaku <- df %>% filter(grepl("\u5951|\u9577", pos_norm))
cat("N rows with 係 or 長 in pos_norm:", nrow(has_kaku), "\n")
if (nrow(has_kaku) > 0) {
  cat("Unique pos_norm (first 20):\n")
  print(head(unique(has_kaku$pos_norm), 20))
}
cat("\nAny 係長 exact match:", sum(grepl("\u5951\u9577", df$pos_norm)), "\n")
cat("Unique positions (first 30):\n")
print(head(unique(df$pos_norm), 30))
