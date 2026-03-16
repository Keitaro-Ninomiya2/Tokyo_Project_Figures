library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("Columns:", paste(names(df), collapse = ", "), "\n\n")
cat("Has kakari:", "kakari" %in% names(df), "\n")
cat("Non-NA kakari:", sum(!is.na(df$kakari)), "/", nrow(df), "\n")
cat("By year:\n")
df %>% filter(!is.na(kakari)) %>% count(year_num) %>% print(n=30)
cat("\nSample kakari values:\n")
df %>% filter(!is.na(kakari)) %>% pull(kakari) %>% unique() %>% head(20) %>% cat(sep="\n")
