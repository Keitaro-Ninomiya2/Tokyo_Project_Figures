library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv")
df <- read_csv(DATA_PATH, locale=locale(encoding="UTF-8"), show_col_types=FALSE) %>%
  filter(is_name==TRUE) %>%
  mutate(year_num=as.numeric(year), pos_norm=str_replace_all(position,"\\s+",""))

cat("=== POSTWAR POSITIONS (1946-1953) ===\n")
pw <- df %>% filter(year_num %in% 1946:1953) %>%
  count(pos_norm, sort=TRUE)
print(pw, n=60)

cat("\n=== PREWAR POSITIONS (1934-1945) ===\n")
pre <- df %>% filter(year_num %in% 1934:1945) %>%
  count(pos_norm, sort=TRUE)
print(pre, n=60)

# Check for specific managerial titles in postwar
cat("\n=== POSTWAR MANAGERIAL TITLES CHECK ===\n")
mgr_patterns <- c("部長", "課長", "係長", "主任", "長", "主事", "技師", "書記")
for (p in mgr_patterns) {
  n <- df %>% filter(year_num %in% 1946:1953, str_detect(pos_norm, p)) %>% nrow()
  cat(p, ":", n, "\n")
}
