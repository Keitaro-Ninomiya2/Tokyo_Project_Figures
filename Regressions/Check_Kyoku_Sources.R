# Quick check: what raw kyoku -> 建築局? And 千代田区役所 under which kyoku?
library(tidyverse)
path <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)", "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")
df <- read_csv(path, locale=locale(encoding="UTF-8"), show_col_types=FALSE) %>% filter(as.numeric(year)==1950)

# Records with 千代田区役所 in ka or office
chiyoda <- df %>% filter(str_detect(ka, "千代田") | str_detect(office_norm, "千代田") | str_detect(office, "千代田"))
cat("Records with 千代田 (Chiyoda):", nrow(chiyoda), "\n")
chiyoda %>% count(kyoku, ka) %>% print()

# All kyoku values in raw data (before any correction)
cat("\nAll distinct kyoku in 1950 data:\n")
df %>% count(kyoku, sort=TRUE) %>% print(n=30)

# 建築局 - what offices/ka does it contain?
cat("\nSample ka under 建築局:\n")
df %>% filter(kyoku == "建築局") %>% count(ka, sort=TRUE) %>% head(30) %>% print()
