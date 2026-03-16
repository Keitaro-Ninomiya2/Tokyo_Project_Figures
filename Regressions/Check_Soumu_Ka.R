################################################################################
# Investigate: Why so many 総務課 / 総務係 in the data?
################################################################################

library(tidyverse)
library(here)

path <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)
df <- read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
df1950 <- df %>% filter(as.numeric(year) == 1950)

# Kyoku correction
kyoku_corr <- tibble(
  kyoku_data = c(
    "マ〓議普導務局", "区議会事務局", "区議〓〓務局", "区議留経務局", "区難留導務局",
    "区智事務局", "会事務局", "区会事務局", "谷区選擧管理委員会事務局",
    "京京都立高等保母学院", "世田谷産院", "築地産院", "荒川産院", "監察医務院",
    "養育院", "中央卸売市場"
  ),
  kyoku_correct = c(
    rep("総務局", 9), "民生局", rep("衛生局", 4), "民生局", "経済局"
  )
)
df1950 <- df1950 %>%
  left_join(kyoku_corr, by = c("kyoku" = "kyoku_data")) %>%
  mutate(kyoku = coalesce(kyoku_correct, kyoku)) %>%
  select(-kyoku_correct)

# Count records where ka contains 総務 or office contains 総務
soumu_ka <- df1950 %>%
  filter(str_detect(ka, "総務") | str_detect(office_norm, "総務") | str_detect(office, "総務")) %>%
  mutate(soumu_source = case_when(
    str_detect(ka, "総務") & str_detect(office_norm, "総務") ~ "ka+office",
    str_detect(ka, "総務") ~ "ka",
    TRUE ~ "office_only"
  ))

cat("=== Records with 総務 in ka or office (1950) ===\n")
cat("Total records:", nrow(soumu_ka), "\n")
cat("Unique (kyoku, ka, office_id):", nrow(distinct(soumu_ka, kyoku, ka, office_id)), "\n\n")

# Breakdown by kyoku
cat("=== By kyoku ===\n")
soumu_by_kyoku <- soumu_ka %>%
  group_by(kyoku) %>%
  summarise(
    n_records = n(),
    n_office_id = n_distinct(office_id),
    n_ka_distinct = n_distinct(ka),
    ka_list = paste(sort(unique(ka)), collapse = " | ")
  ) %>%
  arrange(desc(n_records))
print(soumu_by_kyoku, n = Inf)

# Sample of distinct ka values that contain 総務
cat("\n=== Distinct ka values containing 総務 ===\n")
soumu_kas <- soumu_ka %>% distinct(ka) %>% filter(!is.na(ka)) %>% pull(ka) %>% sort()
print(soumu_kas)

# Are these 総務課 or 総務係 or something else? Check office_norm too
cat("\n=== Sample (kyoku, ka, office_norm) for 総務 - first 50 ===\n")
sample_soumu <- soumu_ka %>%
  distinct(kyoku, ka, office_norm, office_id) %>%
  arrange(kyoku, ka) %>%
  head(50)
print(sample_soumu, n = 50)

# Are 総務課 all under 総務局? Or spread across kyoku?
cat("\n=== Count of 総務課 (exact) by kyoku ===\n")
soumu_ka_exact <- df1950 %>%
  filter(ka == "総務課" | str_trim(ka) == "総務課")
cat("Exact 総務課 records:", nrow(soumu_ka_exact), "\n")
soumu_ka_exact %>% count(kyoku, name = "n") %>% arrange(desc(n)) %>% print(n = Inf)

# What about partial matches - 総務 in ka
cat("\n=== All unique ka containing 総務, with kyoku and count ===\n")
soumu_ka_partial <- df1950 %>%
  filter(str_detect(ka, "総務")) %>%
  count(kyoku, ka, name = "n_staff") %>%
  arrange(kyoku, desc(n_staff))
print(soumu_ka_partial, n = 100)

write_csv(soumu_ka_partial, here::here("Regressions", "Soumu_Ka_Breakdown_1950.csv"))
cat("\nExported to Regressions/Soumu_Ka_Breakdown_1950.csv\n")
