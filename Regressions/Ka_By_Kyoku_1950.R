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

# Kyoku: use order-based assignment (ground-truth ka order)
# Run Assign_Kyoku_By_Order_1950.R first to generate the mapping
office_kyoku_map <- read_csv(here("Regressions", "Office_Kyoku_By_Order_1950.csv"), show_col_types = FALSE)
kyoku_fallback <- tibble(
  kyoku_raw = c("京京都立高等保母学院", "世田谷産院", "築地産院", "荒川産院", "監察医務院", "養育院", "中央卸売市場"),
  kyoku_correct = c("民生局", "衛生局", "衛生局", "衛生局", "衛生局", "民生局", "経済局")
)
df1950 <- df1950 %>%
  left_join(office_kyoku_map, by = "office_id") %>%
  left_join(kyoku_fallback, by = c("kyoku" = "kyoku_raw")) %>%
  mutate(kyoku = coalesce(kyoku_assigned, kyoku_correct, kyoku)) %>%
  select(-kyoku_assigned, -kyoku_correct)

# Count distinct ka per kyoku (1950, corrected)
ka_by_kyoku <- df1950 %>%
  filter(!is.na(kyoku)) %>%
  group_by(kyoku) %>%
  summarise(
    n_ka   = n_distinct(ka, na.rm = TRUE),
    n_office = n_distinct(office_id),
    n_staff = n()
  ) %>%
  arrange(desc(n_staff))

cat("=== Number of ka per kyoku (1950, corrected kyoku) ===\n")
print(ka_by_kyoku, n = Inf)

write_csv(ka_by_kyoku, here::here("Regressions", "Ka_By_Kyoku_1950.csv"))
cat("\nExported to Regressions/Ka_By_Kyoku_1950.csv\n")
