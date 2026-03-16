################################################################################
# Why does 総務局 have 231 ka in data vs 10 in ground truth?
# Investigate what ka values appear under 総務局
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

# Kyoku correction (things that map TO 総務局)
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

# Filter to 総務局 only
soumu <- df1950 %>% filter(kyoku == "総務局")

cat("=== 総務局: 231 ka in data vs 10 in ground truth ===\n\n")
cat("Ground truth 総務局 ka (10): 文書課, 人事課, 地方課, 学務課, 監査課, 統計課, 福利課, 特別調査課, 臨時国勢調査部, 都立大学事務局\n\n")

# List all distinct ka under 総務局 with counts
ka_counts <- soumu %>%
  count(ka, name = "n_staff") %>%
  arrange(desc(n_staff))

cat("=== All distinct ka under 総務局 (data), sorted by n_staff ===\n")
print(ka_counts, n = Inf)

# Categorize: which look like HQ 課 vs ward/local offices?
# Ground truth HQ: 文書課, 人事課, 地方課, 学務課, 監査課, 統計課, 福利課, 特別調査課, 臨時国勢調査部, 都立大学事務局
gt_ka <- c("文書課", "人事課", "地方課", "学務課", "監査課", "統計課", "福利課", "特別調査課", "臨時国勢調査部", "都立大学事務局")

# Patterns that suggest ward/local: 区役所, 支所, 出張所, 地方事務所, 保健所, 〇〇課 (ward-level 課)
ka_counts <- ka_counts %>%
  mutate(
    is_hq = ka %in% gt_ka | str_detect(ka, paste(gt_ka, collapse = "|")),
    looks_ward = str_detect(ka, "区役所|支所|出張所|地方事務所|保健所|区議会")
  )

cat("\n=== Breakdown: HQ-like vs ward/local-like ===\n")
cat("Ka matching ground truth HQ:", sum(ka_counts$is_hq), "\n")
cat("Ka that look like ward/local (区役所, 支所, 出張所, etc.):", sum(ka_counts$looks_ward, na.rm = TRUE), "\n")

# Show sample of ka that are NOT in ground truth
non_gt <- ka_counts %>% filter(!is_hq) %>% head(50)
cat("\n=== Sample of ka NOT in ground truth (first 50) ===\n")
print(non_gt %>% select(ka, n_staff), n = 50)

write_csv(ka_counts, here("Regressions", "SoumuKyoku_Ka_List_1950.csv"))
cat("\nFull list exported to Regressions/SoumuKyoku_Ka_List_1950.csv\n")
