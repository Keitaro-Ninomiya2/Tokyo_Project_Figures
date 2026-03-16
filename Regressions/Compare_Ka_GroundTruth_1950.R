library(tidyverse)
library(here)

# Ground truth: number of ka per kyoku (from 1950 org chart / Kyoku_Crosswalk_1950_GroundTruth.csv)
gt_n_ka <- tibble(
  kyoku = c(
    "知事室", "能率部", "広報部", "総務局", "財務局", "民生局", "経済局",
    "建設局", "交通局", "水道局", "衛生局", "労働局", "建築局", "出納長室",
    "区部"    # Ward offices: 23 wards + 支所, 出張所, etc. — not in central org chart
  ),
  gt_n_ka = c(
    0, 0, 0,
    10,   # 総務局: 文書, 人事, 地方, 学務, 監査, 統計, 福利, 特別調査, 臨時国勢調査, 都立大学
    8,    # 財務局: 経理, 予算, 管財, 主税部1, 主税部2, 渉外庶務, 渉外管理, 連絡調整
    7,    # 民生局: 総務, 保護, 児童, 生活, 保険, 世話, 養育院
    9,    # 経済局: 総務, 商工, 農務, 農地, 林務, 食料, 返還物資, 貿易, 中央卸売市場
    10,   # 建設局: 総務, 都市計画, 公園観光, 道路, 河川, 区画整理, 整地工事, 港湾部管理, 港湾部工事, 臨時露店対策
    6,    # 交通局: 総務, 労働, 電車, 自動車, 工務, 電気
    5,    # 水道局: 総務, 業務, 給水, 建設, 下水
    8,    # 衛生局: 総務, 公衆衛生, 医務, 看護, 予防, 薬務, 清掃事業部管理, 清掃事業部作業
    4,    # 労働局: 総務, 労政, 職業安定, 失業保険徴収
    4,    # 建築局: 企画, 指導, 工事, 建設業室
    0,    # 出納長室
    NA_integer_  # 区部: ward offices — many ka, structure varies by ward
  )
)

# Data counts (from Ka_By_Kyoku_1950.R output)
data_n_ka <- read_csv(here("Regressions", "Ka_By_Kyoku_1950.csv"), show_col_types = FALSE) %>%
  rename(data_n_ka = n_ka) %>%
  select(kyoku, data_n_ka, n_office, n_staff)

# Merge and compare
comparison <- gt_n_ka %>%
  full_join(data_n_ka, by = "kyoku") %>%
  mutate(
    data_n_ka = replace_na(data_n_ka, 0),
    ratio = if_else(!is.na(gt_n_ka) & gt_n_ka > 0, round(data_n_ka / gt_n_ka, 2), NA_real_)
  ) %>%
  arrange(kyoku)

cat("=== CROSSWALK: Data vs Ground Truth — n_ka per kyoku (1950) ===\n\n")
print(comparison, n = Inf)

# Summary stats
cat("\n--- Kyoku in data but not in ground truth: ---\n")
cat(setdiff(data_n_ka$kyoku, gt_n_ka$kyoku), "\n")
cat("\n--- Kyoku in ground truth but not in data: ---\n")
cat(setdiff(gt_n_ka$kyoku, data_n_ka$kyoku), "\n")
cat("\n--- Ratio data/ground truth (central bureaus only): ---\n")
comp_ratio <- comparison %>% filter(!is.na(gt_n_ka), gt_n_ka > 0, data_n_ka > 0)
print(comp_ratio %>% select(kyoku, gt_n_ka, data_n_ka, n_office, ratio), n = Inf)

write_csv(comparison, here::here("Regressions", "Ka_GroundTruth_Comparison_1950.csv"))
cat("\nExported to Regressions/Ka_GroundTruth_Comparison_1950.csv\n")
