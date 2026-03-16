################################################################################
# Cross-check 1950 kyoku/office assignment against ground truth
# Ground truth: 昭和25年 (1950) — reorg from 1948 (総務部→総務局, 教育局→学務課 under 総務局, etc.)
################################################################################

library(tidyverse)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

df_1950 <- df %>% filter(as.numeric(year) == 1950)

cat("=== 1950 KYOKU VALIDATION ===\n")
cat("N rows 1950:", nrow(df_1950), "\n\n")

# -----------------------------------------------------------------------------
# GROUND TRUTH: 昭和25年 (1950) structure
# Source: User-provided org chart (知事室 S24.11.9, etc.)
# -----------------------------------------------------------------------------
gt_1950 <- list(
  "知事室" = character(0),
  "能率部" = character(0),
  "広報部" = character(0),
  "総務局" = c(
    "文書課", "人事課", "地方課", "学務課", "監査課", "統計課", "福利課",
    "特別調査課", "臨時国勢調査部", "都立大学事務局"
  ),
  "財務局" = c(
    "経理課", "予算課", "管財課", "主税部第一課", "主税部第二課",
    "渉外部庶務課", "渉外部管理課", "連絡調整室"
  ),
  "民生局" = c("総務課", "保護課", "児童課", "生活課", "保険課", "世話課", "養育院"),
  "経済局" = c(
    "総務課", "商工課", "農務課", "農地課", "林務課", "食料課",
    "返還物資課", "貿易課", "中央卸売市場"
  ),
  "建設局" = c(
    "総務課", "都市計画課", "公園観光課", "道路課", "河川課",
    "区画整理課", "整地工事課", "港湾部管理課", "港湾部工事課",
    "臨時露店対策部"
  ),
  "交通局" = c("総務課", "労働課", "電車課", "自動車課", "工務課", "電気課"),
  "水道局" = c("総務課", "業務課", "給水課", "建設課", "下水課"),
  "衛生局" = c(
    "総務課", "公衆衛生課", "医務課", "看護課", "予防課",
    "薬務課", "清掃事業部管理課", "清掃事業部作業課"
  ),
  "労働局" = c("総務課", "労政課", "職業安定課", "失業保険徴収課"),
  "建築局" = c("企画課", "指導課", "工事課", "建設業室"),
  "出納長室" = character(0)  # 庶務係、審査係、出納係、国費係 (係-level)
)

# Full crosswalk: kyoku -> ka -> kakari (for reference; simplified)
# Key ka names that may appear in data (with common OCR variants)
gt_ka_to_kyoku <- tibble(
  kyoku = rep(names(gt_1950), times = lengths(gt_1950)),
  ka = unlist(gt_1950)
) %>%
  filter(nchar(ka) > 0)

cat("=== GROUND TRUTH 1950: kyoku and ka ===\n")
for (k in names(gt_1950)) {
  kas <- gt_1950[[k]]
  cat(k, ":", if (length(kas) > 0) paste(kas, collapse = ", ") else "(top-level only)", "\n")
}

# -----------------------------------------------------------------------------
# DATA: 1950 office_id x kyoku
# -----------------------------------------------------------------------------
cat("\n=== DATA: 1950 office_id x kyoku (modal) ===\n")
office_kyoku_1950 <- df_1950 %>%
  filter(!is.na(kyoku)) %>%
  count(office_id, kyoku) %>%
  group_by(office_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(kyoku, office_id)

print(office_kyoku_1950, n = 50)
cat("... (total", nrow(office_kyoku_1950), "office_ids)\n\n")

# -----------------------------------------------------------------------------
# DATA: 1950 kyoku > ka (distinct)
# -----------------------------------------------------------------------------
cat("=== DATA: 1950 kyoku > ka (distinct) ===\n")
hierarchy_1950 <- df_1950 %>%
  filter(!is.na(kyoku)) %>%
  distinct(kyoku, ka, office_id) %>%
  arrange(kyoku, ka)
print(hierarchy_1950, n = 100)
cat("... (total", nrow(hierarchy_1950), "rows)\n\n")

# -----------------------------------------------------------------------------
# COMPARISON
# -----------------------------------------------------------------------------
gt_kyoku <- names(gt_1950)
data_kyoku <- sort(unique(na.omit(df_1950$kyoku)))

cat("=== KYOKU COMPARISON ===\n")
cat("In data, not in ground truth:", paste(setdiff(data_kyoku, gt_kyoku), collapse = ", "), "\n")
cat("In ground truth, not in data:", paste(setdiff(gt_kyoku, data_kyoku), collapse = ", "), "\n")
cat("Match:", identical(sort(data_kyoku), sort(gt_kyoku)), "\n\n")

# -----------------------------------------------------------------------------
# EXPORT CROSSWALK: ground truth 1950 for external use
# -----------------------------------------------------------------------------
crosswalk_1950 <- bind_rows(
  gt_ka_to_kyoku,
  tibble(kyoku = c("知事室", "能率部", "広報部", "出納長室"), ka = NA_character_)
) %>%
  distinct() %>%
  arrange(kyoku, ka)

out_path <- here::here("Regressions", "Kyoku_Crosswalk_1950.csv")
write_csv(crosswalk_1950, out_path, na = "")
cat("Crosswalk exported to:", out_path, "\n")

# Also save full ground truth as RDS for programmatic use
gt_full_1950 <- list(gt_kyoku = gt_kyoku, gt_1950 = gt_1950, gt_ka_to_kyoku = gt_ka_to_kyoku)
saveRDS(gt_full_1950, here::here("Regressions", "Kyoku_GroundTruth_1950.rds"))
cat("Ground truth saved to: Regressions/Kyoku_GroundTruth_1950.rds\n")

# -----------------------------------------------------------------------------
# OCR CORRECTION CROSSWALK: data kyoku -> correct kyoku (for known errors)
# 1950 data has encoding/OCR issues; add mappings as you identify them
# -----------------------------------------------------------------------------
ocr_corrections <- tibble(
  kyoku_data = c(
    "マ〓議普導務局",     # OCR garbled; ward/regional offices -> 総務局
    "区議会事務局", "区議〓〓務局", "区議留経務局", "区難留導務局",
    "区智事務局", "会事務局", "区会事務局",
    "谷区選擧管理委員会事務局",
    "京京都立高等保母学院",   # typo 京京 -> 都立, under 民生局/教育局
    "世田谷産院", "築地産院", "荒川産院",   # maternity hospitals -> 衛生局
    "監察医務院",                            # medical examiner -> 衛生局
    "養育院",                                 # under 民生局
    "中央卸売市場",                           # under 経済局
    "事務局"                                  # ambiguous; may need ka to disambiguate
  ),
  kyoku_correct = c(
    "総務局", "総務局", "総務局", "総務局", "総務局",
    "総務局", "総務局", "総務局",
    "総務局",   # 選挙管理委員会
    "民生局",   # 保母学院
    "衛生局", "衛生局", "衛生局",
    "衛生局",
    "民生局",
    "経済局",
    NA_character_   # leave as-is for manual review
  )
)
write_csv(ocr_corrections, here::here("Regressions", "Kyoku_OCR_Corrections_1950.csv"))
cat("OCR correction crosswalk: Regressions/Kyoku_OCR_Corrections_1950.csv\n")
cat("  (Edit this file to add kyoku_data -> kyoku_correct mappings)\n")
