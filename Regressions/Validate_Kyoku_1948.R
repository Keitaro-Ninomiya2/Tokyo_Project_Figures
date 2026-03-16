################################################################################
# Cross-check 1948 kyoku/office assignment against ground truth
# Ground truth: 昭和23年8月1日現在 (as of Aug 1, 1948)
################################################################################

library(tidyverse)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

cat("Columns in data:", paste(names(df), collapse = ", "), "\n\n")

# Filter to 1948, get distinct org structure
df_1948 <- df %>% filter(as.numeric(year) == 1948)

# What identifier columns exist?
id_cols <- c("office_id", "kyoku", "ka", "kakari", "office", "department", "section")
have_cols <- id_cols[id_cols %in% names(df_1948)]
cat("Org-related columns present:", paste(have_cols, collapse = ", "), "\n\n")

# Get 1948 structure: office_id -> kyoku (and ka, kakari if present)
struct_1948 <- df_1948 %>%
  group_by(across(any_of(c("office_id", "kyoku", "ka", "kakari")))) %>%
  summarise(n_staff = n(), .groups = "drop")

cat("=== DATA: 1948 office_id x kyoku (modal if multiple) ===\n")
if ("office_id" %in% names(df_1948) && "kyoku" %in% names(df_1948)) {
  office_kyoku <- df_1948 %>%
    count(office_id, kyoku) %>%
    group_by(office_id) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(kyoku, office_id)
  print(office_kyoku, n = Inf)
  cat("\n")
}

# If ka, kakari exist, show full hierarchy
if (all(c("office_id", "kyoku", "ka", "kakari") %in% names(df_1948))) {
  cat("=== DATA: 1948 kyoku > ka > kakari (distinct) ===\n")
  hierarchy <- df_1948 %>%
    distinct(kyoku, ka, kakari, office_id) %>%
    arrange(kyoku, ka, kakari)
  print(hierarchy, n = Inf)
}

# Ground truth: 昭和23年8月1日現在
# kyoku/bu level: 総務部, 財務部, 民生局, 教育局, 経済局, 建設局, 交通局, 水道局, 衛生局, 労働局, 出納長室
ground_truth_kyoku <- tibble(
  kyoku_true = c(
    "総務部", "財務部", "民生局", "教育局", "経済局",
    "建設局", "交通局", "水道局", "衛生局", "労働局", "出納長室"
  )
)

# Ground truth ka (課) under each kyoku - from user's structure
gt_structure <- list(
  "総務部" = c("文書課", "人事課", "行政課", "監査課", "統計課", "福利課", "調査課", "特殊財産管理課", "観光課"),
  "財務部" = c("経理課", "予算課", "主税課", "渉外部", "庶務課", "工事課", "管理課"),
  "民生局" = c("総務課", "保護課", "児童課", "生活課", "世話課", "保険課", "養育院"),
  "教育局" = c("総務課", "普通教育課", "高等教育課", "社会教育課", "体育課", "施設課"),
  "経済局" = c("総務課", "商工課", "農務課", "農地課", "林務課", "食料課", "返還物資課", "貿易課", "賠償課", "中央卸売市場"),
  "建設局" = c("総務課", "都市計画課", "公園緑地課", "道路課", "河川課", "建築課", "住宅課", "港湾課", "区画整理課", "整地工事課", "土地課"),
  "交通局" = c("総務課", "労働課", "経理課", "電車課", "自動車課", "工務課", "電気課"),
  "水道局" = c("総務課", "業務課", "給水課", "工事課", "下水課"),
  "衛生局" = c("総務課", "公衆衛生課", "医務課", "看護課", "防疫課", "薬務課", "清掃課"),
  "労働局" = c("総務課", "労政課", "職業課", "労働組合課", "失業保険徴収課"),
  "出納長室" = c("庶務係", "審査係", "出納係", "国費係")  # note: 出納長室 has 係 not 課
)

cat("\n=== GROUND TRUTH: Expected kyoku and ka (昭和23年8月1日) ===\n")
for (k in names(gt_structure)) {
  cat(k, ":", paste(gt_structure[[k]], collapse = ", "), "\n")
}

# Compare: what kyoku values does the data have?
if ("kyoku" %in% names(df_1948)) {
  data_kyoku <- sort(unique(na.omit(df_1948$kyoku)))
  gt_kyoku <- names(gt_structure)
  cat("\n=== KYOKU COMPARISON ===\n")
  cat("In data, not in ground truth:", paste(setdiff(data_kyoku, gt_kyoku), collapse = ", "), "\n")
  cat("In ground truth, not in data:", paste(setdiff(gt_kyoku, data_kyoku), collapse = ", "), "\n")
  cat("Match:", all(data_kyoku %in% gt_kyoku) && all(gt_kyoku %in% data_kyoku), "\n")
}

# If office_id maps to ka, check office names
if ("office_id" %in% names(df_1948)) {
  # Any office name column?
  name_cols <- names(df_1948)[str_detect(names(df_1948), "name|office|課|ka|kakari")]
  cat("\nPotential name/label columns:", paste(name_cols, collapse = ", "), "\n")
}
