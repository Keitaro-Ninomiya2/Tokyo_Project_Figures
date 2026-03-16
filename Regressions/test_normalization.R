library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# ============================================================
# STEP 1: Normalize kyoku names (fix OCR errors)
# ============================================================
normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    # Subsidiaries → parent kyoku
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生") ~ "厚生局",
    str_detect(k, "東京市主事.*厚生") ~ "厚生局",
    str_detect(k, "清掃監督.*厚生") ~ "厚生局",
    # OCR garbage with identifiable kyoku embedded
    str_detect(k, "社会") ~ "社会局",
    str_detect(k, "保健") ~ "保健局",
    str_detect(k, "上木") ~ "土木局",  # OCR: 上木→土木
    str_detect(k, "土木") ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務") ~ "府会事務局",
    str_detect(k, "發育") ~ "教育局",  # OCR: 發育→教育
    str_detect(k, "教育") ~ "教育局",
    str_detect(k, "水違") ~ "水道局",  # OCR: 水違→水道
    str_detect(k, "水道") ~ "水道局",
    str_detect(k, "電気") ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務") ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",  # OCR variants
    str_detect(k, "経済") ~ "経済局",
    str_detect(k, "厚生") ~ "厚生局",
    str_detect(k, "市民") ~ "市民局",
    str_detect(k, "中央卸売") ~ "中央卸売市場",
    str_detect(k, "港準") ~ "港湾局",  # OCR: 港準→港湾
    str_detect(k, "港湾") ~ "港湾局",
    str_detect(k, "監査") ~ "監査局",
    str_detect(k, "産業") ~ "産業局",
    str_detect(k, "城東病院") ~ "健民局",  # hospital under 健民局
    str_detect(k, "健民") ~ "健民局",
    str_detect(k, "戰時生活") ~ "戰時生活局",
    str_detect(k, "経〓") ~ "経理局",  # OCR: 経〓→経理
    str_detect(k, "経理") ~ "経理局",
    str_detect(k, "労働") ~ "労働局",
    str_detect(k, "建築|御築") ~ "建築局",
    str_detect(k, "道路") ~ "道路局",
    str_detect(k, "復興") ~ "復興事業局",
    str_detect(k, "交通") ~ "交通局",
    str_detect(k, "計[晝画]") ~ "計画局",
    str_detect(k, "民局") ~ "健民局",   # OCR truncation of 健民局
    str_detect(k, "後醍院") ~ "電気局",  # worker flow: mostly from 電気局
    str_detect(k, "総局") ~ NA_character_,  # ambiguous OCR
    str_detect(k, "築地産院|荒産院") ~ "健民局",  # hospitals
    str_detect(k, "衆議院|参講院") ~ NA_character_,  # national legislature, not Tokyo
    str_detect(k, "補画金") ~ NA_character_,
    str_detect(k, "伊興事業") ~ "復興事業局",
    str_detect(k, "告補助") ~ NA_character_,
    str_detect(k, "交換臺") ~ NA_character_,
    str_detect(k, "日大参保療院|四保第") ~ NA_character_,
    str_detect(k, "八王子郵便|横須賀郵便") ~ NA_character_,
    str_detect(k, "防衛") ~ "防衛局",
    str_detect(k, "民生") ~ "民生局",
    str_detect(k, "長官官房") ~ "長官官房",
    TRUE ~ NA_character_  # unrecognized → NA
  )
}

# ============================================================
# STEP 2: Merger crosswalk — map pre-1943 and post-1943 kyoku to groups
# ============================================================
# Based on worker flows and 1944 Tokyo-To ground truth:
# - 電気局 → 交通局
# - 土木局 + 経理局(部分) → 計画局
# - 戰時生活局 + 中央卸売市場 → 経済局
# - 健民局 + 厚生局 + 社会局 → 民生局
# - 教育局 → 教育局 + 計画局(部分)
# - 水道局 → 水道局
# - 港湾局 → 港湾局
# - 財務局 + 経理局(部分) → 長官官房
# - 市会事務局 + 府会事務局 → (dissolved, absorbed into 長官官房)
assign_kyoku_group <- function(norm_kyoku) {
  case_when(
    is.na(norm_kyoku) ~ NA_character_,
    norm_kyoku %in% c("電気局", "交通局")                   ~ "transport",
    norm_kyoku %in% c("水道局")                             ~ "water",
    norm_kyoku %in% c("港湾局")                             ~ "port",
    norm_kyoku %in% c("教育局")                             ~ "education",
    norm_kyoku %in% c("土木局", "計画局", "経理局", "建築局", "道路局") ~ "infrastructure",
    norm_kyoku %in% c("戰時生活局", "経済局", "中央卸売市場", "産業局") ~ "economy",
    norm_kyoku %in% c("健民局", "厚生局", "社会局", "保健局",
                       "民生局", "養育院", "労働局")         ~ "welfare",
    norm_kyoku %in% c("市会事務局", "府会事務局")           ~ "assembly",
    norm_kyoku %in% c("財務局", "監査局", "長官官房")       ~ "finance_admin",
    norm_kyoku == "復興事業局"                               ~ "reconstruction",
    norm_kyoku == "市民局"                                   ~ "citizen",
    norm_kyoku == "防衛局"                                   ~ "defense",
    TRUE ~ NA_character_
  )
}

df <- df %>%
  mutate(
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku)
  )

# ============================================================
# TEST: Coverage and stability
# ============================================================

cat("=== Normalization coverage by year ===\n")
for (yr in 1938:1944) {
  sub <- df %>% filter(year_num == yr)
  n_total <- nrow(sub)
  n_has_norm <- sum(!is.na(sub$norm_kyoku))
  n_has_group <- sum(!is.na(sub$kyoku_group))
  cat(sprintf("%d: %d workers, %d (%.0f%%) have norm_kyoku, %d (%.0f%%) have kyoku_group\n",
      yr, n_total, n_has_norm, 100*n_has_norm/n_total, n_has_group, 100*n_has_group/n_total))
}

cat("\n=== Normalized kyoku stability across years ===\n")
for (yr in 1938:1944) {
  wa <- df %>% filter(year_num == yr) %>% select(staff_id, nk_a = norm_kyoku)
  wb <- df %>% filter(year_num == yr + 1) %>% select(staff_id, nk_b = norm_kyoku)
  mm <- inner_join(wa, wb, by = "staff_id") %>% filter(!is.na(nk_a) & !is.na(nk_b))
  same <- sum(mm$nk_a == mm$nk_b)
  cat(sprintf("  %d->%d: %d matched (both non-NA), %d same norm_kyoku (%.0f%%)\n",
      yr, yr+1, nrow(mm), same, 100*same/nrow(mm)))
}

cat("\n=== Kyoku GROUP stability across years ===\n")
for (yr in 1938:1944) {
  wa <- df %>% filter(year_num == yr) %>% select(staff_id, g_a = kyoku_group)
  wb <- df %>% filter(year_num == yr + 1) %>% select(staff_id, g_b = kyoku_group)
  mm <- inner_join(wa, wb, by = "staff_id") %>% filter(!is.na(g_a) & !is.na(g_b))
  same <- sum(mm$g_a == mm$g_b)
  cat(sprintf("  %d->%d: %d matched (both non-NA), %d same group (%.0f%%)\n",
      yr, yr+1, nrow(mm), same, 100*same/nrow(mm)))
}

# Check: what fraction of workers are "transfers" under the group definition?
cat("\n=== Transfer rate (kyoku_group changed) by year ===\n")
for (yr in 1939:1944) {
  wa <- df %>% filter(year_num == yr - 1) %>% select(staff_id, g_a = kyoku_group)
  wb <- df %>% filter(year_num == yr) %>% select(staff_id, g_b = kyoku_group)
  mm <- inner_join(wa, wb, by = "staff_id") %>% filter(!is.na(g_a) & !is.na(g_b))
  diff <- sum(mm$g_a != mm$g_b)
  cat(sprintf("  %d: %d workers tracked, %d changed group (%.1f%% transfer rate)\n",
      yr, nrow(mm), diff, 100*diff/nrow(mm)))
}

# Remaining unmapped kyoku
cat("\n=== Unmapped kyoku values (norm_kyoku = NA) by year ===\n")
for (yr in 1938:1944) {
  unmapped <- df %>% filter(year_num == yr, is.na(norm_kyoku), !is.na(kyoku)) %>%
    count(kyoku, sort = TRUE)
  if (nrow(unmapped) > 0) {
    cat(sprintf("\n%d:\n", yr))
    print(unmapped, n = 20)
  }
}
