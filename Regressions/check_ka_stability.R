library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Reuse normalization
normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生") ~ "厚生局",
    str_detect(k, "東京市主事.*厚生") ~ "厚生局",
    str_detect(k, "清掃監督.*厚生") ~ "厚生局",
    str_detect(k, "社会") ~ "社会局", str_detect(k, "保健") ~ "保健局",
    str_detect(k, "上木") ~ "土木局", str_detect(k, "土木") ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務") ~ "府会事務局",
    str_detect(k, "發育") ~ "教育局", str_detect(k, "教育") ~ "教育局",
    str_detect(k, "水違") ~ "水道局", str_detect(k, "水道") ~ "水道局",
    str_detect(k, "電気") ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院", str_detect(k, "財務") ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",
    str_detect(k, "経済") ~ "経済局", str_detect(k, "厚生") ~ "厚生局",
    str_detect(k, "市民") ~ "市民局", str_detect(k, "中央卸売") ~ "中央卸売市場",
    str_detect(k, "港準") ~ "港湾局", str_detect(k, "港湾") ~ "港湾局",
    str_detect(k, "監査") ~ "監査局", str_detect(k, "産業") ~ "産業局",
    str_detect(k, "城東病院") ~ "健民局", str_detect(k, "健民") ~ "健民局",
    str_detect(k, "戰時生活") ~ "戰時生活局",
    str_detect(k, "経〓") ~ "経理局", str_detect(k, "経理") ~ "経理局",
    str_detect(k, "労働") ~ "労働局", str_detect(k, "建築|御築") ~ "建築局",
    str_detect(k, "道路") ~ "道路局", str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通") ~ "交通局", str_detect(k, "計[晝画]") ~ "計画局",
    str_detect(k, "民局") ~ "健民局", str_detect(k, "後醍院") ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛") ~ "防衛局", str_detect(k, "民生") ~ "民生局",
    str_detect(k, "長官官房") ~ "長官官房", TRUE ~ NA_character_)
}
assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("電気局", "交通局") ~ "transport", nk == "水道局" ~ "water",
    nk == "港湾局" ~ "port", nk == "教育局" ~ "education",
    nk %in% c("土木局", "計画局", "経理局", "建築局", "道路局") ~ "infrastructure",
    nk %in% c("戰時生活局", "経済局", "中央卸売市場", "産業局") ~ "economy",
    nk %in% c("健民局", "厚生局", "社会局", "保健局", "民生局", "養育院", "労働局") ~ "welfare",
    nk %in% c("市会事務局", "府会事務局") ~ "assembly",
    nk %in% c("財務局", "監査局", "長官官房") ~ "finance_admin",
    nk == "復興事業局" ~ "reconstruction", nk == "市民局" ~ "citizen",
    nk == "防衛局" ~ "defense", TRUE ~ NA_character_)
}

df <- df %>% mutate(norm_kyoku = normalize_kyoku(kyoku),
                     kyoku_group = assign_kyoku_group(norm_kyoku))

# Among workers who STAYED in the same kyoku_group, how often does ka match?
cat("=== Ka match rate among same-group stayers ===\n")
for (yr in 1938:1944) {
  wa <- df %>% filter(year_num == yr) %>%
    select(staff_id, g_a = kyoku_group, ka_a = ka, kakari_a = kakari)
  wb <- df %>% filter(year_num == yr + 1) %>%
    select(staff_id, g_b = kyoku_group, ka_b = ka, kakari_b = kakari)
  mm <- inner_join(wa, wb, by = "staff_id") %>%
    filter(!is.na(g_a) & !is.na(g_b) & g_a == g_b)  # same group stayers
  ka_match <- sum(mm$ka_a == mm$ka_b, na.rm = TRUE)
  ka_both_valid <- sum(!is.na(mm$ka_a) & !is.na(mm$ka_b))
  cat(sprintf("  %d->%d: %d same-group, %d both have ka, %d ka match (%.0f%% of valid)\n",
      yr, yr+1, nrow(mm), ka_both_valid, ka_match,
      100*ka_match/max(ka_both_valid, 1)))
}

# Show example ka mismatches within same kyoku_group (e.g. 電気局 1942->1943)
cat("\n=== Sample ka values within 電気局 group, 1941->1942 ===\n")
wa <- df %>% filter(year_num == 1941, kyoku_group == "transport") %>%
  select(staff_id, ka_a = ka)
wb <- df %>% filter(year_num == 1942, kyoku_group == "transport") %>%
  select(staff_id, ka_b = ka)
mm <- inner_join(wa, wb, by = "staff_id") %>% filter(!is.na(ka_a) & !is.na(ka_b))
cat("Same ka:", sum(mm$ka_a == mm$ka_b), "/ Diff ka:", sum(mm$ka_a != mm$ka_b), "\n")
mm %>% filter(ka_a != ka_b) %>% count(ka_a, ka_b, sort = TRUE) %>% print(n = 20)

# Show ka name variants within a single kyoku+year
cat("\n=== Ka names within 電気局, by year ===\n")
for (yr in 1940:1943) {
  kas <- df %>% filter(year_num == yr, norm_kyoku == "電気局") %>%
    count(ka, sort = TRUE)
  cat(sprintf("\n%d (%d unique ka):\n", yr, nrow(kas)))
  print(kas, n = 20)
}

# Show ka names for 水道局
cat("\n=== Ka names within 水道局, by year ===\n")
for (yr in 1940:1943) {
  kas <- df %>% filter(year_num == yr, norm_kyoku == "水道局") %>%
    count(ka, sort = TRUE)
  cat(sprintf("\n%d (%d unique ka):\n", yr, nrow(kas)))
  print(kas, n = 15)
}
