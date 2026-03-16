################################################################################
# Assign kyoku by ground-truth ka order (1950)
#
# The document lists ka in the order of the 1950 org chart:
# 総務局 (10 ka) → 財務局 (8) → 民生局 (7) → 経済局 (9) → 建設局 (10) →
# 交通局 (6) → 水道局 (5) → 衛生局 (8) → 労働局 (4) → 建築局 (4) → 出納長室
#
# Strategy: Order offices by (page, image, x, y). Group into "ka runs" (consecutive
# offices with same ka). Assign run i → gt_ka[i] → kyoku.
# Ward offices (区役所, 支所, etc.) → 区部.
################################################################################

library(tidyverse)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

# Load ground truth: ka in document order (from Kyoku_Crosswalk_1950_GroundTruth.csv)
gt <- read_csv(here("Regressions", "Kyoku_Crosswalk_1950_GroundTruth.csv"),
               show_col_types = FALSE) %>%
  filter(nchar(ka) > 0) %>%
  select(kyoku, ka) %>%
  mutate(gt_ord = row_number())

# Load 1950 data
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
df <- df %>% filter(as.numeric(year) == 1950)

# Document order: (page, image, x, y)
# Coerce page to numeric; use image as string for tie-break
df <- df %>%
  mutate(
    page_num = as.numeric(page),
    x_num = as.numeric(x),
    y_num = as.numeric(y)
  )

# Office-level: one row per office_id; use first occurrence for position
office_pos <- df %>%
  filter(!is.na(office_id)) %>%
  group_by(office_id) %>%
  summarise(
    kyoku = first(kyoku),
    ka = first(na.omit(ka)),
    office = first(office),
    office_norm = first(office_norm),
    page_num = min(page_num, na.rm = TRUE),
    x_num = min(x_num, na.rm = TRUE),
    y_num = min(y_num, na.rm = TRUE),
    image = first(image),
    .groups = "drop"
  )

# Ward: (1) kyoku is ward-level, or (2) ka/office matches ward pattern
ward_kyoku <- c("マ〓議普導務局", "区議会事務局", "区議〓〓務局", "区議留経務局", "区難留導務局",
                "区智事務局", "会事務局", "区会事務局", "谷区選擧管理委員会事務局")
ward_pattern <- "区役所|支所|出張所|地方事務所|税務課|民生課|教育課|戸籍課|土木課|区民課|区政課|収入役|監査委員室|厚生課|経済課|産業課"

# Known entity overrides (appear in non-standard doc order)
override_衛生局 <- "産院|診療所|療養所|病院|保健所"
override_民生局 <- "養育院|保母学院"
override_経済局 <- "卸売市場|競馬事務所"
# Kyoku-level fallbacks (OCR'd kyoku names)
kyoku_override <- tibble(
  kyoku_raw = c("京京都立高等保母学院", "世田谷産院", "築地産院", "荒川産院", "監察医務院", "養育院", "中央卸売市場"),
  kyoku_override = c("民生局", "衛生局", "衛生局", "衛生局", "衛生局", "民生局", "経済局")
)

office_pos <- office_pos %>%
  left_join(kyoku_override, by = c("kyoku" = "kyoku_raw")) %>%
  mutate(
    is_ward = kyoku %in% ward_kyoku |
      str_detect(ka, ward_pattern) | str_detect(office_norm, ward_pattern) | str_detect(office, ward_pattern),
    override_kyoku = coalesce(
      kyoku_override,
      case_when(
        str_detect(ka, override_衛生局) | str_detect(office_norm, override_衛生局) | str_detect(office, override_衛生局) ~ "衛生局",
        str_detect(ka, override_民生局) | str_detect(office_norm, override_民生局) | str_detect(office, override_民生局) ~ "民生局",
        str_detect(ka, override_経済局) | str_detect(office_norm, override_経済局) | str_detect(office, override_経済局) ~ "経済局",
        TRUE ~ NA_character_
      )
    )
  ) %>%
  select(-kyoku_override)

# Split: wards → 区部; override → use override_kyoku; rest → order-based
wards <- office_pos %>% filter(is_ward)
override_offices <- office_pos %>% filter(!is_ward, !is.na(override_kyoku))
central <- office_pos %>% filter(!is_ward, is.na(override_kyoku))

# Order central offices by document position
central <- central %>%
  arrange(page_num, image, x_num, y_num)

# Ka runs: when ka changes, new run (use ka from data; may be OCR'd)
central <- central %>%
  mutate(ka_fill = replace_na(ka, "")) %>%
  mutate(
    ka_changed = ka_fill != lag(ka_fill) | is.na(lag(ka_fill)),
    run_id = cumsum(replace_na(ka_changed, TRUE))
  ) %>%
  select(-ka_fill)

# Assign run_id → gt ka (by position)
# Run 1 → gt[1], Run 2 → gt[2], ... ; if more runs than gt, use last kyoku
n_gt <- nrow(gt)
central <- central %>%
  mutate(gt_idx = pmin(run_id, n_gt)) %>%
  left_join(gt %>% select(gt_ord, kyoku_assigned = kyoku), by = c("gt_idx" = "gt_ord"))

# Build office_id → kyoku mapping
kyoku_ward <- wards %>% select(office_id) %>% mutate(kyoku_assigned = "区部")
kyoku_override <- override_offices %>% select(office_id, kyoku_assigned = override_kyoku)
kyoku_central <- central %>% select(office_id, kyoku_assigned) %>% filter(!is.na(kyoku_assigned)) %>% distinct()

office_kyoku_map <- bind_rows(
  kyoku_ward,
  kyoku_override,
  kyoku_central
) %>%
  group_by(office_id) %>%
  slice(1) %>%
  ungroup()

# Export
write_csv(office_kyoku_map, here("Regressions", "Office_Kyoku_By_Order_1950.csv"))
cat("Exported office_id → kyoku mapping to Regressions/Office_Kyoku_By_Order_1950.csv\n")
cat("N offices assigned:", nrow(office_kyoku_map), "\n")

# Diagnostics: run_id vs gt
run_summary <- central %>%
  group_by(run_id, gt_idx, kyoku_assigned) %>%
  summarise(n_offices = n(), ka_data = first(ka), .groups = "drop")
cat("\n=== First 30 ka runs (order-based assignment) ===\n")
print(run_summary %>% head(30), n = 30)
