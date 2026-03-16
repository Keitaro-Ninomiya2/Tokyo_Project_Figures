################################################################################
# Assign kyoku by ground-truth ka order (1951)
#
# The document lists ka in the order of the 1951 org chart.
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

# Load ground truth: ka in document order (from Kyoku_Crosswalk_1951_GroundTruth.csv)
gt <- read_csv(here("Regressions", "Kyoku_Crosswalk_1951_GroundTruth.csv"),
               show_col_types = FALSE) %>%
  filter(nchar(ka) > 0) %>%
  select(kyoku, ka) %>%
  mutate(gt_ord = row_number())

# Load 1951 data
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
df <- df %>% filter(as.numeric(year) == 1951)

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
# Using Unicode escapes to avoid encoding issues when run from CLI
ptrn_eisei <- "\u7523\u9662|\u8a3a\u7642\u6240|\u7642\u990a\u6240|\u75c5\u9662|\u4fdd\u5065\u6240"
ptrn_min   <- "\u990a\u80b2\u9662|\u4fdd\u6bcd\u5b66\u9662"
ptrn_keizai <- "\u58f2\u58f2\u5e02\u5834|\u7af6\u99ac\u4e8b\u52d9\u6240"
kyoku_override <- tibble(
  kyoku_raw = c("\u4eac\u4eac\u90fd\u7acb\u9ad8\u7b49\u4fdd\u6bcd\u5b66\u9662", "\u4e16\u7530\u8c37\u7523\u9662", "\u7af9\u5730\u7523\u9662", "\u8352\u5ddd\u7523\u9662", "\u76e3\u5bdf\u533b\u52d9\u9662", "\u990a\u80b2\u9662", "\u4e2d\u592e\u58f2\u58f2\u5e02\u5834"),
  kyoku_override = c("\u6c11\u751f\u5c40", "\u885b\u751f\u5c40", "\u885b\u751f\u5c40", "\u885b\u751f\u5c40", "\u885b\u751f\u5c40", "\u6c11\u751f\u5c40", "\u7d4c\u6e08\u5c40")
)

office_pos <- office_pos %>%
  left_join(kyoku_override, by = c("kyoku" = "kyoku_raw")) %>%
  mutate(
    is_ward = kyoku %in% ward_kyoku |
      str_detect(ka, ward_pattern) | str_detect(office_norm, ward_pattern) | str_detect(office, ward_pattern),
    override_kyoku = coalesce(
      kyoku_override,
      case_when(
        str_detect(ka, ptrn_eisei) | str_detect(office_norm, ptrn_eisei) | str_detect(office, ptrn_eisei) ~ "\u885b\u751f\u5c40",
        str_detect(ka, ptrn_min) | str_detect(office_norm, ptrn_min) | str_detect(office, ptrn_min) ~ "\u6c11\u751f\u5c40",
        str_detect(ka, ptrn_keizai) | str_detect(office_norm, ptrn_keizai) | str_detect(office, ptrn_keizai) ~ "\u7d4c\u6e08\u5c40",
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
write_csv(office_kyoku_map, here("Regressions", "Office_Kyoku_By_Order_1951.csv"))
cat("Exported office_id → kyoku mapping to Regressions/Office_Kyoku_By_Order_1951.csv\n")
cat("N offices assigned:", nrow(office_kyoku_map), "\n")

# Diagnostics: run_id vs gt
run_summary <- central %>%
  group_by(run_id, gt_idx, kyoku_assigned) %>%
  summarise(n_offices = n(), ka_data = first(ka), .groups = "drop")
cat("\n=== First 30 ka runs (order-based assignment, 1951) ===\n")
print(run_summary %>% head(30), n = 30)
