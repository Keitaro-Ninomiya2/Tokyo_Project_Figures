library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Check if pre-1943 ka names appear as post-1943 kyoku names
# This would confirm the hierarchy shift hypothesis

cat("=== Pre-1943 ka names (1942) ===\n")
ka_42 <- df %>% filter(year_num == 1942, !is.na(ka)) %>% distinct(kyoku, ka) %>% arrange(kyoku, ka)
print(ka_42, n = 60)

cat("\n=== Post-1943 kyoku names (1944) ===\n")
kyoku_44 <- df %>% filter(year_num == 1944, !is.na(kyoku)) %>% distinct(kyoku) %>% arrange(kyoku)
print(kyoku_44, n = 30)

# Check: do 1942 ka names match 1944 kyoku names?
cat("\n=== Ka (1942) that appear as kyoku (1944) ===\n")
overlap <- intersect(ka_42$ka, kyoku_44$kyoku)
cat(paste(overlap, collapse = "\n"), "\n")

# Also check 1941 structure
cat("\n=== Pre-1943 ka names (1941) ===\n")
ka_41 <- df %>% filter(year_num == 1941, !is.na(ka)) %>% distinct(kyoku, ka) %>% arrange(kyoku, ka)
print(ka_41, n = 60)

# What does the hierarchy look like in 1944?
cat("\n=== 1944 hierarchy: kyoku > ka ===\n")
hier_44 <- df %>% filter(year_num == 1944, !is.na(kyoku)) %>%
  distinct(kyoku, ka) %>% arrange(kyoku, ka)
print(hier_44, n = 60)

# Track workers from 1941 to 1944 to see the mapping
cat("\n=== Worker flow: 1941 kyoku+ka -> 1944 kyoku+ka ===\n")
w41 <- df %>% filter(year_num == 1941) %>%
  select(staff_id, kyoku_41 = kyoku, ka_41 = ka)
w44 <- df %>% filter(year_num == 1944) %>%
  select(staff_id, kyoku_44 = kyoku, ka_44 = ka)
matched <- inner_join(w41, w44, by = "staff_id")
cat("Matched 1941->1944:", nrow(matched), "\n\n")
matched %>%
  mutate(unit_41 = paste(kyoku_41, ka_41, sep = " > "),
         unit_44 = paste(kyoku_44, ka_44, sep = " > ")) %>%
  count(unit_41, unit_44, sort = TRUE) %>%
  print(n = 40)
