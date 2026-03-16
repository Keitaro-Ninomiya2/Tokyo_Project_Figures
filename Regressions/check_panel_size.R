library(tidyverse)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year),
         pos_norm = str_replace_all(position, "\\s+", ""))

cat("Total named worker-years:", nrow(df), "\n")
cat("Years available:", sort(unique(df$year_num)), "\n")
cat("Offices in 1944:", df %>% filter(year_num == 1944) %>% distinct(office_id) %>% nrow(), "\n")

# Panel: office x kakari x pos_norm x year for 1938-1945
p <- df %>%
  filter(year_num %in% 1938:1945) %>%
  distinct(office_id, kakari, pos_norm, year_num)
cat("\nPanel cells (office x kakari x pos x year), 1938-1945:", nrow(p), "\n")
cat("Unique offices:", n_distinct(p$office_id), "\n")
cat("Unique kakari:", n_distinct(p$kakari), "\n")
cat("Unique pos_norm:", n_distinct(p$pos_norm), "\n")

# Restricted to 1944-observable offices
offices_1944 <- df %>% filter(year_num == 1944) %>% distinct(office_id)
pr <- p %>% semi_join(offices_1944, by = "office_id")
cat("Restricted to 1944 offices:", nrow(pr), "\n")

# Table 1 uses all offices (not restricted), so 11,758 is the full panel
# But the question is whether this includes offices not in 1944

# Check: how many offices in 1938-1945 are NOT in 1944?
all_offices <- df %>% filter(year_num %in% 1938:1945) %>% distinct(office_id)
not_in_1944 <- all_offices %>% anti_join(offices_1944, by = "office_id")
cat("\nOffices in 1938-1945 panel NOT in 1944:", nrow(not_in_1944), "\n")
cat("Offices in 1938-1945 panel IN 1944:", nrow(all_offices) - nrow(not_in_1944), "\n")

# The regression panel from Table 1 uses ALL offices in 1938-1945
# The treatment is n_drafted which is only nonzero for 1944 obs
# So it's really comparing offices with drafting in a given year vs not
# Let me check: is drafted ever TRUE outside 1944?
cat("\nDrafted by year:\n")
df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year))
print(df_all %>% filter(drafted == TRUE) %>% count(year_num))
