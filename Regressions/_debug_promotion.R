library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

drafted_ids <- df_all %>% filter(drafted == TRUE) %>% distinct(staff_id) %>% pull(staff_id)

# Step 1: How many drafted at office x pos level?
draft_years <- c(1938, 1940, 1941, 1943, 1944, 1948)
position_drafts <- df_all %>%
  filter(year_num %in% draft_years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE), .groups = "drop")

cat("=== Position-level draft counts ===\n")
cat("Total office x pos x year combos with drafts:", sum(position_drafts$n_drafted_male > 0), "\n")
cat("By year:\n")
position_drafts %>% filter(n_drafted_male > 0) %>% count(year_num) %>% print()

# Step 2: Build the panel for 1944 only as test
t <- 1944
workers_prev <- df %>%
  filter(year_num == t - 1, !(staff_id %in% drafted_ids)) %>%
  select(staff_id, office_id, pos_norm, is_female, ka, kyoku)
workers_curr <- df %>%
  filter(year_num == t) %>%
  select(staff_id, office_id_t = office_id)

matched <- workers_prev %>%
  inner_join(workers_curr, by = "staff_id") %>%
  filter(office_id == office_id_t)

cat("\n=== 1944 panel (workers in both 1943 and 1944, same office) ===\n")
cat("Workers matched:", nrow(matched), "\n")

# Join drafts
matched2 <- matched %>%
  left_join(position_drafts %>% filter(year_num == 1944),
            by = c("office_id", "pos_norm")) %>%
  mutate(n_drafted_male = replace_na(n_drafted_male, 0),
         ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_))

cat("With any drafts at their office x pos:", sum(matched2$n_drafted_male > 0), "\n")
cat("With ka_id:", sum(!is.na(matched2$ka_id)), "\n")
cat("With ka_id AND drafts:", sum(!is.na(matched2$ka_id) & matched2$n_drafted_male > 0), "\n")

# What offices have drafts?
drafted_offices <- position_drafts %>% filter(year_num == 1944, n_drafted_male > 0) %>%
  distinct(office_id) %>% pull(office_id)
cat("\nOffices with any drafted in 1944:", length(drafted_offices), "\n")
cat("Workers in panel at those offices:", sum(matched2$office_id %in% drafted_offices), "\n")
cat("Workers in panel at those offices with ka_id:",
    sum(matched2$office_id %in% drafted_offices & !is.na(matched2$ka_id)), "\n")

# Check: are drafted workers' positions matching panel workers' positions?
cat("\n=== Positions of drafted vs panel workers ===\n")
drafted_pos <- position_drafts %>% filter(year_num == 1944, n_drafted_male > 0) %>%
  distinct(pos_norm) %>% pull(pos_norm)
panel_pos <- matched2 %>% distinct(pos_norm) %>% pull(pos_norm)
cat("Drafted positions:", length(drafted_pos), "\n")
cat("Panel positions:", length(panel_pos), "\n")
cat("Overlap:", length(intersect(drafted_pos, panel_pos)), "\n")
cat("\nDrafted positions:\n")
cat(drafted_pos, sep="\n")
cat("\n\nSample panel positions:\n")
cat(head(panel_pos, 20), sep="\n")

# The issue might be: drafts are counted from df_all (including non is_name),
# but panel workers are from df (is_name only). Check if drafted workers' pos_norm
# differs when is_name is TRUE vs FALSE
cat("\n=== Drafted worker pos_norm in df_all vs df ===\n")
drafted_in_all <- df_all %>% filter(year_num == 1944, drafted == TRUE)
drafted_in_df <- df %>% filter(year_num == 1944, drafted == TRUE)
cat("Drafted in df_all (1944):", nrow(drafted_in_all), "\n")
cat("Drafted in df (is_name, 1944):", nrow(drafted_in_df), "\n")
cat("\nDrafted pos_norm in df_all:\n")
drafted_in_all %>% count(pos_norm, sort = TRUE) %>% head(15) %>% print()
cat("\nDrafted pos_norm in df (is_name):\n")
drafted_in_df %>% count(pos_norm, sort = TRUE) %>% head(15) %>% print()
