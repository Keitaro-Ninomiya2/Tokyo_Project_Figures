library(tidyverse)
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv")
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# What kyoku exist by year?
cat("=== Kyoku by year (1937-1945) ===\n")
for (yr in 1937:1945) {
  kyokus <- df %>% filter(year_num == yr) %>% distinct(kyoku) %>% pull(kyoku) %>% sort()
  cat(sprintf("\n%d (%d kyoku): %s\n", yr, length(kyokus), paste(kyokus, collapse = ", ")))
}

# How many workers by kyoku in 1942 vs 1943?
cat("\n\n=== Workers by kyoku: 1942 ===\n")
df %>% filter(year_num == 1942) %>% count(kyoku, sort = TRUE) %>% print(n = 30)

cat("\n=== Workers by kyoku: 1943 ===\n")
df %>% filter(year_num == 1943) %>% count(kyoku, sort = TRUE) %>% print(n = 30)

# Track workers across 1942->1943: where did they go?
cat("\n\n=== Worker movement 1942->1943 by kyoku ===\n")
w42 <- df %>% filter(year_num == 1942) %>% select(staff_id, kyoku_42 = kyoku, ka_42 = ka, kakari_42 = kakari)
w43 <- df %>% filter(year_num == 1943) %>% select(staff_id, kyoku_43 = kyoku, ka_43 = ka, kakari_43 = kakari)
matched <- inner_join(w42, w43, by = "staff_id")
cat("Matched workers:", nrow(matched), "\n\n")

# Kyoku transition matrix
cat("Top kyoku transitions (1942 -> 1943):\n")
matched %>% count(kyoku_42, kyoku_43, sort = TRUE) %>% print(n = 40)

# Ka stability within kyoku transitions
cat("\n\nKa stability for top kyoku transitions:\n")
top_transitions <- matched %>% count(kyoku_42, kyoku_43, sort = TRUE) %>% head(15)
for (i in 1:nrow(top_transitions)) {
  sub <- matched %>% filter(kyoku_42 == top_transitions$kyoku_42[i], kyoku_43 == top_transitions$kyoku_43[i])
  same_ka <- sum(sub$ka_42 == sub$ka_43, na.rm = TRUE)
  cat(sprintf("  %s -> %s: %d workers, %d same ka (%.0f%%)\n",
      top_transitions$kyoku_42[i], top_transitions$kyoku_43[i],
      nrow(sub), same_ka, 100*same_ka/nrow(sub)))
}
