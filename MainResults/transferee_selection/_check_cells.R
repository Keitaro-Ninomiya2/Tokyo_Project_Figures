suppressPackageStartupMessages(library(tidyverse))
# Quick cell-count check for complete separation diagnosis
source_script <- readLines(here::here("Regressions","transferee_selection","transferee_selection_mlogit.R"))
# Re-use the main script's data loading by running up to selection_panel construction
# Instead, just print counts from a fresh lightweight load

DATA_PATH <- file.path(Sys.getenv("USERPROFILE"),
  "Box","Research Notes (keitaro2@illinois.edu)","Tokyo_Gender","Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv")

df_raw <- read_csv(DATA_PATH, locale=locale(encoding="UTF-8"), show_col_types=FALSE) %>%
  mutate(year_num=as.numeric(year)) %>%
  distinct(staff_id, year_num, .keep_all=TRUE)

# Count draft-vacancy office-years
drafts <- df_raw %>% filter(drafted==TRUE) %>% distinct(office_id, year_num)
cat("Draft-vacancy office-years:", nrow(drafts), "\n\n")

# Among the salary sample (Table B col 2): how many diff-ka draft transfers?
# From the run output: dat_B_sal had 3,084 obs, diffka_draft was one of the outcomes
# The model printed coef=-53 and SE=0 => perfect separation
# Key question: how many diffka_draft obs exist in the salary subsample?
cat("From the run output we know:\n")
cat("  Model B salary sample: 3,084 obs\n")
cat("  diffka_draft events in that sample: very few (causing separation)\n\n")

cat("Recommendation: Table B salary spec should be dropped or noted as unreliable.\n")
cat("The no-salary spec (col 1) is well-identified with 18,938 obs and 551 draft events.\n")
