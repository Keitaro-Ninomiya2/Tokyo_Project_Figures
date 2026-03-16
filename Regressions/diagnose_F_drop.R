# Diagnose why F-stat dropped when adding 1951
# Run after FirstStage_FemaleManager.R has built kakari_panel (or source it with modifications)
library(tidyverse)
library(fixest)

# Minimal reproduction: load data and build est, then compare by year
DATA_PATH <- file.path(Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years_v2.csv")

# We need the full pipeline - source FirstStage but stop before regressions, extract est
# Simpler: re-run key steps and build est_1950, est_1951, est_pooled
source(here::here("Regressions", "FirstStage_FemaleManager.R"), encoding = "UTF-8", 
       local = new.env()) 
# That will run the full script. Instead, let's manually trace through.

# Actually - we need to run FirstStage with a small change to export est by year
# Or run it twice: once with panel_years=1950, once with c(1950,1951)

# Quick approach: create a modified copy that runs both and compares
cat("=== Diagnosing F-stat drop: 1950-only vs 1950+1951 ===\n\n")

# Load the workspace after FirstStage - we don't have that. 
# Instead, add diagnostic code to FirstStage that we can run.
# The simplest: run FirstStage with panel_years=1950, capture output, then with c(1950,1951)

# Let me just add inline diagnostic to a run
# We'll need to run the data prep and then subset est by year
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""),
         is_kakacho = str_detect(pos_norm, "\u4fc2\u9577|4FC2.*9577"))

# Skip full pipeline - just check: for 1950-only sample, what's the corr and coefficient?
# We need the full kakari_panel. Easiest: run FirstStage and add a block that subsets est by year and re-runs.

# Simpler diagnostic without full pipeline:
# The key is: (1) sample size by year, (2) coefficient by year, (3) repeated kakari
cat("Run FirstStage with panel_years=1950, note F and coef.\n")
cat("Then run with panel_years=c(1950,1951), note F and coef.\n")
cat("Compare: if 1951 has weaker/zero relationship, pooling dilutes F.\n")
cat("Also: n_drafted_1944 is time-invariant per kakari - within-kakari obs add noise.\n")
