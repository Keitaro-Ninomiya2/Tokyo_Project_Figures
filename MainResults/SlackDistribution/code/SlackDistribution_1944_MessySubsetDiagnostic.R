################################################################################
# 1944-only diagnostic on the original messy slack panel.
# Goal: test whether the transfer_share substitution pattern was already present
# in the 1944 subset before destination-side cleanup.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")

panel <- read_csv(
  file.path(result_dir, "slack_distribution_panel.csv"),
  show_col_types = FALSE
) %>%
  filter(year_num == 1944)

run_feols_1944 <- function(data, outcome, ratio = FALSE) {
  rhs <- if (!ratio) {
    "any_slack_same_section + log_slack_same_section + log_slack_same_department + log_slack_different_department + n_drafted_male + log_section_size"
  } else {
    "any_slack_same_section + log_slack_same_section_per_draft + log_slack_same_department_per_draft + log_slack_different_department_per_draft + n_drafted_male + log_section_size"
  }
  fml <- as.formula(paste0(outcome, " ~ ", rhs, " | norm_kyoku"))
  feols(fml, data = data, cluster = ~office_id)
}

models_level <- list(
  transfers_in = run_feols_1944(panel, "transfers_in"),
  new_hires = run_feols_1944(panel, "new_hires"),
  transfer_share = run_feols_1944(panel, "transfer_share")
)

models_ratio <- list(
  transfers_in = run_feols_1944(panel, "transfers_in", ratio = TRUE),
  new_hires = run_feols_1944(panel, "new_hires", ratio = TRUE),
  transfer_share = run_feols_1944(panel, "transfer_share", ratio = TRUE)
)

extract_results <- function(model_list, spec_label) {
  imap_dfr(model_list, function(mod, outcome) {
    tidy(mod) %>%
      mutate(outcome = outcome, specification = spec_label, n = nobs(mod))
  })
}

results <- bind_rows(
  extract_results(models_level, "level_slack_1944_messy"),
  extract_results(models_ratio, "ratio_slack_1944_messy")
) %>%
  filter(term %in% c(
    "any_slack_same_section",
    "log_slack_same_section",
    "log_slack_same_department",
    "log_slack_different_department",
    "log_slack_same_section_per_draft",
    "log_slack_same_department_per_draft",
    "log_slack_different_department_per_draft",
    "n_drafted_male",
    "log_section_size"
  ))

write_csv(
  results,
  file.path(result_dir, "tokyoto_1944_messy_subset_diagnostic_results.csv")
)

cat("1944 messy subset diagnostic:\n")
print(results, n = Inf)
