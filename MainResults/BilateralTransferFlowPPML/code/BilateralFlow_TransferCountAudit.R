################################################################################
# Audit actual transfer counts in bilateral pair-year panels.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
out_dir <- file.path(result_dir, "transfer_count_audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

panel <- readRDS(file.path(result_dir, "bilateral_flow_pair_panel_full.rds"))

slice_summary <- function(df, sample_name) {
  positive <- df %>% filter(flow_count > 0)
  tibble(
    sample = sample_name,
    total_pair_years = nrow(df),
    total_pairs = n_distinct(df$pair_id),
    total_transfers = sum(df$flow_count),
    n_positive_pair_years = nrow(positive),
    n_positive_pairs = n_distinct(positive$pair_id),
    positive_pair_year_share = mean(df$flow_count > 0),
    avg_flow_when_positive = if_else(nrow(positive) > 0, sum(positive$flow_count) / nrow(positive), NA_real_),
    median_flow_when_positive = if_else(nrow(positive) > 0, as.numeric(median(positive$flow_count)), NA_real_),
    p90_flow_when_positive = if_else(nrow(positive) > 0, as.numeric(quantile(positive$flow_count, 0.90)), NA_real_),
    p99_flow_when_positive = if_else(nrow(positive) > 0, as.numeric(quantile(positive$flow_count, 0.99)), NA_real_),
    max_flow = if_else(nrow(df) > 0, max(df$flow_count), NA_integer_)
  )
}

samples <- list(
  full_panel = panel,
  nondraft_full_panel = panel %>% filter(D_origin == 0L, D_destination == 0L),
  same_department = panel %>% filter(same_department == 1L),
  same_department_nondraft = panel %>% filter(same_department == 1L, D_origin == 0L, D_destination == 0L),
  different_department = panel %>% filter(different_department == 1L),
  different_department_nondraft = panel %>% filter(different_department == 1L, D_origin == 0L, D_destination == 0L),
  draft_affected_either_endpoint = panel %>% filter(D_origin > 0L | D_destination > 0L)
)

flow_totals <- imap_dfr(samples, ~ slice_summary(.x, .y))

flow_totals_by_year <- imap_dfr(samples, function(df, sample_name) {
  df %>%
    group_by(year) %>%
    summarise(
      sample = sample_name,
      total_pair_years = n(),
      total_pairs = n_distinct(pair_id),
      total_transfers = sum(flow_count),
      n_positive_pair_years = sum(flow_count > 0),
      positive_pair_year_share = mean(flow_count > 0),
      avg_flow_when_positive = if_else(sum(flow_count > 0) > 0,
                                       sum(flow_count) / sum(flow_count > 0),
                                       NA_real_),
      .groups = "drop"
    ) %>%
    relocate(sample, year)
})

positive_flow_distribution <- imap_dfr(samples, function(df, sample_name) {
  df %>%
    filter(flow_count > 0) %>%
    count(flow_count, name = "n_pair_years") %>%
    mutate(sample = sample_name, share_positive_pair_years = n_pair_years / sum(n_pair_years)) %>%
    relocate(sample, flow_count)
})

write_csv(flow_totals, file.path(out_dir, "bilateral_flow_transfer_totals_by_sample.csv"))
write_csv(flow_totals_by_year, file.path(out_dir, "bilateral_flow_transfer_totals_by_sample_year.csv"))
write_csv(positive_flow_distribution, file.path(out_dir, "bilateral_flow_positive_flow_distribution_by_sample.csv"))

cat("\nTransfer totals by sample:\n")
print(flow_totals)

cat("\nTransfer totals by sample-year:\n")
print(flow_totals_by_year)

cat("\nFinished. Audit files written to: ", out_dir, "\n", sep = "")
