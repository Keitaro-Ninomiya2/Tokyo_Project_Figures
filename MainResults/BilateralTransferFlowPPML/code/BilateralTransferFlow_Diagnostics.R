################################################################################
# Follow-up diagnostics for bilateral transfer-flow PPML.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
panel <- readRDS(file.path(out_dir, "bilateral_flow_pair_panel_full.rds"))

pair_diag <- panel %>%
  group_by(pair_id) %>%
  summarise(
    n_years = n(),
    years_origin_draft = sum(D_origin > 0),
    years_destination_draft = sum(D_destination > 0),
    years_both_draft = sum(D_origin > 0 & D_destination > 0),
    any_origin_draft = any(D_origin > 0),
    any_destination_draft = any(D_destination > 0),
    origin_draft_status_varies = n_distinct(D_origin > 0) > 1L,
    destination_draft_status_varies = n_distinct(D_destination > 0) > 1L,
    origin_draft_count_values = n_distinct(D_origin),
    destination_draft_count_values = n_distinct(D_destination),
    total_flows = sum(flow_count),
    positive_flow_years = sum(flow_count > 0),
    .groups = "drop"
  )

pair_summary <- bind_rows(
  pair_diag %>% count(metric = "years_origin_draft", value = years_origin_draft, name = "n_pairs"),
  pair_diag %>% count(metric = "years_destination_draft", value = years_destination_draft, name = "n_pairs"),
  pair_diag %>%
    count(metric = "origin_draft_status_varies", value = as.integer(origin_draft_status_varies), name = "n_pairs"),
  pair_diag %>%
    count(metric = "destination_draft_status_varies", value = as.integer(destination_draft_status_varies), name = "n_pairs"),
  pair_diag %>%
    count(metric = "origin_draft_count_values", value = origin_draft_count_values, name = "n_pairs"),
  pair_diag %>%
    count(metric = "destination_draft_count_values", value = destination_draft_count_values, name = "n_pairs")
) %>%
  group_by(metric) %>%
  mutate(share_pairs = n_pairs / sum(n_pairs)) %>%
  ungroup()

write_csv(pair_diag, file.path(out_dir, "bilateral_flow_within_pair_draft_variation_pairs.csv"))
write_csv(pair_summary, file.path(out_dir, "bilateral_flow_within_pair_draft_variation_summary.csv"))

baseline_nondraft <- panel %>%
  filter(D_origin == 0, D_destination == 0)

baseline_year_fe <- fepois(
  flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
    occ_similarity + same_department + different_department | year,
  data = baseline_nondraft,
  cluster = ~ origin_office + destination_office
)

baseline_no_fe <- fepois(
  flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
    occ_similarity + same_department + different_department,
  data = baseline_nondraft,
  cluster = ~ origin_office + destination_office
)

model_table <- function(model, model_name) {
  model_nobs <- as.integer(model$nobs)
  model_pr2 <- as.numeric(fitstat(model, "pr2")[[1L]])
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      model = model_name,
      nobs = model_nobs,
      pseudo_r2 = model_pr2,
      .before = 1
    )
}

gravity_results <- bind_rows(
  model_table(baseline_year_fe, "baseline_year_fe_no_pair_fe"),
  model_table(baseline_no_fe, "baseline_no_fe")
)

write_csv(gravity_results, file.path(out_dir, "bilateral_flow_baseline_no_pair_fe_results.csv"))

cat("\nWithin-pair draft variation summary:\n")
print(pair_summary)

cat("\nNo-pair-FE baseline gravity coefficients:\n")
print(gravity_results %>% select(model, term, estimate, std.error, p.value))
