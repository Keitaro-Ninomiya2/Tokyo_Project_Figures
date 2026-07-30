################################################################################
# Drafts x nearby slack PPML tests
#
# Unit: draft-impacted destination office x section x year.
# Question: do drafted units rely more on internal transfers, and less on
# external hires, when nearby internal substitute pools are thicker?
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")

panel_path <- file.path(result_dir, "slack_distribution_panel.csv")

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    kyoku_group = replace_na(kyoku_group, "missing"),
    nearby_slack = slack_same_section + slack_same_department,
    log_nearby_slack = log1p(nearby_slack),
    nearby_slack_rate = nearby_slack / pmax(n_workers, 1),
    log_nearby_slack_rate = log1p(nearby_slack_rate),
    log_far_slack = log1p(slack_different_department)
  ) %>%
  filter(
    !is.na(n_drafted_male),
    !is.na(log_nearby_slack),
    !is.na(log_far_slack),
    !is.na(log_section_size)
  )

estimate_ppml <- function(outcome) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ n_drafted_male * log_nearby_slack + log_far_slack + log_section_size | ",
      "year_num + kyoku_group"
    )
  )
  fepois(fml, data = panel, cluster = ~office_id)
}

models <- list(
  internal_transfers = estimate_ppml("transfers_in"),
  external_hires = estimate_ppml("new_hires")
)

share_panel <- panel %>%
  filter(!is.na(transfer_share))

share_model <- feols(
  transfer_share ~ n_drafted_male * log_nearby_slack + log_far_slack + log_section_size |
    year_num + kyoku_group,
  data = share_panel,
  cluster = ~office_id
)

sensitivity_models <- list(
  share_baseline = share_model,
  share_no_size = feols(
    transfer_share ~ n_drafted_male * log_nearby_slack + log_far_slack |
      year_num + kyoku_group,
    data = share_panel,
    cluster = ~office_id
  ),
  share_no_department_fe = feols(
    transfer_share ~ n_drafted_male * log_nearby_slack + log_far_slack + log_section_size |
      year_num,
    data = share_panel,
    cluster = ~office_id
  ),
  share_slack_rate = feols(
    transfer_share ~ n_drafted_male * log_nearby_slack_rate + log_far_slack |
      year_num + kyoku_group,
    data = share_panel,
    cluster = ~office_id
  ),
  share_slack_rate_with_size = feols(
    transfer_share ~ n_drafted_male * log_nearby_slack_rate + log_far_slack + log_section_size |
      year_num + kyoku_group,
    data = share_panel,
    cluster = ~office_id
  )
)

coef_out <- imap_dfr(models, function(model, outcome) {
  tidy(model, conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      nobs = nobs(model),
      pseudo_r2 = fitstat(model, "pr2")[[1]]
    ) %>%
    select(outcome, nobs, pseudo_r2, everything())
})

write.csv(
  coef_out,
  file.path(result_dir, "slack_interaction_ppml_coefficients.csv"),
  row.names = FALSE
)

share_coef_out <- tidy(share_model, conf.int = TRUE) %>%
  mutate(
    outcome = "internal_share",
    nobs = nobs(share_model),
    r2 = fitstat(share_model, "r2")[[1]]
  ) %>%
  select(outcome, nobs, r2, everything())

write.csv(
  share_coef_out,
  file.path(result_dir, "slack_interaction_share_coefficients.csv"),
  row.names = FALSE
)

sensitivity_coef_out <- imap_dfr(sensitivity_models, function(model, model_name) {
  model_nobs <- nobs(model)
  model_r2 <- fitstat(model, "r2")[[1]]
  tidy(model, conf.int = TRUE) %>%
    mutate(
      model = model_name,
      nobs = model_nobs,
      r2 = model_r2
    ) %>%
    select(model, nobs, r2, everything())
}) %>%
  filter(str_detect(term, "n_drafted_male:"))

write.csv(
  sensitivity_coef_out,
  file.path(result_dir, "slack_interaction_share_sensitivity.csv"),
  row.names = FALSE
)

draft_distribution <- panel %>%
  summarise(
    n = n(),
    min_drafts = min(n_drafted_male),
    p25_drafts = unname(quantile(n_drafted_male, 0.25)),
    median_drafts = median(n_drafted_male),
    mean_drafts = mean(n_drafted_male),
    p75_drafts = unname(quantile(n_drafted_male, 0.75)),
    max_drafts = max(n_drafted_male),
    share_one_draft = mean(n_drafted_male == 1),
    share_one_or_two_drafts = mean(n_drafted_male <= 2)
  )

write.csv(
  draft_distribution,
  file.path(result_dir, "slack_interaction_draft_distribution.csv"),
  row.names = FALSE
)

contrast_predictions <- function(model) {
  low_slack <- unname(quantile(panel$nearby_slack, 0.25, na.rm = TRUE))
  high_slack <- unname(quantile(panel$nearby_slack, 0.75, na.rm = TRUE))

  low_data <- panel %>%
    mutate(
      nearby_slack = low_slack,
      log_nearby_slack = log1p(low_slack)
    )

  high_data <- panel %>%
    mutate(
      nearby_slack = high_slack,
      log_nearby_slack = log1p(high_slack)
    )

  low_pred <- predict(model, newdata = low_data, type = "response")
  high_pred <- predict(model, newdata = high_data, type = "response")

  tibble(
    low_nearby_slack_p25 = low_slack,
    high_nearby_slack_p75 = high_slack,
    mean_pred_low_slack = mean(low_pred, na.rm = TRUE),
    mean_pred_high_slack = mean(high_pred, na.rm = TRUE),
    high_minus_low = mean(high_pred - low_pred, na.rm = TRUE),
    ratio_high_to_low = mean(high_pred, na.rm = TRUE) / mean(low_pred, na.rm = TRUE)
  )
}

contrast_out <- imap_dfr(models, function(model, outcome) {
  contrast_predictions(model) %>%
    mutate(outcome = outcome, .before = 1)
})

write.csv(
  contrast_out,
  file.path(result_dir, "slack_interaction_ppml_contrasts.csv"),
  row.names = FALSE
)

cat("\nDrafts x nearby slack PPML coefficients\n")
print(coef_out)

cat("\nLow-vs-high nearby slack prediction contrasts\n")
print(contrast_out)

cat("\nInternal-share OLS coefficients\n")
print(share_coef_out)

cat("\nInternal-share sensitivity: interaction terms\n")
print(sensitivity_coef_out)

cat("\nDraft distribution in slack panel\n")
print(draft_distribution)
