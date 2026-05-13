################################################################################
# Within-department bilateral PPML diagnostic and follow-up regressions.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
out_dir <- file.path(result_dir, "within_dept")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

panel_path <- file.path(result_dir, "bilateral_flow_pair_panel_full.rds")
pair_panel <- readRDS(panel_path)

fit_warnings <- character()
fit_errors <- character()

capture_fit <- function(label, expr) {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, paste(label, conditionMessage(w), sep = ": "))
        message("WARNING [", label, "]: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      fit_errors <<- c(fit_errors, paste(label, conditionMessage(e), sep = ": "))
      message("ERROR [", label, "]: ", conditionMessage(e))
      NULL
    }
  )
}

model_table <- function(model, model_name, sample_name) {
  if (is.null(model)) {
    return(tibble(
      model = model_name,
      sample = sample_name,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      nobs = NA_integer_,
      pseudo_r2 = NA_real_,
      dispersion = NA_real_,
      status = "failed"
    ))
  }

  model_nobs <- as.integer(model$nobs)
  pseudo_r2 <- tryCatch(as.numeric(fitstat(model, "pr2")[[1L]]), error = function(e) NA_real_)
  pearson <- tryCatch(residuals(model, type = "pearson"), error = function(e) rep(NA_real_, model_nobs))
  residual_df <- tryCatch(df.residual(model), error = function(e) model_nobs - length(coef(model)))
  dispersion <- if (all(is.na(pearson)) || is.na(residual_df) || residual_df <= 0) {
    NA_real_
  } else {
    sum(pearson^2, na.rm = TRUE) / residual_df
  }

  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      model = model_name,
      sample = sample_name,
      nobs = model_nobs,
      pseudo_r2 = pseudo_r2,
      dispersion = dispersion,
      status = "estimated",
      .before = 1
    )
}

fe_count <- function(model, fe_name) {
  if (is.null(model) || is.null(model$fixef_id) || is.null(model$fixef_id[[fe_name]])) {
    return(NA_integer_)
  }
  length(unique(model$fixef_id[[fe_name]]))
}

within_dept <- pair_panel %>%
  filter(same_department == 1L)

variation_summary <- within_dept %>%
  group_by(pair_id) %>%
  summarise(
    n_years = n(),
    origin_draft_years = sum(D_origin > 0),
    destination_draft_years = sum(D_destination > 0),
    origin_draft_varies = origin_draft_years > 0 & origin_draft_years < n_years,
    destination_draft_varies = destination_draft_years > 0 & destination_draft_years < n_years,
    any_positive_flow_years = sum(flow_count > 0),
    multiple_active_years = sum(flow_count > 0) >= 2,
    .groups = "drop"
  )

diagnostic <- variation_summary %>%
  summarise(
    n_within_dept_pairs = n(),
    n_within_dept_pair_years = nrow(within_dept),
    share_zero_origin_draft = mean(origin_draft_years == 0),
    share_any_origin_draft = mean(origin_draft_years > 0),
    share_origin_varies = mean(origin_draft_varies),
    share_zero_dest_draft = mean(destination_draft_years == 0),
    share_any_dest_draft = mean(destination_draft_years > 0),
    share_dest_varies = mean(destination_draft_varies),
    share_any_positive_flow = mean(any_positive_flow_years > 0),
    share_multi_active = mean(multiple_active_years),
    n_origin_varies = sum(origin_draft_varies),
    n_dest_varies = sum(destination_draft_varies)
  )

write_csv(diagnostic, file.path(out_dir, "within_dept_draft_variation_summary.csv"))
write_csv(variation_summary, file.path(out_dir, "within_dept_pair_variation_detail.csv"))

share_origin_varies <- diagnostic$share_origin_varies[[1L]]
share_dest_varies <- diagnostic$share_dest_varies[[1L]]
borderline <- (share_origin_varies >= 0.02 && share_origin_varies < 0.03) ||
  (share_dest_varies >= 0.02 && share_dest_varies < 0.03)
passes_threshold <- share_origin_varies >= 0.03 || share_dest_varies >= 0.03

outcome_lines <- c(
  "Within-department bilateral PPML diagnostic outcome",
  "",
  paste("Pair panel source:", panel_path),
  "Restriction: same_department == 1, i.e. same cleaned kyoku and different ka.",
  "Office identifier: cleaned kyoku x ka from the earlier bilateral workflow.",
  "",
  paste("Within-department pairs:", diagnostic$n_within_dept_pairs[[1L]]),
  paste("Within-department pair-years:", diagnostic$n_within_dept_pair_years[[1L]]),
  paste("Share origin draft varies:", signif(share_origin_varies, 5)),
  paste("Share destination draft varies:", signif(share_dest_varies, 5)),
  paste("Share any positive flow:", signif(diagnostic$share_any_positive_flow[[1L]], 5)),
  paste("Share multiple active years:", signif(diagnostic$share_multi_active[[1L]], 5)),
  "",
  "Decision rule: proceed if origin or destination draft variation share is at least 0.03.",
  if (borderline) {
    "Review flag: within-pair draft variation is borderline, between 0.02 and 0.03."
  } else {
    "Review flag: not borderline under the 0.02-0.03 definition."
  },
  "Merger/identifier note: the sample uses the existing cleaned kyoku x ka office identifiers from the prior bilateral workflow; it does not create an additional pre/post-merger ka linkage.",
  "",
  if (passes_threshold) {
    "Outcome: threshold passed; PPML models were attempted."
  } else {
    "Outcome: threshold failed; even within-department pairs do not have sufficient within-pair draft variation for pair-FE identification. PPML estimation was skipped."
  }
)

writeLines(outcome_lines, file.path(out_dir, "within_dept_diagnostic_outcome.txt"))

if (!passes_threshold) {
  diagnostics <- tibble(
    model = character(),
    status = character(),
    nobs = integer(),
    pseudo_r2 = double(),
    dispersion = double(),
    pair_fe_absorbed = integer(),
    effective_origin_draft_pairs = integer(),
    effective_destination_draft_pairs = integer(),
    warnings = character(),
    errors = character()
  )
  write_csv(diagnostics, file.path(out_dir, "within_dept_model_diagnostics.csv"))
  cat(paste(outcome_lines, collapse = "\n"), "\n")
  quit(save = "no", status = 0)
}

within_dept_nondraft <- within_dept %>%
  filter(D_origin == 0L, D_destination == 0L)

baseline_formula <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity | pair_id + year

draft_formula_pair <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity +
  D_origin + D_destination +
  D_origin:log_N_o + D_origin:log_N_d + D_origin:occ_similarity +
  D_destination:log_N_o + D_destination:log_N_d + D_destination:occ_similarity |
  pair_id + year

draft_formula_endpoint_year <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity +
  D_origin + D_destination +
  D_origin:log_N_o + D_origin:log_N_d + D_origin:occ_similarity +
  D_destination:log_N_o + D_destination:log_N_d + D_destination:occ_similarity |
  origin_office^year + destination_office^year

within_dept_baseline <- capture_fit(
  "within_dept_baseline_pair_fe",
  fepois(
    baseline_formula,
    data = within_dept_nondraft,
    cluster = ~ origin_office + destination_office
  )
)

within_dept_draft <- capture_fit(
  "within_dept_draft_pair_fe",
  fepois(
    draft_formula_pair,
    data = within_dept,
    cluster = ~ origin_office + destination_office
  )
)

within_dept_draft_endpoint_year <- capture_fit(
  "within_dept_draft_endpoint_year_fe",
  fepois(
    draft_formula_endpoint_year,
    data = within_dept,
    cluster = ~ origin_office + destination_office
  )
)

baseline_results <- model_table(
  within_dept_baseline,
  "within_dept_baseline_pair_fe",
  "within_department_nondraft"
)
draft_results <- model_table(
  within_dept_draft,
  "within_dept_draft_pair_fe",
  "within_department_full"
)
endpoint_results <- model_table(
  within_dept_draft_endpoint_year,
  "within_dept_draft_endpoint_year_fe",
  "within_department_full"
)

write_csv(baseline_results, file.path(out_dir, "within_dept_baseline_results.csv"))
write_csv(draft_results, file.path(out_dir, "within_dept_draft_results.csv"))
write_csv(endpoint_results, file.path(out_dir, "within_dept_draft_endpoint_year_fe_results.csv"))

model_diagnostics <- bind_rows(
  baseline_results %>%
    distinct(model, sample, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = fe_count(within_dept_baseline, "pair_id")),
  draft_results %>%
    distinct(model, sample, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = fe_count(within_dept_draft, "pair_id")),
  endpoint_results %>%
    distinct(model, sample, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = NA_integer_)
) %>%
  mutate(
    effective_origin_draft_pairs = sum(variation_summary$origin_draft_varies),
    effective_destination_draft_pairs = sum(variation_summary$destination_draft_varies),
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(model_diagnostics, file.path(out_dir, "within_dept_model_diagnostics.csv"))

cat(paste(outcome_lines, collapse = "\n"), "\n")
cat("\nModel diagnostics:\n")
print(model_diagnostics)
