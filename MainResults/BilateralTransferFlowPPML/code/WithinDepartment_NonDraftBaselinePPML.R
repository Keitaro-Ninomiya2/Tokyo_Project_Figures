################################################################################
# Within-department non-draft baseline bilateral PPML.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
out_dir <- file.path(result_dir, "within_dept_nondraft")
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

model_table <- function(model, model_name) {
  if (is.null(model)) {
    return(tibble(
      model = model_name,
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

within_pair_variation <- function(df, var) {
  df %>%
    group_by(pair_id) %>%
    summarise(
      n_years = n(),
      n_distinct_value = n_distinct(.data[[var]]),
      sd_value = sd(.data[[var]], na.rm = TRUE),
      range_value = max(.data[[var]], na.rm = TRUE) - min(.data[[var]], na.rm = TRUE),
      varies = n_distinct_value > 1L,
      .groups = "drop"
    ) %>%
    mutate(variable = var, .before = 1)
}

within_dept_nondraft <- pair_panel %>%
  filter(same_department == 1L, D_origin == 0L, D_destination == 0L)

sample_summary <- within_dept_nondraft %>%
  summarise(
    n_pair_years = n(),
    n_pairs = n_distinct(pair_id),
    n_origin_offices = n_distinct(origin_office),
    n_destination_offices = n_distinct(destination_office),
    zero_flow_share = mean(flow_count == 0),
    positive_flow_share = mean(flow_count > 0),
    total_flows = sum(flow_count),
    mean_flow = mean(flow_count),
    max_flow = max(flow_count),
    min_flow = min(flow_count),
    p50_flow = as.numeric(quantile(flow_count, 0.50)),
    p90_flow = as.numeric(quantile(flow_count, 0.90)),
    p99_flow = as.numeric(quantile(flow_count, 0.99)),
    n_pairs_with_positive_flow = n_distinct(pair_id[flow_count > 0]),
    share_pairs_with_positive_flow = n_distinct(pair_id[flow_count > 0]) / n_distinct(pair_id),
    draft_origin_pair_years = sum(D_origin > 0),
    draft_destination_pair_years = sum(D_destination > 0),
    same_department_verification = all(same_department == 1L),
    nondraft_verification = all(D_origin == 0L & D_destination == 0L)
  )

flow_distribution <- within_dept_nondraft %>%
  count(flow_count, name = "n_pair_years") %>%
  mutate(share = n_pair_years / sum(n_pair_years))

covariate_pair_variation <- bind_rows(
  within_pair_variation(within_dept_nondraft, "log_N_o"),
  within_pair_variation(within_dept_nondraft, "log_N_d"),
  within_pair_variation(within_dept_nondraft, "occ_similarity")
)

covariate_variation_summary <- covariate_pair_variation %>%
  group_by(variable) %>%
  summarise(
    n_pairs = n(),
    share_pairs_varying = mean(varies),
    mean_distinct_values = mean(n_distinct_value),
    median_distinct_values = median(n_distinct_value),
    mean_sd = mean(sd_value, na.rm = TRUE),
    median_sd = median(sd_value, na.rm = TRUE),
    mean_range = mean(range_value, na.rm = TRUE),
    median_range = median(range_value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(sample_summary, file.path(out_dir, "within_dept_nondraft_sample_summary.csv"))
write_csv(flow_distribution, file.path(out_dir, "within_dept_nondraft_flow_distribution.csv"))
write_csv(covariate_pair_variation, file.path(out_dir, "within_dept_nondraft_within_pair_covariate_variation_detail.csv"))
write_csv(covariate_variation_summary, file.path(out_dir, "within_dept_nondraft_within_pair_covariate_variation_summary.csv"))

pair_fe_formula <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity | pair_id + year

endpoint_fe_formula <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity | origin_office^year + destination_office^year

year_fe_formula <- flow_count ~ log_N_o + I(log_N_o^2) + log_N_d + I(log_N_d^2) +
  occ_similarity | year

cat("Estimating within-department non-draft baseline models...\n")

pair_fe_model <- capture_fit(
  "within_dept_nondraft_pairFE",
  fepois(pair_fe_formula, data = within_dept_nondraft,
         cluster = ~ origin_office + destination_office)
)

endpoint_fe_model <- capture_fit(
  "within_dept_nondraft_endpointFE",
  fepois(endpoint_fe_formula, data = within_dept_nondraft,
         cluster = ~ origin_office + destination_office)
)

year_fe_model <- capture_fit(
  "within_dept_nondraft_yearFE",
  fepois(year_fe_formula, data = within_dept_nondraft,
         cluster = ~ origin_office + destination_office)
)

pair_fe_results <- model_table(pair_fe_model, "within_dept_nondraft_pairFE")
endpoint_fe_results <- model_table(endpoint_fe_model, "within_dept_nondraft_endpointFE")
year_fe_results <- model_table(year_fe_model, "within_dept_nondraft_yearFE")

write_csv(pair_fe_results, file.path(out_dir, "within_dept_nondraft_pairFE_results.csv"))
write_csv(endpoint_fe_results, file.path(out_dir, "within_dept_nondraft_endpointFE_results.csv"))
write_csv(year_fe_results, file.path(out_dir, "within_dept_nondraft_yearFE_results.csv"))

model_diagnostics <- bind_rows(
  pair_fe_results %>%
    distinct(model, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = fe_count(pair_fe_model, "pair_id")),
  endpoint_fe_results %>%
    distinct(model, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = NA_integer_),
  year_fe_results %>%
    distinct(model, status, nobs, pseudo_r2, dispersion) %>%
    mutate(pair_fe_absorbed = NA_integer_)
) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(model_diagnostics, file.path(out_dir, "within_dept_nondraft_model_diagnostics.csv"))

notes <- c(
  "Within-department non-draft baseline PPML notes",
  "",
  paste("Pair panel source:", panel_path),
  "Sample: same_department == 1 and D_origin == 0 and D_destination == 0.",
  "Same-department means same cleaned kyoku and different ka, inherited from the earlier bilateral workflow.",
  "Office identifier remains cleaned kyoku x ka; no additional pre/post-merger ka linkage was imposed.",
  "",
  paste("Pair-years:", sample_summary$n_pair_years[[1L]]),
  paste("Pairs:", sample_summary$n_pairs[[1L]]),
  paste("Zero-flow share:", signif(sample_summary$zero_flow_share[[1L]], 5)),
  paste("Pairs with positive flows:", sample_summary$n_pairs_with_positive_flow[[1L]]),
  "",
  "Within-pair covariate variation is saved in:",
  "- within_dept_nondraft_within_pair_covariate_variation_summary.csv",
  "- within_dept_nondraft_within_pair_covariate_variation_detail.csv",
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)

writeLines(notes, file.path(out_dir, "within_dept_nondraft_specification_notes.txt"))

cat("\nSample summary:\n")
print(sample_summary)
cat("\nWithin-pair covariate variation summary:\n")
print(covariate_variation_summary)
cat("\nModel diagnostics:\n")
print(model_diagnostics)
cat("\nFinished. Results written to: ", out_dir, "\n", sep = "")
