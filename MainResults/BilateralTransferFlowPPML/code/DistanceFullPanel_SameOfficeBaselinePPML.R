################################################################################
# Re-estimate full occupation-level PPML with same-office/different-occupation
# as the omitted distance category.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(
  root_dir, "MainResults", "BilateralTransferFlowPPML", "results",
  "distance_decomposition", "distance_full_panel_within_office"
)

panel_path <- file.path(out_dir, "distance_full_panel_with_within_office_panel.rds")
panel <- readRDS(panel_path)

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
      model = model_name, term = NA_character_, estimate = NA_real_,
      std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, nobs = NA_integer_,
      pseudo_r2 = NA_real_, dispersion = NA_real_, status = "failed"
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
      model = model_name, nobs = model_nobs, pseudo_r2 = pseudo_r2,
      dispersion = dispersion, status = "estimated", .before = 1
    )
}

models <- list(
  col1_dist_full_year_fe = capture_fit(
    "col1_dist_full_year_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku | year,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col2_dist_full_year_occ_fe = capture_fit(
    "col2_dist_full_year_occ_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku |
        year + origin_occupation^destination_occupation,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col3_dist_full_origin_kyoku_fe = capture_fit(
    "col3_dist_full_origin_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku |
        year + origin_kyoku,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col4_dist_full_destination_kyoku_fe = capture_fit(
    "col4_dist_full_destination_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku |
        year + destination_kyoku,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col5_dist_full_origin_ka_fe = capture_fit(
    "col5_dist_full_origin_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku |
        year + origin_ka_fe,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col6_dist_full_destination_ka_fe = capture_fit(
    "col6_dist_full_destination_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_kyoku_diff_ka + diff_kyoku |
        year + destination_ka_fe,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  )
)

results <- imap_dfr(models, ~ model_table(.x, .y))
diagnostics <- results %>%
  distinct(model, status, nobs, pseudo_r2, dispersion) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(results, file.path(out_dir, "distance_full_panel_same_office_baseline_results.csv"))
write_csv(diagnostics, file.path(out_dir, "distance_full_panel_same_office_baseline_model_diagnostics.csv"))

notes <- c(
  "Same-office baseline re-estimation",
  "",
  "Omitted distance category: same office (cleaned kyoku x ka), different occupation.",
  "This is the supported ka-level analogue to a same-kakari/different-occupation baseline.",
  "A literal same-kakari baseline was attempted separately but has no positive flow cells in the kakari-covered sample.",
  "Distance coefficients therefore show log flow intensity relative to within-office occupation changes.",
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)
writeLines(notes, file.path(out_dir, "distance_full_panel_same_office_baseline_notes.txt"))

cat("\nSame-office baseline results:\n")
print(results %>% select(model, term, estimate, std.error, p.value))
cat("\nDiagnostics:\n")
print(diagnostics)
