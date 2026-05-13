################################################################################
# Within-department hierarchical FE PPML with office x occupation sizes.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
distance_dir <- file.path(
  root_dir, "MainResults", "BilateralTransferFlowPPML", "results", "distance_decomposition"
)
out_dir <- file.path(distance_dir, "hierarchical_fe_occ")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

worker_path <- file.path(distance_dir, "cleaned_worker_panel_names.rds")
composition_path <- file.path(distance_dir, "office_position_composition_lagged.csv")
pair_panel_path <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results",
                             "bilateral_flow_pair_panel_full.rds")

workers <- readRDS(worker_path)
office_occ <- read_csv(composition_path, show_col_types = FALSE) %>%
  transmute(
    lag_year,
    year = lag_year + 1L,
    office = office_unit,
    occupation = pos_norm,
    N_office_occ = n_position,
    log_N_office_occ = log(N_office_occ)
  )

pair_panel <- readRDS(pair_panel_path)

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

cat("Constructing same-department non-draft pair-year-occupation panel...\n")

within_pairs <- pair_panel %>%
  filter(same_department == 1L, D_origin == 0L, D_destination == 0L) %>%
  distinct(
    year, origin_office, destination_office, origin_kyoku, destination_kyoku,
    origin_ka, destination_ka, pair_id, occ_similarity
  ) %>%
  mutate(
    kyoku = origin_kyoku,
    origin_ka_fe = origin_office,
    destination_ka_fe = destination_office
  )

origin_occ <- office_occ %>%
  rename(
    origin_office = office,
    log_N_o_occ = log_N_office_occ,
    N_o_occ = N_office_occ
  )

destination_occ <- office_occ %>%
  rename(
    destination_office = office,
    log_N_d_occ = log_N_office_occ,
    N_d_occ = N_office_occ
  )

within_dept_nondraft_occ <- within_pairs %>%
  inner_join(origin_occ, by = c("year", "origin_office"), relationship = "many-to-many") %>%
  inner_join(destination_occ, by = c("year", "destination_office", "occupation")) %>%
  filter(is.finite(log_N_o_occ), is.finite(log_N_d_occ))

lag_workers <- workers %>%
  transmute(
    staff_id,
    year = year_num + 1L,
    origin_office = office_clean,
    origin_kyoku = norm_kyoku_filled,
    origin_ka = ka,
    occupation = pos_norm
  )

current_workers <- workers %>%
  transmute(
    staff_id,
    year = year_num,
    destination_office = office_clean,
    destination_kyoku = norm_kyoku_filled,
    destination_ka = ka,
    current_occupation = pos_norm
  )

same_occ_flows <- current_workers %>%
  filter(year %in% 1938:1945) %>%
  inner_join(lag_workers, by = c("staff_id", "year")) %>%
  filter(
    !is.na(origin_office), !is.na(destination_office),
    origin_office != destination_office,
    origin_kyoku == destination_kyoku,
    origin_office != destination_office,
    occupation == current_occupation
  ) %>%
  count(year, origin_office, destination_office, occupation, name = "flow_count")

within_dept_nondraft_occ <- within_dept_nondraft_occ %>%
  left_join(same_occ_flows, by = c("year", "origin_office", "destination_office", "occupation")) %>%
  mutate(
    flow_count = replace_na(flow_count, 0L),
    origin_kyoku = kyoku,
    destination_kyoku = kyoku
  )

write_rds(within_dept_nondraft_occ, file.path(out_dir, "within_dept_nondraft_occ_panel.rds"))

diagnostics_occ <- bind_rows(
  office_occ %>%
    summarise(
      diagnostic = "N_office_occ_distribution",
      n_cells = n(),
      mean = mean(N_office_occ),
      median = median(N_office_occ),
      sd = sd(N_office_occ),
      min = min(N_office_occ),
      max = max(N_office_occ),
      share_zero = mean(N_office_occ == 0)
    ),
  office_occ %>%
    group_by(year, office) %>%
    summarise(sd_within_office = sd(N_office_occ), .groups = "drop") %>%
    summarise(
      diagnostic = "within_office_across_occupations_sd",
      n_cells = n(),
      mean = mean(sd_within_office, na.rm = TRUE),
      median = median(sd_within_office, na.rm = TRUE),
      sd = sd(sd_within_office, na.rm = TRUE),
      min = min(sd_within_office, na.rm = TRUE),
      max = max(sd_within_office, na.rm = TRUE),
      share_zero = mean(replace_na(sd_within_office, 0) == 0)
    ),
  within_dept_nondraft_occ %>%
    group_by(year, origin_ka_fe) %>%
    summarise(sd_within_ka = sd(N_o_occ), .groups = "drop") %>%
    summarise(
      diagnostic = "within_origin_ka_panel_sd",
      n_cells = n(),
      mean = mean(sd_within_ka, na.rm = TRUE),
      median = median(sd_within_ka, na.rm = TRUE),
      sd = sd(sd_within_ka, na.rm = TRUE),
      min = min(sd_within_ka, na.rm = TRUE),
      max = max(sd_within_ka, na.rm = TRUE),
      share_zero = mean(replace_na(sd_within_ka, 0) == 0)
    )
)

sample_summary <- within_dept_nondraft_occ %>%
  summarise(
    n_tuple_years = n(),
    n_pairs = n_distinct(pair_id),
    n_occupations = n_distinct(occupation),
    n_positive_tuple_years = sum(flow_count > 0),
    total_same_occ_transfers = sum(flow_count),
    zero_share = mean(flow_count == 0),
    mean_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                    mean(flow_count[flow_count > 0]), NA_real_),
    max_flow = max(flow_count)
  )

write_csv(diagnostics_occ, file.path(out_dir, "hierarchical_fe_occ_diagnostics.csv"))
write_csv(sample_summary, file.path(out_dir, "hierarchical_fe_occ_sample_summary.csv"))

models <- list(
  col1_year_fe = capture_fit(
    "col1_year_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  ),
  col2_year_kyoku_fe = capture_fit(
    "col2_year_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year + kyoku,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  ),
  col3_year_origin_kyoku_fe = capture_fit(
    "col3_year_origin_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year + origin_kyoku,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  ),
  col4_year_destination_kyoku_fe = capture_fit(
    "col4_year_destination_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year + destination_kyoku,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  ),
  col5_year_origin_ka_fe = capture_fit(
    "col5_year_origin_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year + origin_ka_fe,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  ),
  col6_year_destination_ka_fe = capture_fit(
    "col6_year_destination_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity | year + destination_ka_fe,
      data = within_dept_nondraft_occ,
      cluster = ~ origin_office + destination_office
    )
  )
)

results <- imap_dfr(models, ~ model_table(.x, .y))

model_diagnostics <- results %>%
  distinct(model, status, nobs, pseudo_r2, dispersion) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(results, file.path(out_dir, "hierarchical_fe_occ_results.csv"))
write_csv(model_diagnostics, file.path(out_dir, "hierarchical_fe_occ_model_diagnostics.csv"))

notes <- c(
  "Within-department hierarchical FE PPML with office x occupation sizes",
  "",
  "Sample: same-department non-draft pair-year-position tuples.",
  "Occupation is pos_norm. Same-occupation transfers require lag and current pos_norm equality.",
  "Office is cleaned kyoku x ka. Kakari is not used.",
  "Size variables are non-drafted lagged office x pos_norm counts.",
  "occ_similarity remains the office-level cosine similarity from the earlier pair panel.",
  "",
  paste("Tuple-year observations:", sample_summary$n_tuple_years[[1L]]),
  paste("Same-occupation transfers:", sample_summary$total_same_occ_transfers[[1L]]),
  paste("Positive tuple-years:", sample_summary$n_positive_tuple_years[[1L]]),
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)
writeLines(notes, file.path(out_dir, "hierarchical_fe_occ_specification_notes.txt"))

cat("\nSample summary:\n")
print(sample_summary)
cat("\nResults:\n")
print(results %>% select(model, term, estimate, std.error, p.value))
cat("\nModel diagnostics:\n")
print(model_diagnostics)
cat("\nFinished. Results written to: ", out_dir, "\n", sep = "")
