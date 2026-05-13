################################################################################
# Full occupation-level bilateral PPML including within-office occupation changes.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
distance_dir <- file.path(result_dir, "distance_decomposition")
out_dir <- file.path(distance_dir, "distance_full_panel_within_office")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

worker_path <- file.path(distance_dir, "cleaned_worker_panel_names.rds")
composition_path <- file.path(distance_dir, "office_position_composition_lagged.csv")
pair_panel_path <- file.path(result_dir, "bilateral_flow_pair_panel_full.rds")

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

cat("Constructing occupation-level full distance panel...\n")

office_year_attr <- workers %>%
  filter(year_num %in% 1937:1944, !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  group_by(year = year_num + 1L, office = office_clean) %>%
  summarise(
    kyoku = first(na.omit(norm_kyoku_filled)),
    ka = first(na.omit(ka)),
    .groups = "drop"
  )

office_year_draft <- bind_rows(
  pair_panel %>% distinct(year, office = origin_office, D = D_origin),
  pair_panel %>% distinct(year, office = destination_office, D = D_destination)
) %>%
  group_by(year, office) %>%
  summarise(D = max(D, na.rm = TRUE), .groups = "drop") %>%
  mutate(D = replace_na(D, 0L))

origin_occ <- office_occ %>%
  rename(
    origin_office = office,
    origin_occupation = occupation,
    N_o_occ = N_office_occ,
    log_N_o_occ = log_N_office_occ
  ) %>%
  left_join(
    office_year_attr %>%
      rename(origin_office = office, origin_kyoku = kyoku, origin_ka = ka),
    by = c("year", "origin_office")
  ) %>%
  left_join(
    office_year_draft %>% rename(origin_office = office, D_origin = D),
    by = c("year", "origin_office")
  ) %>%
  mutate(D_origin = replace_na(D_origin, 0L))

destination_occ <- office_occ %>%
  rename(
    destination_office = office,
    destination_occupation = occupation,
    N_d_occ = N_office_occ,
    log_N_d_occ = log_N_office_occ
  ) %>%
  left_join(
    office_year_attr %>%
      rename(destination_office = office, destination_kyoku = kyoku, destination_ka = ka),
    by = c("year", "destination_office")
  ) %>%
  left_join(
    office_year_draft %>% rename(destination_office = office, D_destination = D),
    by = c("year", "destination_office")
  ) %>%
  mutate(D_destination = replace_na(D_destination, 0L))

full_panel <- origin_occ %>%
  inner_join(destination_occ, by = "year", relationship = "many-to-many") %>%
  filter(
    !(origin_office == destination_office & origin_occupation == destination_occupation),
    !is.na(origin_kyoku), !is.na(destination_kyoku),
    D_origin == 0L, D_destination == 0L
  ) %>%
  mutate(
    same_office = as.integer(origin_office == destination_office),
    same_kyoku_diff_ka = as.integer(origin_office != destination_office & origin_kyoku == destination_kyoku),
    diff_kyoku = as.integer(origin_kyoku != destination_kyoku),
    distance_category = case_when(
      same_office == 1L ~ "same_office",
      same_kyoku_diff_ka == 1L ~ "same_kyoku_diff_ka",
      diff_kyoku == 1L ~ "diff_kyoku",
      TRUE ~ NA_character_
    ),
    origin_ka_fe = origin_office,
    destination_ka_fe = destination_office,
    pair_id = paste(origin_office, origin_occupation, "=>", destination_office, destination_occupation),
    office_pair_id = paste(origin_office, destination_office, sep = " => ")
  ) %>%
  filter(!is.na(distance_category), is.finite(log_N_o_occ), is.finite(log_N_d_occ))

lag_workers <- workers %>%
  transmute(
    staff_id,
    year = year_num + 1L,
    origin_office = office_clean,
    origin_occupation = pos_norm
  )

current_workers <- workers %>%
  transmute(
    staff_id,
    year = year_num,
    destination_office = office_clean,
    destination_occupation = pos_norm
  )

flows <- current_workers %>%
  filter(year %in% 1938:1945) %>%
  inner_join(lag_workers, by = c("staff_id", "year")) %>%
  filter(
    !is.na(origin_office), !is.na(destination_office),
    !is.na(origin_occupation), !is.na(destination_occupation),
    !(origin_office == destination_office & origin_occupation == destination_occupation)
  ) %>%
  count(year, origin_office, origin_occupation, destination_office, destination_occupation,
        name = "flow_count")

full_panel <- full_panel %>%
  left_join(
    flows,
    by = c("year", "origin_office", "origin_occupation", "destination_office", "destination_occupation")
  ) %>%
  mutate(flow_count = replace_na(flow_count, 0L))

office_cosine <- pair_panel %>%
  transmute(year, origin_office, destination_office, occ_similarity) %>%
  distinct()

full_panel <- full_panel %>%
  left_join(office_cosine, by = c("year", "origin_office", "destination_office")) %>%
  mutate(
    occ_similarity = case_when(
      same_office == 1L ~ 1,
      is.na(occ_similarity) ~ 0,
      TRUE ~ occ_similarity
    )
  )

write_rds(full_panel, file.path(out_dir, "distance_full_panel_with_within_office_panel.rds"))

distance_diagnostics <- full_panel %>%
  group_by(distance_category) %>%
  summarise(
    n_cells = n(),
    n_office_pairs = n_distinct(office_pair_id),
    n_origin_destination_occ_cells = n_distinct(pair_id),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    transfer_share = total_transfers / sum(full_panel$flow_count),
    mean_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                    mean(flow_count[flow_count > 0]), NA_real_),
    median_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                      as.numeric(median(flow_count[flow_count > 0])), NA_real_),
    max_flow = max(flow_count),
    .groups = "drop"
  )

sample_summary <- full_panel %>%
  summarise(
    n_cells = n(),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    zero_share = mean(flow_count == 0),
    n_origin_offices = n_distinct(origin_office),
    n_destination_offices = n_distinct(destination_office),
    n_origin_occupations = n_distinct(origin_occupation),
    n_destination_occupations = n_distinct(destination_occupation)
  )

write_csv(distance_diagnostics, file.path(out_dir, "distance_full_panel_diagnostics.csv"))
write_csv(sample_summary, file.path(out_dir, "distance_full_panel_sample_summary.csv"))

cat("Estimating full distance PPML models...\n")

models <- list(
  col1_dist_full_year_fe = capture_fit(
    "col1_dist_full_year_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka | year,
      data = full_panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col2_dist_full_year_occ_fe = capture_fit(
    "col2_dist_full_year_occ_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka | year + origin_occupation^destination_occupation,
      data = full_panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col3_dist_full_origin_kyoku_fe = capture_fit(
    "col3_dist_full_origin_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka |
        year + origin_kyoku,
      data = full_panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col4_dist_full_destination_kyoku_fe = capture_fit(
    "col4_dist_full_destination_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka |
        year + destination_kyoku,
      data = full_panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col5_dist_full_origin_ka_fe = capture_fit(
    "col5_dist_full_origin_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka |
        year + origin_ka_fe,
      data = full_panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col6_dist_full_destination_ka_fe = capture_fit(
    "col6_dist_full_destination_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_office + same_kyoku_diff_ka |
        year + destination_ka_fe,
      data = full_panel,
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

write_csv(results, file.path(out_dir, "distance_full_panel_with_within_office_results.csv"))
write_csv(model_diagnostics, file.path(out_dir, "distance_full_panel_model_diagnostics.csv"))

notes <- c(
  "Full occupation-level bilateral PPML including within-office occupation changes",
  "",
  "Unit: origin office x origin pos_norm x destination office x destination pos_norm x year.",
  "Retention is same office and same pos_norm, and is excluded.",
  "same_office is same cleaned kyoku x ka but different occupation.",
  "same_kyoku_diff_ka is different office within the same cleaned kyoku.",
  "diff_kyoku is omitted distance category.",
  "Sample is restricted to non-draft origin and destination offices in year t using office-year draft counts from the prior bilateral pair panel.",
  "",
  "Specification columns:",
  "- Column 1: year FE.",
  "- Column 2: year + origin occupation x destination occupation FE.",
  "- Column 3: year + origin kyoku FE.",
  "- Column 4: year + destination kyoku FE.",
  "- Column 5: year + origin ka FE.",
  "- Column 6: year + destination ka FE.",
  "Both endpoint occupation-size terms and their interaction are retained in every column because they vary at office x occupation level.",
  "occ_similarity is office-level cosine; same-office rows set occ_similarity = 1.",
  "",
  paste("Cells:", sample_summary$n_cells[[1L]]),
  paste("Transfers:", sample_summary$total_transfers[[1L]]),
  paste("Positive cells:", sample_summary$n_positive_cells[[1L]]),
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)
writeLines(notes, file.path(out_dir, "distance_full_panel_specification_notes.txt"))

cat("\nDistance diagnostics:\n")
print(distance_diagnostics)
cat("\nResults:\n")
print(results %>% select(model, term, estimate, std.error, p.value))
cat("\nModel diagnostics:\n")
print(model_diagnostics)
cat("\nFinished. Results written to: ", out_dir, "\n", sep = "")
