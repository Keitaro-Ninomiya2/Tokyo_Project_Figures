################################################################################
# Occupation-level PPML with same-kakari/different-occupation as baseline.
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
out_dir <- file.path(distance_dir, "distance_full_panel_kakari_baseline")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

workers_names <- readRDS(file.path(distance_dir, "cleaned_worker_panel_names.rds"))
workers_all <- readRDS(file.path(distance_dir, "cleaned_worker_panel_all.rds"))

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

prep_workers <- function(df) {
  df %>%
    mutate(
      kakari_clean = str_squish(replace_na(as.character(kakari), "")),
      kakari_clean = na_if(kakari_clean, ""),
      unit = if_else(!is.na(office_clean) & !is.na(kakari_clean),
                     paste(office_clean, kakari_clean, sep = " :: "),
                     NA_character_)
    )
}

workers_names <- prep_workers(workers_names)
workers_all <- prep_workers(workers_all)

cat("Constructing kakari-level occupation panel...\n")

unit_occ <- workers_all %>%
  filter(
    year_num %in% 1937:1944,
    !is.na(unit),
    !is.na(pos_norm),
    is.na(drafted) | drafted != TRUE
  ) %>%
  count(
    year = year_num + 1L,
    unit,
    office = office_clean,
    kyoku = norm_kyoku_filled,
    ka,
    occupation = pos_norm,
    name = "N_occ"
  ) %>%
  mutate(log_N_occ = log1p(N_occ))

unit_attr <- workers_all %>%
  filter(year_num %in% 1937:1944, !is.na(unit), !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  group_by(year = year_num + 1L, unit) %>%
  summarise(
    office = first(na.omit(office_clean)),
    kyoku = first(na.omit(norm_kyoku_filled)),
    ka = first(na.omit(ka)),
    .groups = "drop"
  )

unit_attr_current <- workers_all %>%
  filter(year_num %in% 1938:1945, !is.na(unit), !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  group_by(year = year_num, unit) %>%
  summarise(
    office = first(na.omit(office_clean)),
    kyoku = first(na.omit(norm_kyoku_filled)),
    ka = first(na.omit(ka)),
    .groups = "drop"
  )

unit_year_draft <- workers_all %>%
  filter(year_num %in% 1938:1945, !is.na(unit), drafted == TRUE, !is_female) %>%
  count(year = year_num, unit, name = "D_unit")

year_occupations <- unit_occ %>%
  distinct(year, occupation)

origin_occ <- unit_occ %>%
  rename(
    origin_unit = unit,
    origin_office = office,
    origin_kyoku = kyoku,
    origin_ka = ka,
    origin_occupation = occupation,
    N_o_occ = N_occ,
    log_N_o_occ = log_N_occ
  ) %>%
  left_join(unit_year_draft %>% rename(origin_unit = unit, D_origin = D_unit),
            by = c("year", "origin_unit")) %>%
  mutate(D_origin = replace_na(D_origin, 0L))

destination_occ <- unit_attr_current %>%
  inner_join(year_occupations, by = "year", relationship = "many-to-many") %>%
  left_join(
    unit_occ %>%
      select(year, unit, occupation, N_occ, log_N_occ),
    by = c("year", "unit", "occupation")
  ) %>%
  mutate(
    N_occ = replace_na(N_occ, 0L),
    log_N_occ = log1p(N_occ)
  ) %>%
  rename(
    destination_unit = unit,
    destination_office = office,
    destination_kyoku = kyoku,
    destination_ka = ka,
    destination_occupation = occupation,
    N_d_occ = N_occ,
    log_N_d_occ = log_N_occ
  ) %>%
  left_join(unit_year_draft %>% rename(destination_unit = unit, D_destination = D_unit),
            by = c("year", "destination_unit")) %>%
  mutate(D_destination = replace_na(D_destination, 0L))

panel <- origin_occ %>%
  inner_join(destination_occ, by = "year", relationship = "many-to-many") %>%
  filter(
    !(origin_unit == destination_unit & origin_occupation == destination_occupation),
    D_origin == 0L,
    D_destination == 0L
  ) %>%
  mutate(
    same_kakari_diff_occ = as.integer(origin_unit == destination_unit &
                                        origin_occupation != destination_occupation),
    same_ka_diff_kakari = as.integer(origin_office == destination_office &
                                       origin_unit != destination_unit),
    same_kyoku_diff_ka = as.integer(origin_office != destination_office &
                                      origin_kyoku == destination_kyoku),
    diff_kyoku = as.integer(origin_kyoku != destination_kyoku),
    distance_category = case_when(
      same_kakari_diff_occ == 1L ~ "same_kakari_diff_occ",
      same_ka_diff_kakari == 1L ~ "same_ka_diff_kakari",
      same_kyoku_diff_ka == 1L ~ "same_kyoku_diff_ka",
      diff_kyoku == 1L ~ "diff_kyoku",
      TRUE ~ NA_character_
    ),
    origin_ka_fe = origin_office,
    destination_ka_fe = destination_office,
    origin_kakari_fe = origin_unit,
    destination_kakari_fe = destination_unit
  ) %>%
  filter(!is.na(distance_category))

unit_shares <- unit_occ %>%
  group_by(year, unit) %>%
  mutate(share = N_occ / sum(N_occ), norm = sqrt(sum(share^2))) %>%
  ungroup() %>%
  select(year, unit, occupation, share, norm)

unit_similarity <- unit_shares %>%
  select(year, occupation, origin_unit = unit, origin_share = share, origin_norm = norm) %>%
  inner_join(
    unit_shares %>%
      select(year, occupation, destination_unit = unit, destination_share = share, destination_norm = norm),
    by = c("year", "occupation"),
    relationship = "many-to-many"
  ) %>%
  group_by(year, origin_unit, destination_unit) %>%
  summarise(
    dot = sum(origin_share * destination_share),
    origin_norm = first(origin_norm),
    destination_norm = first(destination_norm),
    occ_similarity = dot / (origin_norm * destination_norm),
    .groups = "drop"
  )

lag_workers <- workers_names %>%
  transmute(
    staff_id,
    year = year_num + 1L,
    origin_unit = unit,
    origin_office = office_clean,
    origin_occupation = pos_norm
  )

current_workers <- workers_names %>%
  transmute(
    staff_id,
    year = year_num,
    destination_unit = unit,
    destination_office = office_clean,
    destination_occupation = pos_norm
  )

flows <- current_workers %>%
  filter(year %in% 1938:1945) %>%
  inner_join(lag_workers, by = c("staff_id", "year")) %>%
  filter(
    !is.na(origin_unit), !is.na(destination_unit),
    !is.na(origin_occupation), !is.na(destination_occupation),
    !(origin_unit == destination_unit & origin_occupation == destination_occupation)
  ) %>%
  count(year, origin_unit, origin_occupation, destination_unit, destination_occupation,
        name = "flow_count")

panel <- panel %>%
  left_join(
    flows,
    by = c("year", "origin_unit", "origin_occupation", "destination_unit", "destination_occupation")
  ) %>%
  left_join(unit_similarity, by = c("year", "origin_unit", "destination_unit")) %>%
  mutate(
    flow_count = replace_na(flow_count, 0L),
    occ_similarity = if_else(origin_unit == destination_unit, 1, replace_na(occ_similarity, 0))
  )

write_rds(panel, file.path(out_dir, "distance_full_panel_kakari_baseline_panel.rds"))

distance_diagnostics <- panel %>%
  group_by(distance_category) %>%
  summarise(
    n_cells = n(),
    n_origin_destination_unit_cells = n_distinct(paste(origin_unit, destination_unit)),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    transfer_share = total_transfers / sum(panel$flow_count),
    mean_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                    mean(flow_count[flow_count > 0]), NA_real_),
    median_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                      as.numeric(median(flow_count[flow_count > 0])), NA_real_),
    max_flow = max(flow_count),
    .groups = "drop"
  )

sample_summary <- panel %>%
  summarise(
    n_cells = n(),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    zero_share = mean(flow_count == 0),
    n_origin_units = n_distinct(origin_unit),
    n_destination_units = n_distinct(destination_unit),
    n_origin_occupations = n_distinct(origin_occupation),
    n_destination_occupations = n_distinct(destination_occupation)
  )

write_csv(distance_diagnostics, file.path(out_dir, "distance_full_panel_kakari_baseline_diagnostics.csv"))
write_csv(sample_summary, file.path(out_dir, "distance_full_panel_kakari_baseline_sample_summary.csv"))

cat("Estimating kakari-baseline PPML models...\n")

models <- list(
  col1_year_fe = capture_fit(
    "col1_year_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku | year,
      data = panel,
      cluster = ~ origin_unit + destination_unit
    )
  ),
  col2_year_occ_pair_fe = capture_fit(
    "col2_year_occ_pair_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku |
        year + origin_occupation^destination_occupation,
      data = panel,
      cluster = ~ origin_unit + destination_unit
    )
  ),
  col3_year_origin_kyoku_fe = capture_fit(
    "col3_year_origin_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku |
        year + origin_kyoku,
      data = panel,
      cluster = ~ origin_unit + destination_unit
    )
  ),
  col4_year_destination_kyoku_fe = capture_fit(
    "col4_year_destination_kyoku_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku |
        year + destination_kyoku,
      data = panel,
      cluster = ~ origin_unit + destination_unit
    )
  ),
  col5_year_origin_ka_fe = capture_fit(
    "col5_year_origin_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku |
        year + origin_ka_fe,
      data = panel,
      cluster = ~ origin_unit + destination_unit
    )
  ),
  col6_year_destination_ka_fe = capture_fit(
    "col6_year_destination_ka_fe",
    fepois(
      flow_count ~ log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
        occ_similarity + same_ka_diff_kakari + same_kyoku_diff_ka + diff_kyoku |
        year + destination_ka_fe,
      data = panel,
      cluster = ~ origin_unit + destination_unit
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

write_csv(results, file.path(out_dir, "distance_full_panel_kakari_baseline_results.csv"))
write_csv(model_diagnostics, file.path(out_dir, "distance_full_panel_kakari_baseline_model_diagnostics.csv"))

notes <- c(
  "Kakari-baseline occupation-level PPML",
  "",
  "Unit: origin kakari x origin occupation x destination kakari x destination occupation x year.",
  "Retention is same kakari and same occupation; excluded.",
  "Omitted baseline category is same kakari, different occupation.",
  "Distance coefficients compare farther moves to this within-kakari occupation-change baseline.",
  "Only rows with nonmissing kakari can enter; this is a smaller kakari-covered diagnostic panel.",
  "Size variables are log(1 + non-drafted lagged kakari x occupation counts).",
  "The +1 is required because the omitted same-kakari/different-occupation baseline includes destination occupations with zero lagged employment.",
  "",
  paste("Cells:", sample_summary$n_cells[[1L]]),
  paste("Transfers:", sample_summary$total_transfers[[1L]]),
  paste("Positive cells:", sample_summary$n_positive_cells[[1L]]),
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)

writeLines(notes, file.path(out_dir, "distance_full_panel_kakari_baseline_notes.txt"))

cat("\nDistance diagnostics:\n")
print(distance_diagnostics)
cat("\nResults:\n")
print(results %>% select(model, term, estimate, std.error, p.value))
cat("\nModel diagnostics:\n")
print(model_diagnostics)
cat("\nFinished. Results written to: ", out_dir, "\n", sep = "")
