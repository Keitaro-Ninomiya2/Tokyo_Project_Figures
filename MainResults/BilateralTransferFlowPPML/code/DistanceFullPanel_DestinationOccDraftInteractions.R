################################################################################
# Full occupation-level PPML with destination office x occupation draft interactions.
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
out_dir <- file.path(distance_dir, "distance_full_panel_dest_occ_draft")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

workers_all <- readRDS(file.path(distance_dir, "cleaned_worker_panel_all.rds"))
workers_names <- readRDS(file.path(distance_dir, "cleaned_worker_panel_names.rds"))
base_panel_path <- file.path(
  distance_dir, "distance_full_panel_within_office",
  "distance_full_panel_with_within_office_panel.rds"
)

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

cat("Constructing destination occupation-draft interaction panel...\n")

# Reuse the already constructed non-draft panel as a skeleton for office/occupation support,
# but expand it with draft-affected destination occupation indicators from the all-worker panel.
# The underlying skeleton has the same distance and size construction as the slide table.
panel <- readRDS(base_panel_path)

dest_occ_draft <- workers_all %>%
  filter(year_num %in% 1938:1945, drafted == TRUE, !is_female,
         !is.na(office_clean), !is.na(pos_norm)) %>%
  count(
    year = year_num,
    destination_office = office_clean,
    destination_occupation = pos_norm,
    name = "D_dest_occ"
  ) %>%
  mutate(D_dest_occ_any = as.integer(D_dest_occ > 0L))

panel <- panel %>%
  left_join(dest_occ_draft,
            by = c("year", "destination_office", "destination_occupation")) %>%
  mutate(
    D_dest_occ = replace_na(D_dest_occ, 0L),
    D_dest_occ_any = replace_na(D_dest_occ_any, 0L)
  )

# The saved panel is non-draft at the office level. Rebuild a second, draft-inclusive
# support panel only for destination occupation draft cells that were excluded by the
# earlier office-level non-draft filter.
if (sum(panel$D_dest_occ_any) == 0L) {
  message("Saved panel has no destination occupation draft cells; rebuilding without office-level draft restriction.")

  office_occ <- workers_all %>%
    filter(year_num %in% 1937:1944, !is.na(office_clean), !is.na(pos_norm)) %>%
    group_by(year = year_num + 1L, office = office_clean, occupation = pos_norm) %>%
    summarise(
      N_office_occ = sum(is.na(drafted) | drafted != TRUE),
      .groups = "drop"
    ) %>%
    mutate(log_N_office_occ = log1p(N_office_occ))

  year_occupations <- workers_all %>%
    filter(year_num %in% 1937:1944, !is.na(pos_norm)) %>%
    distinct(year = year_num + 1L, occupation = pos_norm)

  office_year_attr <- workers_all %>%
    filter(year_num %in% 1937:1944, !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
    group_by(year = year_num + 1L, office = office_clean) %>%
    summarise(
      kyoku = first(na.omit(norm_kyoku_filled)),
      ka = first(na.omit(ka)),
      .groups = "drop"
    )

  office_year_attr_current <- workers_all %>%
    filter(year_num %in% 1938:1945, !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
    group_by(year = year_num, office = office_clean) %>%
    summarise(
      kyoku = first(na.omit(norm_kyoku_filled)),
      ka = first(na.omit(ka)),
      .groups = "drop"
    )

  origin_occ <- office_occ %>%
    rename(
      origin_office = office,
      origin_occupation = occupation,
      N_o_occ = N_office_occ,
      log_N_o_occ = log_N_office_occ
    ) %>%
    left_join(
      office_year_attr %>% rename(origin_office = office, origin_kyoku = kyoku, origin_ka = ka),
      by = c("year", "origin_office")
    )

  destination_occ <- office_year_attr_current %>%
    inner_join(year_occupations, by = "year", relationship = "many-to-many") %>%
    left_join(
      office_occ %>% select(year, office, occupation, N_office_occ, log_N_office_occ),
      by = c("year", "office", "occupation")
    ) %>%
    mutate(
      N_office_occ = replace_na(N_office_occ, 0L),
      log_N_office_occ = log1p(N_office_occ)
    ) %>%
    rename(
      destination_office = office,
      destination_kyoku = kyoku,
      destination_ka = ka,
      destination_occupation = occupation,
      N_d_occ = N_office_occ,
      log_N_d_occ = log_N_office_occ
    )

  panel <- origin_occ %>%
    inner_join(destination_occ, by = "year", relationship = "many-to-many") %>%
    filter(
      !(origin_office == destination_office & origin_occupation == destination_occupation),
      !is.na(origin_kyoku), !is.na(destination_kyoku)
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
      destination_ka_fe = destination_office
    ) %>%
    filter(!is.na(distance_category))

  lag_workers <- workers_names %>%
    transmute(
      staff_id,
      year = year_num + 1L,
      origin_office = office_clean,
      origin_occupation = pos_norm
    )

  current_workers <- workers_names %>%
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

  unit_shares <- office_occ %>%
    group_by(year, office) %>%
    mutate(share = N_office_occ / sum(N_office_occ), norm = sqrt(sum(share^2))) %>%
    ungroup() %>%
    select(year, office, occupation, share, norm)

  office_cosine <- unit_shares %>%
    select(year, occupation, origin_office = office, origin_share = share, origin_norm = norm) %>%
    inner_join(
      unit_shares %>%
        select(year, occupation, destination_office = office, destination_share = share,
               destination_norm = norm),
      by = c("year", "occupation"),
      relationship = "many-to-many"
    ) %>%
    group_by(year, origin_office, destination_office) %>%
    summarise(
      dot = sum(origin_share * destination_share),
      origin_norm = first(origin_norm),
      destination_norm = first(destination_norm),
      occ_similarity = dot / (origin_norm * destination_norm),
      .groups = "drop"
    )

  panel <- panel %>%
    left_join(
      flows,
      by = c("year", "origin_office", "origin_occupation", "destination_office",
             "destination_occupation")
    ) %>%
    left_join(office_cosine, by = c("year", "origin_office", "destination_office")) %>%
    left_join(dest_occ_draft,
              by = c("year", "destination_office", "destination_occupation")) %>%
    mutate(
      flow_count = replace_na(flow_count, 0L),
      occ_similarity = if_else(origin_office == destination_office, 1, replace_na(occ_similarity, 0)),
      D_dest_occ = replace_na(D_dest_occ, 0L),
      D_dest_occ_any = replace_na(D_dest_occ_any, 0L)
    )
}

write_rds(panel, file.path(out_dir, "distance_full_panel_dest_occ_draft_panel.rds"))

sample_summary <- panel %>%
  summarise(
    n_cells = n(),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    share_dest_occ_drafted = mean(D_dest_occ_any == 1L),
    n_dest_occ_drafted_cells = sum(D_dest_occ_any == 1L),
    transfers_to_dest_occ_drafted = sum(flow_count[D_dest_occ_any == 1L]),
    transfers_to_dest_occ_nondrafted = sum(flow_count[D_dest_occ_any == 0L])
  )

distance_diagnostics <- panel %>%
  group_by(distance_category, D_dest_occ_any) %>%
  summarise(
    n_cells = n(),
    n_positive_cells = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    .groups = "drop"
  )

write_csv(sample_summary, file.path(out_dir, "dest_occ_draft_interaction_sample_summary.csv"))
write_csv(distance_diagnostics, file.path(out_dir, "dest_occ_draft_interaction_distance_diagnostics.csv"))

rhs <- flow_count ~
  (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
     occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any

cat("Estimating destination occupation-draft interaction models...\n")

models <- list(
  col1_year_fe = capture_fit(
    "col1_year_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any | year,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col2_year_occ_pair_fe = capture_fit(
    "col2_year_occ_pair_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any |
        year + origin_occupation^destination_occupation,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col3_year_origin_kyoku_fe = capture_fit(
    "col3_year_origin_kyoku_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any |
        year + origin_kyoku,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col4_year_destination_kyoku_fe = capture_fit(
    "col4_year_destination_kyoku_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any |
        year + destination_kyoku,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col5_year_origin_ka_fe = capture_fit(
    "col5_year_origin_ka_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any |
        year + origin_ka_fe,
      data = panel,
      cluster = ~ origin_office + destination_office
    )
  ),
  col6_year_destination_ka_fe = capture_fit(
    "col6_year_destination_ka_fe",
    fepois(
      flow_count ~
        (log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ +
           occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any |
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

write_csv(results, file.path(out_dir, "dest_occ_draft_interaction_results.csv"))
write_csv(diagnostics, file.path(out_dir, "dest_occ_draft_interaction_model_diagnostics.csv"))

notes <- c(
  "Destination office x occupation draft interaction PPML",
  "",
  "D_dest_occ_any equals 1 if at least one male worker in the destination office x occupation cell is drafted in year t.",
  "All reported regressors are interacted with D_dest_occ_any.",
  "Omitted distance category is same office, different occupation.",
  "This model does not impose the previous non-draft office-year restriction because that would make D_dest_occ_any identically zero.",
  "",
  paste("Cells:", sample_summary$n_cells[[1L]]),
  paste("Destination occupation drafted cells:", sample_summary$n_dest_occ_drafted_cells[[1L]]),
  paste("Transfers to drafted destination occupation cells:", sample_summary$transfers_to_dest_occ_drafted[[1L]]),
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)
writeLines(notes, file.path(out_dir, "dest_occ_draft_interaction_notes.txt"))

cat("\nSample summary:\n")
print(sample_summary)
cat("\nInteraction results:\n")
print(results %>% select(model, term, estimate, std.error, p.value))
cat("\nDiagnostics:\n")
print(diagnostics)
