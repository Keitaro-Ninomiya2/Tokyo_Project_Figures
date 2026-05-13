################################################################################
# Similarity-measure robustness for within-department flow PPML.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
distance_dir <- file.path(result_dir, "distance_decomposition")

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

model_table <- function(model, model_name, sim_measure) {
  if (is.null(model)) {
    return(tibble(
      model = model_name,
      sim_measure = sim_measure,
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
      sim_measure = sim_measure,
      nobs = model_nobs,
      pseudo_r2 = pseudo_r2,
      dispersion = dispersion,
      status = "estimated",
      .before = 1
    )
}

cat("Loading same-department non-draft pair-years...\n")

within_dept_nondraft <- pair_panel %>%
  filter(same_department == 1L, D_origin == 0L, D_destination == 0L) %>%
  transmute(
    year,
    lag_year = year - 1L,
    pair_id,
    origin_unit = origin_office,
    destination_unit = destination_office,
    kyoku = origin_kyoku,
    flow_count,
    log_N_o,
    log_N_d,
    occ_similarity
  )

cat("Building alternative similarity measures from saved pair panel covariates...\n")

# The existing pair panel does not retain position-set vectors. Reconstructing
# those vectors exactly would require re-running the master-panel builder.
# However, the pair panel already contains N_o/N_d and cosine similarity by pair.
# For the alternative measures requested here, use the saved full panel if the
# richer feature panel has already been produced; otherwise create it from the
# same source by calling the existing RDS fields is impossible. Stop loudly rather
# than fabricating overlap measures.
rich_path <- file.path(distance_dir, "distance_decomposition_transfer_panel.rds")
if (!file.exists(rich_path)) {
  stop("distance_decomposition_transfer_panel.rds not found. Run DistanceDecomposition_FlowConcentration.R first.")
}

# The distance panel also does not include position-set overlap measures; compute
# approximate binary/overlap measures from already-available cosine only would be
# invalid. Instead, source the original PPML builder's saved pair panel is
# insufficient. The code below rebuilds lagged position compositions using the
# same master-panel cleanup helpers by sourcing the distance decomposition script
# is avoided to prevent rerunning models. Therefore this script requires the
# enriched composition file if created by a future run.
composition_path <- file.path(distance_dir, "office_position_composition_lagged.csv")

if (!file.exists(composition_path)) {
  stop(paste(
    "Missing office_position_composition_lagged.csv.",
    "Need to add/export lagged office position compositions from the bilateral builder before similarity robustness can be estimated."
  ))
}

office_pos <- read_csv(composition_path, show_col_types = FALSE)

top_positions <- office_pos %>%
  group_by(lag_year, office_unit) %>%
  arrange(desc(n_position), pos_norm, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  summarise(
    top1 = first(pos_norm),
    top3_list = list(pos_norm[rank <= 3L]),
    pos_list = list(pos_norm),
    .groups = "drop"
  )

pair_sets <- within_dept_nondraft %>%
  left_join(
    top_positions %>%
      rename(origin_unit = office_unit, origin_top1 = top1,
             origin_top3 = top3_list, origin_positions = pos_list),
    by = c("lag_year", "origin_unit")
  ) %>%
  left_join(
    top_positions %>%
      rename(destination_unit = office_unit, destination_top1 = top1,
             destination_top3 = top3_list, destination_positions = pos_list),
    by = c("lag_year", "destination_unit")
  )

set_overlap <- function(a, b) {
  if (length(a) == 0L || length(b) == 0L || all(is.na(a)) || all(is.na(b))) {
    return(NA_real_)
  }
  length(intersect(a, b))
}

set_union_n <- function(a, b) {
  if (length(a) == 0L || length(b) == 0L || all(is.na(a)) || all(is.na(b))) {
    return(NA_real_)
  }
  length(union(a, b))
}

pair_measures <- pair_sets %>%
  rowwise() %>%
  mutate(
    overlap_n = set_overlap(origin_positions, destination_positions),
    union_n = set_union_n(origin_positions, destination_positions),
    occ_overlap_any = as.integer(overlap_n > 0),
    occ_overlap_top1 = as.integer(!is.na(origin_top1) && !is.na(destination_top1) &&
                                    origin_top1 == destination_top1),
    occ_overlap_top3 = as.integer(set_overlap(origin_top3, destination_top3) > 0),
    occ_jaccard = if_else(union_n > 0, overlap_n / union_n, NA_real_)
  ) %>%
  ungroup() %>%
  select(year, lag_year, pair_id, origin_unit, destination_unit, kyoku, flow_count,
         log_N_o, log_N_d, occ_similarity, occ_overlap_any, occ_overlap_top1,
         occ_overlap_top3, occ_jaccard)

share_overlap <- office_pos %>%
  select(lag_year, pos_norm, origin_unit = office_unit, origin_share = position_share) %>%
  inner_join(
    office_pos %>%
      select(lag_year, pos_norm, destination_unit = office_unit, destination_share = position_share),
    by = c("lag_year", "pos_norm"),
    relationship = "many-to-many"
  ) %>%
  group_by(lag_year, origin_unit, destination_unit) %>%
  summarise(occ_share_overlap = sum(pmin(origin_share, destination_share)), .groups = "drop")

analysis_panel <- pair_measures %>%
  left_join(share_overlap, by = c("lag_year", "origin_unit", "destination_unit")) %>%
  mutate(occ_share_overlap = replace_na(occ_share_overlap, 0))

sim_measures <- c(
  "occ_similarity",
  "occ_overlap_any",
  "occ_overlap_top1",
  "occ_overlap_top3",
  "occ_jaccard",
  "occ_share_overlap"
)

distribution <- analysis_panel %>%
  summarise(
    across(
      all_of(sim_measures),
      list(
        n_nonmissing = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        p10 = ~ as.numeric(quantile(.x, 0.10, na.rm = TRUE)),
        p90 = ~ as.numeric(quantile(.x, 0.90, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(everything(), names_to = "name", values_to = "value") %>%
  separate_wider_regex(name, patterns = c(measure = ".*", "_", statistic = "[^_]+$"))

within_kyoku_sd <- analysis_panel %>%
  group_by(kyoku) %>%
  summarise(across(all_of(sim_measures), ~ sd(.x, na.rm = TRUE), .names = "{.col}_within_kyoku_sd"),
            .groups = "drop") %>%
  summarise(across(ends_with("_within_kyoku_sd"),
                   list(mean = ~ mean(.x, na.rm = TRUE), median = ~ median(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = "name", values_to = "value")

correlations <- analysis_panel %>%
  select(all_of(sim_measures)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("measure")

models <- map(sim_measures, function(sim) {
  fml <- as.formula(paste0(
    "flow_count ~ log_N_o + log_N_d + log_N_o:log_N_d + ", sim,
    " | year + kyoku"
  ))
  capture_fit(
    paste0("similarity_", sim),
    fepois(fml, data = analysis_panel, cluster = ~ origin_unit + destination_unit)
  )
})
names(models) <- sim_measures

results <- imap_dfr(models, ~ model_table(.x, paste0("within_dept_", .y), .y))

diagnostics <- results %>%
  distinct(model, sim_measure, status, nobs, pseudo_r2, dispersion) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(analysis_panel, file.path(distance_dir, "similarity_robustness_analysis_panel.csv"))
write_csv(results, file.path(distance_dir, "similarity_robustness_results.csv"))
write_csv(distribution, file.path(distance_dir, "similarity_measures_distribution.csv"))
write_csv(within_kyoku_sd, file.path(distance_dir, "similarity_measures_within_kyoku_sd.csv"))
write_csv(correlations, file.path(distance_dir, "similarity_measures_correlations.csv"))
write_csv(diagnostics, file.path(distance_dir, "similarity_robustness_diagnostics.csv"))

cat("\nSimilarity robustness coefficient rows:\n")
print(results %>% filter(term %in% sim_measures) %>% select(sim_measure, estimate, std.error, p.value))
cat("\nFinished. Results written to: ", distance_dir, "\n", sep = "")
