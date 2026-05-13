################################################################################
# Targeted destination occupation-draft interaction PPML.
# Fixes the undercount by adding drafted destination occupation support cells
# and all observed positive transfer cells to the existing full panel.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
distance_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results", "distance_decomposition")
out_dir <- file.path(distance_dir, "distance_full_panel_dest_occ_draft")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

workers_all <- readRDS(file.path(distance_dir, "cleaned_worker_panel_all.rds"))
workers_names <- readRDS(file.path(distance_dir, "cleaned_worker_panel_names.rds"))
base_panel <- readRDS(file.path(distance_dir, "distance_full_panel_within_office",
                                "distance_full_panel_with_within_office_panel.rds"))

fit_warnings <- character()
fit_errors <- character()

capture_fit <- function(label, expr) {
  tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      fit_warnings <<- c(fit_warnings, paste(label, conditionMessage(w), sep = ": "))
      message("WARNING [", label, "]: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      fit_errors <<- c(fit_errors, paste(label, conditionMessage(e), sep = ": "))
      message("ERROR [", label, "]: ", conditionMessage(e))
      NULL
    }
  )
}

model_table <- function(model, model_name) {
  if (is.null(model)) {
    return(tibble(model = model_name, term = NA_character_, estimate = NA_real_,
                  std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
                  conf.low = NA_real_, conf.high = NA_real_, nobs = NA_integer_,
                  pseudo_r2 = NA_real_, dispersion = NA_real_, status = "failed"))
  }
  model_nobs <- as.integer(model$nobs)
  pseudo_r2 <- tryCatch(as.numeric(fitstat(model, "pr2")[[1L]]), error = function(e) NA_real_)
  pearson <- tryCatch(residuals(model, type = "pearson"), error = function(e) rep(NA_real_, model_nobs))
  residual_df <- tryCatch(df.residual(model), error = function(e) model_nobs - length(coef(model)))
  dispersion <- if (all(is.na(pearson)) || is.na(residual_df) || residual_df <= 0) NA_real_ else sum(pearson^2, na.rm = TRUE) / residual_df
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(model = model_name, nobs = model_nobs, pseudo_r2 = pseudo_r2,
           dispersion = dispersion, status = "estimated", .before = 1)
}

cat("Building targeted drafted-destination support...\n")

office_occ <- workers_all %>%
  filter(year_num %in% 1937:1944, !is.na(office_clean), !is.na(pos_norm)) %>%
  group_by(year = year_num + 1L, office = office_clean, occupation = pos_norm) %>%
  summarise(N_office_occ = sum(is.na(drafted) | drafted != TRUE), .groups = "drop") %>%
  mutate(log_N_office_occ = log1p(N_office_occ))

office_year_attr_lag <- workers_all %>%
  filter(year_num %in% 1937:1944, !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  group_by(year = year_num + 1L, office = office_clean) %>%
  summarise(kyoku = first(na.omit(norm_kyoku_filled)), ka = first(na.omit(ka)), .groups = "drop")

office_year_attr_cur <- workers_all %>%
  filter(year_num %in% 1938:1945, !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  group_by(year = year_num, office = office_clean) %>%
  summarise(kyoku = first(na.omit(norm_kyoku_filled)), ka = first(na.omit(ka)), .groups = "drop")

origin_occ <- office_occ %>%
  rename(origin_office = office, origin_occupation = occupation,
         N_o_occ = N_office_occ, log_N_o_occ = log_N_office_occ) %>%
  left_join(office_year_attr_lag %>% rename(origin_office = office, origin_kyoku = kyoku, origin_ka = ka),
            by = c("year", "origin_office"))

dest_occ_draft <- workers_all %>%
  filter(year_num %in% 1938:1945, drafted == TRUE, !is_female,
         !is.na(office_clean), !is.na(pos_norm)) %>%
  count(year = year_num, destination_office = office_clean,
        destination_occupation = pos_norm, name = "D_dest_occ") %>%
  mutate(D_dest_occ_any = 1L) %>%
  left_join(office_year_attr_cur %>% rename(destination_office = office, destination_kyoku = kyoku, destination_ka = ka),
            by = c("year", "destination_office")) %>%
  left_join(
    office_occ %>%
      rename(destination_office = office, destination_occupation = occupation,
             N_d_occ = N_office_occ, log_N_d_occ = log_N_office_occ),
    by = c("year", "destination_office", "destination_occupation")
  ) %>%
  mutate(N_d_occ = replace_na(N_d_occ, 0L), log_N_d_occ = log1p(N_d_occ))

lag_workers <- workers_names %>%
  transmute(staff_id, year = year_num + 1L,
            origin_office = office_clean, origin_occupation = pos_norm,
            origin_kyoku = norm_kyoku_filled)
current_workers <- workers_names %>%
  transmute(staff_id, year = year_num,
            destination_office = office_clean, destination_occupation = pos_norm,
            destination_kyoku = norm_kyoku_filled)

flows <- current_workers %>%
  filter(year %in% 1938:1945) %>%
  inner_join(lag_workers, by = c("staff_id", "year")) %>%
  filter(!is.na(origin_office), !is.na(destination_office),
         !is.na(origin_occupation), !is.na(destination_occupation),
         !(origin_office == destination_office & origin_occupation == destination_occupation)) %>%
  count(year, origin_office, origin_occupation, destination_office, destination_occupation,
        name = "flow_count")

draft_support <- origin_occ %>%
  inner_join(dest_occ_draft, by = "year", relationship = "many-to-many") %>%
  filter(!(origin_office == destination_office & origin_occupation == destination_occupation),
         !is.na(origin_kyoku), !is.na(destination_kyoku)) %>%
  mutate(source_added = "draft_destination_support")

positive_support <- flows %>%
  left_join(origin_occ, by = c("year", "origin_office", "origin_occupation")) %>%
  left_join(
    office_year_attr_cur %>% rename(destination_office = office, destination_kyoku = kyoku, destination_ka = ka),
    by = c("year", "destination_office")
  ) %>%
  left_join(
    office_occ %>% rename(destination_office = office, destination_occupation = occupation,
                          N_d_occ = N_office_occ, log_N_d_occ = log_N_office_occ),
    by = c("year", "destination_office", "destination_occupation")
  ) %>%
  mutate(N_d_occ = replace_na(N_d_occ, 0L), log_N_d_occ = log1p(N_d_occ),
         D_dest_occ = 0L, D_dest_occ_any = 0L, source_added = "positive_flow_support") %>%
  select(-flow_count)

unit_shares <- office_occ %>%
  group_by(year, office) %>%
  mutate(share = N_office_occ / sum(N_office_occ), norm = sqrt(sum(share^2))) %>%
  ungroup() %>%
  select(year, office, occupation, share, norm)

office_cosine <- unit_shares %>%
  select(year, occupation, origin_office = office, origin_share = share, origin_norm = norm) %>%
  inner_join(unit_shares %>% select(year, occupation, destination_office = office,
                                    destination_share = share, destination_norm = norm),
             by = c("year", "occupation"), relationship = "many-to-many") %>%
  group_by(year, origin_office, destination_office) %>%
  summarise(dot = sum(origin_share * destination_share), origin_norm = first(origin_norm),
            destination_norm = first(destination_norm),
            occ_similarity = dot / (origin_norm * destination_norm), .groups = "drop")

base_keep <- base_panel %>%
  mutate(D_dest_occ = 0L, D_dest_occ_any = 0L, source_added = "base_panel") %>%
  select(year, origin_office, origin_occupation, destination_office, destination_occupation,
         N_o_occ, log_N_o_occ, N_d_occ, log_N_d_occ, origin_kyoku, destination_kyoku,
         origin_ka, destination_ka, origin_ka_fe, destination_ka_fe, occ_similarity,
         same_office, same_kyoku_diff_ka, diff_kyoku, flow_count, D_dest_occ,
         D_dest_occ_any, source_added)

added_keep <- bind_rows(draft_support, positive_support) %>%
  mutate(
    same_office = as.integer(origin_office == destination_office),
    same_kyoku_diff_ka = as.integer(origin_office != destination_office & origin_kyoku == destination_kyoku),
    diff_kyoku = as.integer(origin_kyoku != destination_kyoku),
    origin_ka_fe = origin_office,
    destination_ka_fe = destination_office
  ) %>%
  filter(!is.na(origin_kyoku), !is.na(destination_kyoku)) %>%
  left_join(office_cosine, by = c("year", "origin_office", "destination_office")) %>%
  mutate(occ_similarity = if_else(origin_office == destination_office, 1, replace_na(occ_similarity, 0))) %>%
  select(year, origin_office, origin_occupation, destination_office, destination_occupation,
         N_o_occ, log_N_o_occ, N_d_occ, log_N_d_occ, origin_kyoku, destination_kyoku,
         origin_ka, destination_ka, origin_ka_fe, destination_ka_fe, occ_similarity,
         same_office, same_kyoku_diff_ka, diff_kyoku, D_dest_occ, D_dest_occ_any, source_added)

panel <- bind_rows(base_keep, added_keep %>% mutate(flow_count = NA_integer_)) %>%
  distinct(year, origin_office, origin_occupation, destination_office, destination_occupation, .keep_all = TRUE) %>%
  select(-flow_count) %>%
  left_join(flows, by = c("year", "origin_office", "origin_occupation",
                          "destination_office", "destination_occupation")) %>%
  left_join(dest_occ_draft %>% select(year, destination_office, destination_occupation, D_dest_occ, D_dest_occ_any),
            by = c("year", "destination_office", "destination_occupation"), suffix = c("", "_draft")) %>%
  mutate(
    flow_count = replace_na(flow_count, 0L),
    D_dest_occ = coalesce(D_dest_occ_draft, D_dest_occ, 0L),
    D_dest_occ_any = coalesce(D_dest_occ_any_draft, D_dest_occ_any, 0L)
  ) %>%
  select(-D_dest_occ_draft, -D_dest_occ_any_draft)

write_rds(panel, file.path(out_dir, "dest_occ_draft_interaction_targeted_panel.rds"))

sample_summary <- panel %>%
  summarise(n_cells = n(), n_positive_cells = sum(flow_count > 0),
            total_transfers = sum(flow_count),
            n_dest_occ_drafted_cells = sum(D_dest_occ_any == 1L),
            transfers_to_dest_occ_drafted = sum(flow_count[D_dest_occ_any == 1L]),
            transfers_to_dest_occ_nondrafted = sum(flow_count[D_dest_occ_any == 0L]))
write_csv(sample_summary, file.path(out_dir, "dest_occ_draft_interaction_targeted_sample_summary.csv"))

cat("Estimating targeted interaction models...\n")

fml_rhs <- "(log_N_o_occ + log_N_d_occ + log_N_o_occ:log_N_d_occ + occ_similarity + same_kyoku_diff_ka + diff_kyoku) * D_dest_occ_any"

run_model <- function(label, fe) {
  capture_fit(label, fepois(as.formula(paste0("flow_count ~ ", fml_rhs, " | ", fe)),
                            data = panel, cluster = ~ origin_office + destination_office))
}

models <- list(
  col1_year_fe = run_model("col1_year_fe", "year"),
  col2_year_occ_pair_fe = run_model("col2_year_occ_pair_fe", "year + origin_occupation^destination_occupation"),
  col3_year_origin_kyoku_fe = run_model("col3_year_origin_kyoku_fe", "year + origin_kyoku"),
  col4_year_destination_kyoku_fe = run_model("col4_year_destination_kyoku_fe", "year + destination_kyoku"),
  col5_year_origin_ka_fe = run_model("col5_year_origin_ka_fe", "year + origin_ka_fe"),
  col6_year_destination_ka_fe = run_model("col6_year_destination_ka_fe", "year + destination_ka_fe")
)

results <- imap_dfr(models, ~ model_table(.x, .y))
diagnostics <- results %>%
  distinct(model, status, nobs, pseudo_r2, dispersion) %>%
  mutate(warnings = paste(fit_warnings, collapse = " | "),
         errors = paste(fit_errors, collapse = " | "))

write_csv(results, file.path(out_dir, "dest_occ_draft_interaction_targeted_results.csv"))
write_csv(diagnostics, file.path(out_dir, "dest_occ_draft_interaction_targeted_model_diagnostics.csv"))

cat("\nSample summary:\n")
print(sample_summary)
cat("\nSelected interaction rows:\n")
print(results %>% filter(str_detect(term, "D_dest_occ_any")) %>% select(model, term, estimate, std.error, p.value))
