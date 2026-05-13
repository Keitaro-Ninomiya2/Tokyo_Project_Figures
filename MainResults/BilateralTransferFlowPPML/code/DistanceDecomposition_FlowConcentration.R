################################################################################
# Flow concentration by organizational distance, 1938-1945.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BilateralTransferFlowPPML", "results")
out_dir <- file.path(result_dir, "distance_decomposition")
reference_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "reference_tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- Sys.getenv("TOKYO_PERSONNEL_MASTER", unset = file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
))
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")
BASE_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_ka_to_kyoku.csv")
FACILITY_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_facility_to_kyoku.csv")
KAKARI_REF_PATH <- file.path(reference_dir, "tokyoto_1943_1944_kakari_to_kyoku.csv")
PAIR_PANEL_PATH <- file.path(result_dir, "bilateral_flow_pair_panel_full.rds")

years_of_interest <- 1938:1945
analysis_year <- 1944L

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

clean_label <- function(x) {
  x <- str_squish(replace_na(as.character(x), ""))
  na_if(x, "")
}

office_key <- function(kyoku_clean, ka_clean) {
  if_else(!is.na(kyoku_clean) & !is.na(ka_clean),
          paste(kyoku_clean, ka_clean, sep = " :: "),
          NA_character_)
}

safe_first <- function(x) {
  x <- as.character(x[!is.na(x)])
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

section_key <- function(office_id, ka_group, ka_name) {
  if_else(
    !is.na(ka_group),
    paste0(office_id, "_grp_", ka_group),
    paste0(office_id, "_name_", replace_na(ka_name, "missing"))
  )
}

classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "æŠ€") ~ "engineer",
    str_detect(pos, "é›‡|å‚­|å›‘è¨—") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^ä¸»äº‹$|^æŠ€å¸«$") ~ 3L,
    yr < 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "ä¿‚é•·") ~ 3L,
    yr >= 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$") ~ 1L,
    yr >= 1948 ~ 2L
  )
}

apply_1944_tokyoto_backfill <- function(df, ka_group_raw) {
  base_ref_long <- read_csv(BASE_REF_PATH, show_col_types = FALSE) %>%
    filter(as.Date(effective_start) <= as.Date("1944-12-31"),
           as.Date(effective_end) >= as.Date("1944-01-01"))

  base_ref <- base_ref_long %>%
    count(ka, name = "base_candidate_count") %>%
    left_join(
      base_ref_long %>%
        group_by(ka) %>%
        filter(n() == 1L) %>%
        ungroup() %>%
        transmute(ka, base_ref_kyoku = kyoku),
      by = "ka"
    )

  facility_ref <- read_csv(FACILITY_REF_PATH, show_col_types = FALSE) %>%
    transmute(ka = label, facility_ref_kyoku = kyoku)

  kakari_ref <- read_csv(KAKARI_REF_PATH, show_col_types = FALSE) %>%
    transmute(kakari = label, kakari_ref_kyoku = kyoku)

  adjacent_staff_kyoku <- df %>%
    filter(year_num %in% c(analysis_year - 1L, analysis_year + 1L),
           !is.na(norm_kyoku)) %>%
    group_by(staff_id) %>%
    summarise(
      adjacent_known_kyoku_n = n_distinct(norm_kyoku),
      adjacent_known_kyoku = if_else(n_distinct(norm_kyoku) == 1L, first(norm_kyoku), NA_character_),
      .groups = "drop"
    )

  ambiguous_rows <- df %>%
    left_join(base_ref, by = "ka") %>%
    left_join(adjacent_staff_kyoku, by = "staff_id") %>%
    filter(year_num == analysis_year,
           is.na(clean_label(kyoku)),
           !is.na(ka),
           replace_na(base_candidate_count, 0L) > 1L,
           !is.na(adjacent_known_kyoku)) %>%
    select(staff_id, ka, adjacent_known_kyoku) %>%
    distinct() %>%
    inner_join(base_ref_long %>% select(ka, candidate_kyoku = kyoku),
               by = "ka", relationship = "many-to-many") %>%
    filter(adjacent_known_kyoku == candidate_kyoku) %>%
    group_by(staff_id, ka) %>%
    summarise(
      history_match_n = n(),
      history_match_kyoku = if_else(n() == 1L, first(candidate_kyoku), NA_character_),
      .groups = "drop"
    )

  df %>%
    left_join(base_ref, by = "ka") %>%
    left_join(facility_ref, by = "ka") %>%
    left_join(kakari_ref, by = "kakari") %>%
    left_join(ambiguous_rows, by = c("staff_id", "ka")) %>%
    mutate(
      kyoku_backfill = case_when(
        year_num != analysis_year ~ kyoku,
        !is.na(clean_label(kyoku)) ~ kyoku,
        !is.na(base_ref_kyoku) ~ base_ref_kyoku,
        !is.na(facility_ref_kyoku) ~ facility_ref_kyoku,
        !is.na(history_match_kyoku) ~ history_match_kyoku,
        !is.na(kakari_ref_kyoku) ~ kakari_ref_kyoku,
        TRUE ~ NA_character_
      ),
      norm_kyoku_filled = normalize_kyoku(kyoku_backfill),
      kyoku_group_filled = assign_kyoku_group(norm_kyoku_filled),
      kyoku_clean_filled = replace_na(norm_kyoku_filled, ""),
      ka_clean_filled = replace_na(clean_label(ka), "")
    ) %>%
    left_join(
      ka_group_raw %>%
        rename(ka_group_lookup = ka_group, kyoku_clean_filled = kyoku,
               ka_clean_filled = ka, year_num = year),
      by = c("year_num", "kyoku_clean_filled", "ka_clean_filled")
    ) %>%
    mutate(
      ka_group_filled = if_else(year_num == analysis_year, ka_group_lookup, ka_group),
      section_id_filled = section_key(office_id, ka_group_filled, ka),
      office_clean = office_key(norm_kyoku_filled, clean_label(ka))
    ) %>%
    select(-ka_group_lookup)
}

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

cat("Loading existing cross-ka pair panel...\n")
cross_ka_panel <- readRDS(PAIR_PANEL_PATH) %>%
  mutate(
    distance_category = case_when(
      same_department == 1L ~ "same_kyoku_diff_ka",
      different_department == 1L ~ "diff_kyoku",
      TRUE ~ NA_character_
    ),
    origin_unit = origin_office,
    destination_unit = destination_office,
    kyoku = if_else(distance_category == "same_kyoku_diff_ka", origin_kyoku, NA_character_),
    kyoku_pair_id = paste(pmin(origin_kyoku, destination_kyoku),
                          pmax(origin_kyoku, destination_kyoku), sep = " <=> "),
    same_ka = 0L,
    same_kyoku_diff_ka = as.integer(distance_category == "same_kyoku_diff_ka"),
    diff_kyoku = as.integer(distance_category == "diff_kyoku")
  ) %>%
  select(year, distance_category, origin_unit, destination_unit, pair_id,
         kyoku, kyoku_pair_id, flow_count, log_N_o, log_N_d, occ_similarity,
         same_ka, same_kyoku_diff_ka, diff_kyoku)

cat("Constructing same-ka movement panel from master personnel panel...\n")

raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.integer(as.numeric(year)),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(clean_label(ka), ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku),
    occupation = classify_occ(pos_norm),
    pos_rank = assign_rank(pos_norm, year_num),
    occ_rank = paste(occupation, pos_rank, sep = "_")
  )

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""), ka = replace_na(clean_label(ka), ""))

raw <- raw %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  ) %>%
  mutate(section_id = section_key(office_id, ka_group, ka))

df_names <- raw %>%
  filter(is_name == TRUE) %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  apply_1944_tokyoto_backfill(ka_group_raw)

df_all <- raw %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  apply_1944_tokyoto_backfill(ka_group_raw)

cleaned_worker_panel <- df_names %>%
  select(
    staff_id, year_num, office_clean, norm_kyoku_filled, ka,
    kakari, pos_norm, drafted, is_female
  )

write_rds(cleaned_worker_panel, file.path(out_dir, "cleaned_worker_panel_names.rds"))

cleaned_worker_panel_all <- df_all %>%
  select(
    staff_id, year_num, office_clean, norm_kyoku_filled, ka,
    kakari, pos_norm, drafted, is_female
  )

write_rds(cleaned_worker_panel_all, file.path(out_dir, "cleaned_worker_panel_all.rds"))

office_position_composition <- df_all %>%
  filter(
    year_num %in% (years_of_interest - 1L),
    !is.na(office_clean),
    !is.na(pos_norm),
    is.na(drafted) | drafted != TRUE
  ) %>%
  count(lag_year = year_num, office_unit = office_clean, pos_norm, name = "n_position") %>%
  group_by(lag_year, office_unit) %>%
  mutate(
    office_total = sum(n_position),
    position_share = n_position / office_total
  ) %>%
  ungroup()

write_csv(office_position_composition, file.path(out_dir, "office_position_composition_lagged.csv"))

lag_units <- df_names %>%
  filter(year_num %in% (years_of_interest - 1L)) %>%
  transmute(
    staff_id,
    year = year_num + 1L,
    lag_office_clean = office_clean,
    lag_kyoku = norm_kyoku_filled,
    lag_ka = clean_label(ka),
    lag_kakari = clean_label(kakari)
  )

same_ka_events <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
  transmute(
    staff_id,
    year = year_num,
    office_clean,
    kyoku = norm_kyoku_filled,
    ka = clean_label(ka),
    kakari = clean_label(kakari)
  ) %>%
  inner_join(lag_units, by = c("staff_id", "year")) %>%
  filter(
    !is.na(office_clean),
    office_clean == lag_office_clean,
    !is.na(kakari),
    !is.na(lag_kakari),
    kakari != lag_kakari
  )

same_ka_flows <- same_ka_events %>%
  count(year, origin_unit = office_clean, destination_unit = office_clean,
        kyoku, name = "flow_count")

same_ka_universe <- df_names %>%
  filter(year_num %in% (years_of_interest - 1L), !is.na(office_clean), !is.na(norm_kyoku_filled)) %>%
  distinct(year = year_num + 1L, origin_unit = office_clean,
           destination_unit = office_clean, kyoku = norm_kyoku_filled)

same_ka_sizes <- df_all %>%
  filter(year_num %in% (years_of_interest - 1L),
         !is.na(office_clean), is.na(drafted) | drafted != TRUE) %>%
  count(year = year_num + 1L, origin_unit = office_clean, name = "N_unit") %>%
  mutate(log_N = log(N_unit))

same_ka_panel <- same_ka_universe %>%
  left_join(same_ka_flows,
            by = c("year", "origin_unit", "destination_unit", "kyoku")) %>%
  left_join(same_ka_sizes, by = c("year", "origin_unit")) %>%
  mutate(
    flow_count = replace_na(flow_count, 0L),
    distance_category = "same_ka",
    pair_id = paste(origin_unit, destination_unit, sep = " => "),
    kyoku_pair_id = paste(kyoku, kyoku, sep = " <=> "),
    log_N_o = log_N,
    log_N_d = log_N,
    occ_similarity = 1,
    same_ka = 1L,
    same_kyoku_diff_ka = 0L,
    diff_kyoku = 0L
  ) %>%
  filter(is.finite(log_N_o), is.finite(log_N_d)) %>%
  select(year, distance_category, origin_unit, destination_unit, pair_id,
         kyoku, kyoku_pair_id, flow_count, log_N_o, log_N_d, occ_similarity,
         same_ka, same_kyoku_diff_ka, diff_kyoku)

transfer_panel <- bind_rows(cross_ka_panel, same_ka_panel) %>%
  mutate(distance_category = factor(distance_category,
                                    levels = c("diff_kyoku", "same_kyoku_diff_ka", "same_ka")))

distance_year <- transfer_panel %>%
  group_by(distance_category, year) %>%
  summarise(
    n_pairs = n_distinct(pair_id),
    n_pair_years = n(),
    n_positive_pair_years = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    mean_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                    mean(flow_count[flow_count > 0]), NA_real_),
    median_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                      as.numeric(median(flow_count[flow_count > 0])), NA_real_),
    max_flow = max(flow_count),
    .groups = "drop"
  )

distance_overall <- transfer_panel %>%
  group_by(distance_category) %>%
  summarise(
    n_pairs = n_distinct(pair_id),
    n_pair_years = n(),
    n_positive_pair_years = sum(flow_count > 0),
    total_transfers = sum(flow_count),
    mean_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                    mean(flow_count[flow_count > 0]), NA_real_),
    median_flow_if_positive = if_else(sum(flow_count > 0) > 0,
                                      as.numeric(median(flow_count[flow_count > 0])), NA_real_),
    max_flow = max(flow_count),
    .groups = "drop"
  ) %>%
  mutate(transfer_share = total_transfers / sum(total_transfers))

within_kyoku_concentration <- transfer_panel %>%
  filter(distance_category == "same_kyoku_diff_ka") %>%
  group_by(kyoku, year) %>%
  summarise(
    n_pairs_in_kyoku = n_distinct(pair_id),
    n_positive_pairs = sum(flow_count > 0),
    total_flows = sum(flow_count),
    top_pair_share = if_else(total_flows > 0, max(flow_count) / total_flows, NA_real_),
    top2_pair_share = if_else(total_flows > 0,
                              sum(head(sort(flow_count, decreasing = TRUE), 2L)) / total_flows,
                              NA_real_),
    top3_pair_share = if_else(total_flows > 0,
                              sum(head(sort(flow_count, decreasing = TRUE), 3L)) / total_flows,
                              NA_real_),
    herfindahl = if_else(total_flows > 0, sum((flow_count / total_flows)^2), NA_real_),
    .groups = "drop"
  )

write_csv(distance_overall, file.path(out_dir, "flow_distribution_by_distance.csv"))
write_csv(distance_year, file.path(out_dir, "flow_distribution_by_distance_year.csv"))
write_csv(within_kyoku_concentration, file.path(out_dir, "within_kyoku_concentration.csv"))
write_rds(transfer_panel, file.path(out_dir, "distance_decomposition_transfer_panel.rds"))

cat("Estimating distance-decomposition PPML models...\n")

distance_ppml <- capture_fit(
  "distance_ppml",
  fepois(
    flow_count ~ same_ka + same_kyoku_diff_ka + log_N_o + log_N_d + occ_similarity | year,
    data = transfer_panel,
    cluster = ~ origin_unit + destination_unit
  )
)

distance_ppml_kyokuFE <- capture_fit(
  "distance_ppml_kyokuFE",
  fepois(
    flow_count ~ same_kyoku_diff_ka + log_N_o + log_N_d + occ_similarity | year + kyoku_pair_id,
    data = transfer_panel %>% filter(distance_category != "same_ka"),
    cluster = ~ origin_unit + destination_unit
  )
)

within_dept_features <- capture_fit(
  "within_dept_features",
  fepois(
    flow_count ~ log_N_o + log_N_d + occ_similarity + log_N_o:log_N_d | year + kyoku,
    data = transfer_panel %>% filter(distance_category == "same_kyoku_diff_ka"),
    cluster = ~ origin_unit + destination_unit
  )
)

distance_results <- model_table(distance_ppml, "distance_ppml")
distance_kyoku_results <- model_table(distance_ppml_kyokuFE, "distance_ppml_kyokuFE")
within_features_results <- model_table(within_dept_features, "within_dept_features")

write_csv(distance_results, file.path(out_dir, "distance_ppml_results.csv"))
write_csv(distance_kyoku_results, file.path(out_dir, "distance_ppml_kyokuFE_results.csv"))
write_csv(within_features_results, file.path(out_dir, "within_dept_features_results.csv"))

model_diagnostics <- bind_rows(
  distance_results %>% distinct(model, status, nobs, pseudo_r2, dispersion),
  distance_kyoku_results %>% distinct(model, status, nobs, pseudo_r2, dispersion),
  within_features_results %>% distinct(model, status, nobs, pseudo_r2, dispersion)
) %>%
  mutate(
    warnings = paste(fit_warnings, collapse = " | "),
    errors = paste(fit_errors, collapse = " | ")
  )

write_csv(model_diagnostics, file.path(out_dir, "distance_models_diagnostics.csv"))

notes <- c(
  "Distance-decomposition flow concentration notes",
  "",
  paste("Master panel:", DATA_PATH),
  paste("Cross-ka pair panel:", PAIR_PANEL_PATH),
  "",
  "Distance definitions:",
  "- same_ka: same cleaned kyoku x ka in t and t-1 but changed nonmissing kakari. Because the analysis is ka-level and kakari coverage is incomplete, this is a lower-confidence within-ka movement measure and is aggregated at ka-year rather than sub-ka pair level.",
  "- same_kyoku_diff_ka: existing bilateral pair panel, same cleaned kyoku and different ka.",
  "- diff_kyoku: existing bilateral pair panel, different cleaned kyoku.",
  "",
  "Office/unit definition:",
  "- origin_unit and destination_unit are cleaned kyoku x ka labels.",
  "- Existing 1944 TokyoTo cleanup/backfill logic is reused for same_ka construction.",
  "- No additional pre/post-1943 merger linkage is imposed for kyoku_pair_id stability.",
  "",
  "Draft restriction:",
  "- The main decomposition includes all pair-years in the existing panel and same_ka universe; it is not restricted to non-draft years.",
  "",
  "Regression notes:",
  "- same_ka has no true bilateral sub-ka pair structure, so it enters only the pooled distance-gradient PPML.",
  "- same_ka rows set log_N_o = log_N_d and occ_similarity = 1 by construction.",
  "- Standard errors are clustered two-way by origin_unit and destination_unit.",
  "",
  if (length(fit_warnings) == 0L) "No model warnings captured." else paste("Warnings:", paste(fit_warnings, collapse = " | ")),
  if (length(fit_errors) == 0L) "No model errors captured." else paste("Errors:", paste(fit_errors, collapse = " | "))
)

writeLines(notes, file.path(out_dir, "distance_specification_notes.txt"))

cat("\nFlow distribution by distance:\n")
print(distance_overall)
cat("\nModel diagnostics:\n")
print(model_diagnostics)
cat("\nFinished. Results written to: ", out_dir, "\n", sep = "")
