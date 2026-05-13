################################################################################
# Within-mover test: destination-seat wage relative to origin-seat wage
#
# Among non-drafted transferees into draft-vacancy destinations, test whether
# movers into higher-wage drafted seats have larger subsequent career gains than
# movers from the same donor cell into same/lower-wage drafted seats.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

OUT_DIR <- here("MainResults", "transferee_selection")

# Reuse the exact parsing, draft-vacancy, and selection-panel definitions.
source(file.path("MainResults", "transferee_selection", "transferee_selection_mlogit.R"))

postwar_years <- 1947:1955

origin_seat_wage <- df %>%
  filter(!is.na(salary_num), salary_num > 0) %>%
  group_by(
    lag_office_id = office_id,
    lag_ka = ka,
    lag_pos_norm = pos_norm,
    baseline_year = year_num
  ) %>%
  summarise(
    origin_seat_salary_mean = mean(salary_num, na.rm = TRUE),
    origin_seat_salary_median = median(salary_num, na.rm = TRUE),
    origin_seat_n_salary = n(),
    .groups = "drop"
  )

current_rank_fields <- df %>%
  filter(year_num %in% years_of_interest) %>%
  select(
    staff_id, year_num,
    current_pos_rank = pos_rank,
    current_court_rank = court_rank,
    current_drafted = drafted
  )

worker_outcomes <- df %>%
  group_by(staff_id) %>%
  summarise(
    max_rank = max(pos_rank, na.rm = TRUE),
    last_year = max(year_num),
    first_year = min(year_num),
    .groups = "drop"
  )

postwar_outcomes <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(postwar_years = n_distinct(year_num), .groups = "drop")

within_mover_sample <- selection_panel %>%
  filter(
    move_type %in% c("diff_ka", "diff_kyoku"),
    dest_has_draft_vacancy == 1,
    !is.na(draft_salary_mean),
    has_salary == 1
  ) %>%
  mutate(baseline_year = year_num - 1) %>%
  left_join(
    origin_seat_wage,
    by = c("lag_office_id", "lag_ka", "lag_pos_norm", "baseline_year")
  ) %>%
  left_join(current_rank_fields, by = c("staff_id", "year_num")) %>%
  left_join(worker_outcomes, by = "staff_id") %>%
  left_join(postwar_outcomes, by = "staff_id") %>%
  filter(is.na(current_drafted) | current_drafted != TRUE) %>%
  mutate(
    donor_cell = paste(lag_office_id, lag_ka, lag_pos_norm, year_num, sep = "_"),
    years_after = last_year - year_num,
    postwar_survived = as.integer(!is.na(postwar_years) & postwar_years > 0),
    postwar_years = replace_na(postwar_years, 0L),
    rank_gain = max_rank - lag_pos_rank,
    tenure_at_transfer = year_num - first_yr,
    diff_kyoku = as.integer(move_type == "diff_kyoku"),
    has_origin_seat_wage = !is.na(origin_seat_salary_mean) & origin_seat_salary_mean > 0,
    dest_minus_origin_seat_wage = draft_salary_mean - origin_seat_salary_mean,
    dest_log_origin_seat_wage_gap = log(draft_salary_mean + 1) - log(origin_seat_salary_mean + 1),
    higher_than_origin_seat = as.integer(dest_minus_origin_seat_wage > 0),
    same_or_lower_than_origin_seat = as.integer(dest_minus_origin_seat_wage <= 0),
    dest_minus_own_lag_wage = draft_salary_mean - lag_salary_num,
    dest_log_own_lag_wage_gap = log(draft_salary_mean + 1) - log(lag_salary_num + 1),
    higher_than_own_lag_wage = as.integer(dest_minus_own_lag_wage > 0),
    current_pos_rank_ext = coalesce(as.numeric(current_pos_rank), 0),
    baseline_pos_rank_ext = coalesce(as.numeric(lag_pos_rank), 0),
    pos_rank_change = current_pos_rank_ext - baseline_pos_rank_ext,
    pos_rank_boost = as.integer(pos_rank_change > 0),
    salary_change = current_salary_num - lag_salary_num,
    salary_increase = as.integer(salary_change > 0)
  ) %>%
  filter(!is.na(donor_cell), !is.na(rank_gain), has_origin_seat_wage)

variation_cells <- within_mover_sample %>%
  group_by(donor_cell) %>%
  summarise(
    n = n(),
    n_higher = sum(higher_than_origin_seat == 1, na.rm = TRUE),
    n_same_lower = sum(higher_than_origin_seat == 0, na.rm = TRUE),
    gap_sd = sd(dest_minus_origin_seat_wage, na.rm = TRUE),
    .groups = "drop"
  )

mixed_binary_cells <- variation_cells %>%
  filter(n_higher > 0, n_same_lower > 0) %>%
  select(donor_cell)

continuous_variation_cells <- variation_cells %>%
  filter(!is.na(gap_sd), gap_sd > 0) %>%
  select(donor_cell)

binary_sample <- within_mover_sample %>%
  semi_join(mixed_binary_cells, by = "donor_cell")

continuous_sample <- within_mover_sample %>%
  semi_join(continuous_variation_cells, by = "donor_cell")

summarise_groups <- function(dat, group_var) {
  dat %>%
    group_by({{ group_var }}) %>%
    summarise(
      n = n(),
      n_cells = n_distinct(donor_cell),
      dest_draft_salary = mean(draft_salary_mean, na.rm = TRUE),
      origin_seat_salary = mean(origin_seat_salary_mean, na.rm = TRUE),
      wage_gap = mean(dest_minus_origin_seat_wage, na.rm = TRUE),
      years_after = mean(years_after, na.rm = TRUE),
      postwar_survived = mean(postwar_survived, na.rm = TRUE),
      postwar_years = mean(postwar_years, na.rm = TRUE),
      rank_gain = mean(rank_gain, na.rm = TRUE),
      pos_rank_boost = mean(pos_rank_boost, na.rm = TRUE),
      salary_change = mean(salary_change, na.rm = TRUE),
      salary_increase = mean(salary_increase, na.rm = TRUE),
      .groups = "drop"
    )
}

group_summary <- within_mover_sample %>%
  mutate(dest_vs_origin = if_else(higher_than_origin_seat == 1,
                                  "Higher than origin seat",
                                  "Same/lower than origin seat")) %>%
  summarise_groups(dest_vs_origin)

mixed_group_summary <- binary_sample %>%
  mutate(dest_vs_origin = if_else(higher_than_origin_seat == 1,
                                  "Higher than origin seat",
                                  "Same/lower than origin seat")) %>%
  summarise_groups(dest_vs_origin)

write_csv(group_summary, file.path(OUT_DIR, "WithinMover_DestOriginWage_Summary.csv"))
write_csv(mixed_group_summary, file.path(OUT_DIR, "WithinMover_DestOriginWage_MixedCells_Summary.csv"))

fit_binary <- function(y) {
  feols(
    as.formula(paste0(
      y,
      " ~ higher_than_origin_seat + baseline_pos_rank_ext + tenure_at_transfer + female + diff_kyoku | donor_cell"
    )),
    data = binary_sample,
    cluster = ~donor_cell
  )
}

fit_continuous <- function(y) {
  feols(
    as.formula(paste0(
      y,
      " ~ dest_log_origin_seat_wage_gap + baseline_pos_rank_ext + tenure_at_transfer + female + diff_kyoku | donor_cell"
    )),
    data = continuous_sample,
    cluster = ~donor_cell
  )
}

fit_own_wage <- function(y) {
  feols(
    as.formula(paste0(
      y,
      " ~ dest_log_own_lag_wage_gap + baseline_pos_rank_ext + tenure_at_transfer + female + diff_kyoku | donor_cell"
    )),
    data = continuous_sample %>% filter(!is.na(dest_log_own_lag_wage_gap)),
    cluster = ~donor_cell
  )
}

outcomes <- c("years_after", "postwar_survived", "postwar_years", "rank_gain",
              "pos_rank_boost", "salary_change", "salary_increase")

binary_models <- set_names(map(outcomes, fit_binary), outcomes)
continuous_models <- set_names(map(outcomes, fit_continuous), outcomes)
own_wage_models <- set_names(map(outcomes, fit_own_wage), outcomes)

extract_term <- function(models, term, label) {
  imap_dfr(models, function(mod, outcome) {
    ct <- coeftable(mod)
    tibble(
      specification = label,
      outcome = outcome,
      term = term,
      estimate = ct[term, "Estimate"],
      std_error = ct[term, "Std. Error"],
      p_value = ct[term, "Pr(>|t|)"],
      n = nobs(mod)
    )
  })
}

regression_results <- bind_rows(
  extract_term(binary_models, "higher_than_origin_seat",
               "Binary: destination drafted-seat wage > origin-seat mean wage"),
  extract_term(continuous_models, "dest_log_origin_seat_wage_gap",
               "Continuous: log destination drafted-seat wage - log origin-seat mean wage"),
  extract_term(own_wage_models, "dest_log_own_lag_wage_gap",
               "Robustness: log destination drafted-seat wage - log mover lag wage")
)

write_csv(regression_results, file.path(OUT_DIR, "WithinMover_DestOriginWage_Regressions.csv"))

cat("\n========== WITHIN-MOVER DESTINATION VS ORIGIN-SEAT WAGE TEST ==========\n\n")
cat("Full non-drafted mover sample with destination drafted wage and origin-seat wage:",
    nrow(within_mover_sample), "\n")
cat("Donor cells:", n_distinct(within_mover_sample$donor_cell), "\n")
cat("Cells with binary high/same-lower variation:", nrow(mixed_binary_cells), "\n")
cat("Binary FE sample:", nrow(binary_sample), "movers\n")
cat("Cells with continuous wage-gap variation:", nrow(continuous_variation_cells), "\n")
cat("Continuous FE sample:", nrow(continuous_sample), "movers\n\n")

cat("Unconditional summary, all eligible movers:\n")
print(group_summary, n = Inf)

cat("\nWithin-cell mixed-binary summary:\n")
print(mixed_group_summary, n = Inf)

cat("\nBinary FE models: higher destination drafted-seat wage than origin-seat wage\n")
etable(binary_models)

cat("\nContinuous FE models: log destination drafted-seat wage minus log origin-seat wage\n")
etable(continuous_models)

cat("\nOwn-wage robustness: log destination drafted-seat wage minus log mover's lag wage\n")
etable(own_wage_models)

cat("\nExported:\n")
cat("  ", file.path(OUT_DIR, "WithinMover_DestOriginWage_Summary.csv"), "\n")
cat("  ", file.path(OUT_DIR, "WithinMover_DestOriginWage_MixedCells_Summary.csv"), "\n")
cat("  ", file.path(OUT_DIR, "WithinMover_DestOriginWage_Regressions.csv"), "\n")
