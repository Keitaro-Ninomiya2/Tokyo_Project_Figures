################################################################################
# Add salary to the existing 1944 worker-choice dataset and rerun the
# within-origin worker conditional logit on strata with complete salary data.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "SlackDistribution", "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

parse_salary <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c(
      "〇" = "0", "一" = "1", "二" = "2", "三" = "3", "四" = "4",
      "五" = "5", "六" = "6", "七" = "7", "八" = "8", "九" = "9",
      "ã€‡" = "0", "ä¸€" = "1", "äºŒ" = "2", "ä¸‰" = "3", "å››" = "4",
      "äº”" = "5", "å…­" = "6", "ä¸ƒ" = "7", "å…«" = "8", "ä¹" = "9"
    )
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }

  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    is_annual <- str_detect(x, "^(年|å¹´)")
    cleaned <- str_remove(x, "^(月|年|æœˆ|å¹´)")
    if (nchar(cleaned) == 0) return(NA_real_)
    chars <- strsplit(cleaned, "")[[1]]
    digits <- sapply(chars, kanji_to_digit)
    if (any(is.na(digits))) return(NA_real_)
    val <- as.numeric(paste(digits, collapse = ""))
    if (is_annual) val <- val / 12
    val
  }

  sapply(s, parse_one, USE.NAMES = FALSE)
}

run_clogit_safe <- function(formula, data, method = "efron") {
  tryCatch(
    clogit(formula, data = data, method = method),
    error = function(e) NULL
  )
}

worker_choice <- read_csv(
  file.path(result_dir, "worker_selection_1944_choice_dataset.csv"),
  show_col_types = FALSE
)

salary_panel <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(
    staff_id,
    year_num = as.numeric(year),
    salary_raw = salary,
    salary_num = parse_salary(salary),
    has_salary = as.integer(!is.na(salary_num) & salary_num > 0),
    log_salary = if_else(!is.na(salary_num) & salary_num > 0, log(salary_num + 1), NA_real_)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

worker_choice_salary <- worker_choice %>%
  left_join(salary_panel, by = c("staff_id", "year_num"))

salary_event_diag <- worker_choice_salary %>%
  group_by(event_origin_id) %>%
  summarise(
    event_id = first(event_id),
    chosen_distance = first(chosen_distance),
    n_candidates = n(),
    n_selected = sum(chosen),
    salary_candidate_share = mean(has_salary == 1, na.rm = TRUE),
    any_selected_with_salary = any(chosen == 1 & has_salary == 1),
    all_candidates_with_salary = all(has_salary == 1),
    .groups = "drop"
  ) %>%
  mutate(
    usable_salary = n_candidates >= 2 & n_selected >= 1 &
      all_candidates_with_salary & any_selected_with_salary,
    drop_reason_salary = case_when(
      usable_salary ~ "usable_salary",
      n_candidates < 2 ~ "degenerate_choice_set",
      n_selected == 0 ~ "selected_worker_not_in_candidates",
      !any_selected_with_salary ~ "selected_worker_missing_salary",
      !all_candidates_with_salary ~ "candidate_missing_salary",
      TRUE ~ "other"
    )
  )

worker_choice_salary_usable <- worker_choice_salary %>%
  inner_join(
    salary_event_diag %>% filter(usable_salary) %>% select(event_origin_id),
    by = "event_origin_id"
  )

run_worker_specs <- function(dat, spec_label) {
  models <- list(
    full = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat
    ),
    same_section_origin = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat %>% filter(chosen_distance == "same_section")
    ),
    cross_section_origin = run_clogit_safe(
      chosen ~ female + own_tenure + lag_pos_rank + lag_court_rank_ext +
        exact_pos_match + lag_posrank_x_dest_rank + log_salary + strata(event_origin_id),
      data = dat %>% filter(chosen_distance != "same_section")
    )
  )

  split_data <- list(
    full = dat,
    same_section_origin = dat %>% filter(chosen_distance == "same_section"),
    cross_section_origin = dat %>% filter(chosen_distance != "same_section")
  )

  purrr::imap_dfr(models, function(mod, name) {
    dat_spec <- split_data[[name]]
    if (is.null(mod)) {
      return(tibble(
        term = NA_character_, estimate = NA_real_, std.error = NA_real_,
        statistic = NA_real_, p.value = NA_real_, specification = name,
        sample = spec_label, n_event_origins = n_distinct(dat_spec$event_origin_id),
        n_rows = nrow(dat_spec)
      ))
    }
    tidy(mod) %>%
      mutate(
        specification = name,
        sample = spec_label,
        n_event_origins = n_distinct(dat_spec$event_origin_id),
        n_rows = nrow(dat_spec)
      )
  })
}

results <- run_worker_specs(worker_choice_salary_usable, "occ_rank_worker_pool_salary_from_choice")

salary_coverage_summary <- salary_event_diag %>%
  summarise(
    n_event_origins_total = n(),
    n_usable_salary = sum(usable_salary),
    median_salary_candidate_share = median(salary_candidate_share),
    share_all_candidates_with_salary = mean(all_candidates_with_salary),
    share_selected_with_salary = mean(any_selected_with_salary)
  )

write_csv(worker_choice_salary, file.path(result_dir, "worker_selection_1944_salary_from_choice_dataset.csv"))
write_csv(salary_event_diag, file.path(result_dir, "worker_selection_1944_salary_from_choice_event_diagnostic.csv"))
write_csv(salary_coverage_summary, file.path(result_dir, "worker_selection_1944_salary_from_choice_coverage_summary.csv"))
write_csv(results, file.path(result_dir, "worker_selection_1944_salary_from_choice_clogit_results.csv"))

cat("\nSalary-from-choice event diagnostic:\n")
print(salary_event_diag %>% count(drop_reason_salary, sort = TRUE, name = "n_event_origins"), n = Inf)
cat("\nSalary-from-choice coverage summary:\n")
print(salary_coverage_summary)
cat("\nUsable salary event-origin strata:", n_distinct(worker_choice_salary_usable$event_origin_id), "\n")
cat("\nSalary-from-choice worker clogit results:\n")
print(results, n = Inf)
