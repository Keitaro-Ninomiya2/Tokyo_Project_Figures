################################################################################
# Low-wage draftee destinations and rank boosts, with main-paper-style FE
#
# This is the disciplined follow-up to the raw Misallocation_DestinationDrafteeWage
# split. It asks whether low-wage draftee destinations still predict a promotion-
# style rank boost after conditioning on donor-office/year and origin position.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

OUT_DIR <- here("MainResults", "transferee_selection")

# Reuse the exact data parsing, salary parsing, draft-vacancy definitions, and
# selection-panel construction used by the MNL/misallocation tables.
source(file.path("MainResults", "transferee_selection", "transferee_selection_mlogit.R"))

current_rank_fields <- df %>%
  filter(year_num %in% years_of_interest) %>%
  select(
    staff_id, year_num,
    current_pos_rank = pos_rank,
    current_pos_norm = pos_norm,
    current_drafted = drafted
  )

# Continuous alternative: drafted-worker salary rank within drafted workers in
# the same destination position x year, then averaged over drafted workers in
# the destination office-year and transferee destination position.
draft_position_wage_rank <- df %>%
  filter(drafted == TRUE, !is.na(salary_num), salary_num > 0, !is.na(pos_norm)) %>%
  group_by(year_num, pos_norm) %>%
  mutate(
    draftee_wage_rank_dest_pos_year = if (n() > 1) percent_rank(salary_num) else 0.5
  ) %>%
  ungroup() %>%
  group_by(office_id, year_num, pos_norm) %>%
  summarise(
    dest_pos_year_draftee_wage_rank = mean(draftee_wage_rank_dest_pos_year, na.rm = TRUE),
    n_drafted_dest_pos_year = n(),
    .groups = "drop"
  )

rank_boost_sample <- selection_panel %>%
  filter(
    move_type %in% c("diff_ka", "diff_kyoku"),
    dest_has_draft_vacancy == 1,
    dest_draft_wage_group %in% c("high", "low"),
    has_salary == 1
  ) %>%
  left_join(current_rank_fields, by = c("staff_id", "year_num")) %>%
  left_join(
    draft_position_wage_rank,
    by = c("office_id", "year_num", "current_pos_norm" = "pos_norm")
  ) %>%
  filter(is.na(current_drafted) | current_drafted != TRUE) %>%
  mutate(
    low_wage_draftee = as.integer(dest_draft_wage_group == "low"),
    promoted_table8 = case_when(
      is.na(current_pos_rank) | is.na(lag_pos_rank) ~ NA_integer_,
      TRUE ~ as.integer(current_pos_rank > lag_pos_rank)
    ),
    rank_change_table8 = case_when(
      is.na(current_pos_rank) | is.na(lag_pos_rank) ~ NA_real_,
      TRUE ~ as.numeric(current_pos_rank - lag_pos_rank)
    ),
    donor_office_year = paste(lag_office_id, year_num, sep = "_"),
    donor_ka_year = paste(lag_office_id, lag_ka, year_num, sep = "_"),
    donor_office = lag_office_id,
    origin_position = lag_pos_norm,
    destination_position = current_pos_norm,
    diff_kyoku = as.integer(move_type == "diff_kyoku")
  ) %>%
  filter(
    !is.na(promoted_table8),
    !is.na(low_wage_draftee),
    !is.na(donor_office_year),
    !is.na(origin_position)
  )

variation_report <- rank_boost_sample %>%
  summarise(
    n = n(),
    n_donor_offices = n_distinct(donor_office),
    n_donor_office_year = n_distinct(donor_office_year),
    n_donor_ka_year = n_distinct(donor_ka_year),
    share_low_wage = mean(low_wage_draftee),
    promotion_rate = mean(promoted_table8)
  )

raw_summary <- rank_boost_sample %>%
  group_by(dest_draft_wage_group) %>%
  summarise(
    n = n(),
    n_donor_offices = n_distinct(donor_office),
    promotion_rate = mean(promoted_table8),
    rank_change = mean(rank_change_table8),
    dest_pos_year_wage_rank = mean(dest_pos_year_draftee_wage_rank, na.rm = TRUE),
    .groups = "drop"
  )

fit_model <- function(fml, dat = rank_boost_sample) {
  feols(fml, data = dat, cluster = ~donor_office)
}

models <- list(
  baseline = fit_model(
    promoted_table8 ~ low_wage_draftee |
      donor_office_year + origin_position
  ),
  baseline_controls = fit_model(
    promoted_table8 ~ low_wage_draftee + female + own_tenure + diff_kyoku |
      donor_office_year + origin_position
  ),
  destination_position_fe = fit_model(
    promoted_table8 ~ low_wage_draftee + female + own_tenure + diff_kyoku |
      donor_office_year + origin_position + destination_position
  ),
  donor_ka_year_fe = fit_model(
    promoted_table8 ~ low_wage_draftee + female + own_tenure + diff_kyoku |
      donor_ka_year + origin_position
  ),
  continuous_dest_position_wage_rank = fit_model(
    promoted_table8 ~ dest_pos_year_draftee_wage_rank + female + own_tenure + diff_kyoku |
      donor_office_year + origin_position,
    dat = rank_boost_sample %>% filter(!is.na(dest_pos_year_draftee_wage_rank))
  )
)

rank_change_models <- list(
  rank_change_baseline = fit_model(
    rank_change_table8 ~ low_wage_draftee |
      donor_office_year + origin_position
  ),
  rank_change_controls = fit_model(
    rank_change_table8 ~ low_wage_draftee + female + own_tenure + diff_kyoku |
      donor_office_year + origin_position
  )
)

mechanism_models <- list(
  tenure = fit_model(
    own_tenure ~ low_wage_draftee |
      donor_office_year + origin_position
  ),
  court_rank = fit_model(
    lag_court_rank_ext ~ low_wage_draftee |
      donor_office_year + origin_position
  ),
  decoration_rank = fit_model(
    lag_decor_rank_ext ~ low_wage_draftee |
      donor_office_year + origin_position
  ),
  position_rank_change = fit_model(
    rank_change_table8 ~ low_wage_draftee |
      donor_office_year + origin_position
  )
)

extract_focal <- function(model_list) {
  imap_dfr(model_list, function(mod, spec) {
    focal <- intersect(
      c("low_wage_draftee", "dest_pos_year_draftee_wage_rank"),
      names(coef(mod))
    )
    map_dfr(focal, function(term) {
      ct <- coeftable(mod)
      tibble(
        specification = spec,
        term = term,
        estimate = ct[term, "Estimate"],
        std_error = ct[term, "Std. Error"],
        p_value = ct[term, "Pr(>|t|)"],
        n = nobs(mod)
      )
    })
  })
}

results <- bind_rows(
  extract_focal(models) %>% mutate(outcome = "promoted_table8"),
  extract_focal(rank_change_models) %>% mutate(outcome = "rank_change_table8")
) %>%
  select(outcome, everything())

mechanism_results <- extract_focal(mechanism_models) %>%
  mutate(
    outcome = recode(
      specification,
      tenure = "tenure",
      court_rank = "baseline_court_rank",
      decoration_rank = "baseline_decoration_rank",
      position_rank_change = "position_rank_change"
    )
  ) %>%
  select(outcome, everything())

mechanism_raw_summary <- rank_boost_sample %>%
  group_by(dest_draft_wage_group) %>%
  summarise(
    n = n(),
    tenure = mean(own_tenure, na.rm = TRUE),
    baseline_court_rank = mean(lag_court_rank_ext, na.rm = TRUE),
    baseline_decoration_rank = mean(lag_decor_rank_ext, na.rm = TRUE),
    origin_position_rank = mean(lag_pos_rank, na.rm = TRUE),
    destination_position_rank = mean(current_pos_rank, na.rm = TRUE),
    position_rank_change = mean(rank_change_table8, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(raw_summary, file.path(OUT_DIR, "LowWageRankBoost_FE_RawSummary.csv"))
write_csv(variation_report, file.path(OUT_DIR, "LowWageRankBoost_FE_Variation.csv"))
write_csv(results, file.path(OUT_DIR, "LowWageRankBoost_FE_Results.csv"))
write_csv(mechanism_raw_summary, file.path(OUT_DIR, "LowWageRankBoost_Mechanism_RawSummary.csv"))
write_csv(mechanism_results, file.path(OUT_DIR, "LowWageRankBoost_Mechanism_FE_Results.csv"))

cat("\n========== LOW-WAGE DRAFTEE RANK-BOOST FE TEST ==========\n\n")
cat("Meaningful-effect benchmark: treat >= 0.05 as substantively interesting only\n")
cat("if it is stable across donor-office-year and destination-position FE checks.\n\n")

cat("Sample/variation:\n")
print(variation_report)

cat("\nRaw split using Table-8-style promotion coding:\n")
print(raw_summary, n = Inf)

cat("\nPromotion models:\n")
etable(models)

cat("\nRank-change models:\n")
etable(rank_change_models)

cat("\nMechanism/profile models:\n")
etable(mechanism_models)

cat("\nExported:\n")
cat("  ", file.path(OUT_DIR, "LowWageRankBoost_FE_RawSummary.csv"), "\n")
cat("  ", file.path(OUT_DIR, "LowWageRankBoost_FE_Variation.csv"), "\n")
cat("  ", file.path(OUT_DIR, "LowWageRankBoost_FE_Results.csv"), "\n")
cat("  ", file.path(OUT_DIR, "LowWageRankBoost_Mechanism_RawSummary.csv"), "\n")
cat("  ", file.path(OUT_DIR, "LowWageRankBoost_Mechanism_FE_Results.csv"), "\n")
