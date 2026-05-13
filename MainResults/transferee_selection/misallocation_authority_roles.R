################################################################################
# Misallocation of authority-role transfers
#
# Quantify whether non-drafted transferees into draft-vacancy destinations
# receive salary/rank boosts relative to their own baseline, split by whether
# the destination draftee wage is high or low.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

OUT_DIR <- here("MainResults", "transferee_selection")

# Reuse the exact transformed data and Model G sample definitions.
source(file.path("MainResults", "transferee_selection", "transferee_selection_mlogit.R"))

current_rank_fields <- df %>%
  filter(year_num %in% years_of_interest) %>%
  select(
    staff_id, year_num,
    current_pos_rank = pos_rank,
    current_court_rank = court_rank,
    current_decor_rank = decor_rank,
    current_drafted = drafted
  )

draft_relative_wage <- df %>%
  filter(!is.na(salary_num), salary_num > 0) %>%
  mutate(draft_occupation = classify_occ(pos_norm)) %>%
  group_by(year_num, draft_occupation) %>%
  mutate(
    draftee_wage_pct_occ_year = if (n() > 1) percent_rank(salary_num) else rep(0.5, n()),
    draftee_wage_resid_occ_year = salary_num - mean(salary_num, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(drafted == TRUE) %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_drafted_relative_wage = n(),
    dest_draft_wage_pct_occ_year = mean(draftee_wage_pct_occ_year, na.rm = TRUE),
    dest_draft_wage_resid_occ_year = mean(draftee_wage_resid_occ_year, na.rm = TRUE),
    .groups = "drop"
  )

relative_wage_cutoff <- median(draft_relative_wage$dest_draft_wage_pct_occ_year, na.rm = TRUE)

baseline_worker_quality <- selection_panel %>%
  mutate(baseline_year = year_num - 1) %>%
  group_by(baseline_year, occupation) %>%
  mutate(
    baseline_wage_pct_occ_year = if (sum(!is.na(lag_salary_num) & lag_salary_num > 0) > 1) {
      percent_rank(if_else(!is.na(lag_salary_num) & lag_salary_num > 0, lag_salary_num, NA_real_))
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  ungroup() %>%
  select(staff_id, year_num, baseline_wage_pct_occ_year)

misalloc_sample <- selection_panel %>%
  filter(
    move_type %in% c("diff_ka", "diff_kyoku"),
    dest_has_draft_vacancy == 1,
    dest_draft_wage_group %in% c("high", "low"),
    has_salary == 1
  ) %>%
  left_join(draft_relative_wage, by = c("office_id", "year_num")) %>%
  left_join(baseline_worker_quality, by = c("staff_id", "year_num")) %>%
  left_join(current_rank_fields, by = c("staff_id", "year_num")) %>%
  filter(is.na(current_drafted) | current_drafted != TRUE) %>%
  mutate(
    draftee_wage_group = if_else(dest_draft_wage_group == "high", "High-wage draftee", "Low-wage draftee"),
    dest_draft_relative_wage_group = case_when(
      !is.na(dest_draft_wage_pct_occ_year) & dest_draft_wage_pct_occ_year >= relative_wage_cutoff ~ "high",
      !is.na(dest_draft_wage_pct_occ_year) & dest_draft_wage_pct_occ_year < relative_wage_cutoff ~ "low",
      TRUE ~ NA_character_
    ),
    relative_draftee_wage_group = if_else(
      dest_draft_relative_wage_group == "high",
      "High relative-wage draftee",
      "Low relative-wage draftee"
    ),
    has_baseline_salary = !is.na(lag_salary_num) & lag_salary_num > 0,
    salary_change = current_salary_num - lag_salary_num,
    log_salary_change = log(current_salary_num + 1) - log(lag_salary_num + 1),
    salary_increase = salary_change > 0,
    current_pos_rank_ext = coalesce(as.numeric(current_pos_rank), 0),
    baseline_pos_rank_ext = coalesce(as.numeric(lag_pos_rank), 0),
    pos_rank_change = current_pos_rank_ext - baseline_pos_rank_ext,
    pos_rank_boost = pos_rank_change > 0,
    current_court_rank_ext = coalesce(as.numeric(current_court_rank), 0),
    baseline_court_rank_ext = coalesce(as.numeric(lag_court_rank), 0),
    court_rank_change = current_court_rank_ext - baseline_court_rank_ext,
    court_rank_boost = court_rank_change > 0
  )

cat("\n========== MISALLOCATION: AUTHORITY-ROLE BOOSTS ==========\n\n")
cat("Non-drafted transferees into wage-classified draft-vacancy destinations:",
    nrow(misalloc_sample), "\n")
print(misalloc_sample %>% count(draftee_wage_group))
cat("\nBaseline salary coverage:\n")
print(misalloc_sample %>%
  count(draftee_wage_group, has_baseline_salary) %>%
  group_by(draftee_wage_group) %>%
  mutate(share = n / sum(n)) %>%
  ungroup())

summary_by_group <- misalloc_sample %>%
  group_by(draftee_wage_group) %>%
  summarise(
    n_transferees = n(),
    n_salary_change = sum(has_baseline_salary),
    current_salary_mean = mean(current_salary_num, na.rm = TRUE),
    baseline_salary_mean = mean(lag_salary_num[has_baseline_salary], na.rm = TRUE),
    salary_change_mean = mean(salary_change[has_baseline_salary], na.rm = TRUE),
    salary_change_median = median(salary_change[has_baseline_salary], na.rm = TRUE),
    log_salary_change_mean = mean(log_salary_change[has_baseline_salary], na.rm = TRUE),
    share_salary_increase = mean(salary_increase[has_baseline_salary], na.rm = TRUE),
    pos_rank_change_mean = mean(pos_rank_change, na.rm = TRUE),
    share_pos_rank_boost = mean(pos_rank_boost, na.rm = TRUE),
    court_rank_change_mean = mean(court_rank_change, na.rm = TRUE),
    share_court_rank_boost = mean(court_rank_boost, na.rm = TRUE),
    .groups = "drop"
  )

diff_row <- summary_by_group %>%
  pivot_longer(-draftee_wage_group, names_to = "stat", values_to = "value") %>%
  pivot_wider(names_from = draftee_wage_group, values_from = value) %>%
  mutate(
    draftee_wage_group = "Low minus high",
    value = `Low-wage draftee` - `High-wage draftee`
  ) %>%
  select(draftee_wage_group, stat, value) %>%
  pivot_wider(names_from = stat, values_from = value)

summary_table <- bind_rows(summary_by_group, diff_row)

write_csv(summary_table, file.path(OUT_DIR, "Misallocation_DestinationDrafteeWage.csv"))

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", sprintf(paste0("%.", digits, "f"), x))
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "", sprintf("%.1f\\%%", 100 * x))
}

get_val <- function(group, col) {
  summary_table %>%
    filter(draftee_wage_group == group) %>%
    pull({{ col }})
}

groups <- c("High-wage draftee", "Low-wage draftee", "Low minus high")

row_line <- function(label, values) {
  paste0(label, " & ", paste(values, collapse = " & "), " \\\\")
}

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Salary and Rank Boosts for Non-Drafted Transferees into Draft-Vacancy Destinations}",
  "\\label{tab:misallocation-destination-draftee-wage}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & High-wage draftee & Low-wage draftee & Low $-$ High \\\\",
  "\\midrule",
  row_line("Transferees", sapply(groups, function(g) fmt_num(get_val(g, n_transferees), 0))),
  row_line("With baseline salary", sapply(groups, function(g) fmt_num(get_val(g, n_salary_change), 0))),
  row_line("Current salary", sapply(groups, function(g) fmt_num(get_val(g, current_salary_mean), 2))),
  row_line("Baseline salary", sapply(groups, function(g) fmt_num(get_val(g, baseline_salary_mean), 2))),
  row_line("Salary change", sapply(groups, function(g) fmt_num(get_val(g, salary_change_mean), 2))),
  row_line("Median salary change", sapply(groups, function(g) fmt_num(get_val(g, salary_change_median), 2))),
  row_line("Log salary change", sapply(groups, function(g) fmt_num(get_val(g, log_salary_change_mean), 3))),
  row_line("Share with salary increase", sapply(groups, function(g) fmt_pct(get_val(g, share_salary_increase)))),
  "\\midrule",
  row_line("Position-rank change", sapply(groups, function(g) fmt_num(get_val(g, pos_rank_change_mean), 3))),
  row_line("Share with position-rank boost", sapply(groups, function(g) fmt_pct(get_val(g, share_pos_rank_boost)))),
  row_line("Court-rank change", sapply(groups, function(g) fmt_num(get_val(g, court_rank_change_mean), 3))),
  row_line("Share with court-rank boost", sapply(groups, function(g) fmt_pct(get_val(g, share_court_rank_boost)))),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Sample consists of non-drafted workers who transferred to a draft-vacancy ",
    "destination office-year with observed drafted-worker salary, using the same destination wage split as ",
    "Model G. High-wage draftee destinations are office-years where the mean salary of drafted workers is at ",
    "or above the median among draft-vacancy office-years with observed drafted-worker salary; low-wage ",
    "destinations are below that median. Salary changes compare current-year salary to the worker's own ",
    "baseline (lagged) salary, so those rows use only transferees with observed baseline salary. Position rank ",
    "is coded 1--3 from title hierarchy; court rank is coded 0--8 with higher values indicating higher status. ",
    "Rank-change rows code missing ranks as 0, matching the MNL convention."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)

writeLines(tex_lines, file.path(OUT_DIR, "Misallocation_DestinationDrafteeWage.tex"))

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.8in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{Misallocation_DestinationDrafteeWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_misallocation_destination_draftee_wage.tex"))

cat("\nSummary table:\n")
print(summary_table)
cat("\nExported misallocation table to:",
    file.path(OUT_DIR, "Misallocation_DestinationDrafteeWage.tex"), "\n")


# ============================================================
# Regression version: are low-wage draftee destinations more likely
# to require a position-rank boost among non-drafted transferees?
# ============================================================

reg_sample <- misalloc_sample %>%
  mutate(
    low_wage_draftee = as.integer(dest_draft_wage_group == "low"),
    pos_rank_boost_int = as.integer(pos_rank_boost),
    diff_kyoku = as.integer(move_type == "diff_kyoku")
  )

rank_boost_m1 <- feols(pos_rank_boost_int ~ low_wage_draftee, data = reg_sample, vcov = "hetero")
rank_boost_m2 <- feols(
  pos_rank_boost_int ~ low_wage_draftee + female + own_tenure +
    baseline_pos_rank_ext + diff_kyoku,
  data = reg_sample, vcov = "hetero"
)
rank_change_m3 <- feols(pos_rank_change ~ low_wage_draftee, data = reg_sample, vcov = "hetero")
rank_change_m4 <- feols(
  pos_rank_change ~ low_wage_draftee + female + own_tenure +
    baseline_pos_rank_ext + diff_kyoku,
  data = reg_sample, vcov = "hetero"
)

reg_models <- list(rank_boost_m1, rank_boost_m2, rank_change_m3, rank_change_m4)
reg_var_labels <- c(
  low_wage_draftee = "Low-wage draftee destination",
  female = "Female",
  own_tenure = "Tenure (yrs)",
  baseline_pos_rank_ext = "Baseline position rank",
  diff_kyoku = "Different kyoku transfer"
)

format_p_stars <- function(p) {
  case_when(
    p < 0.01 ~ "^{***}",
    p < 0.05 ~ "^{**}",
    p < 0.1 ~ "^{*}",
    TRUE ~ ""
  )
}

format_model_r2 <- function(mod) {
  sprintf("%.3f", fitstat(mod, "r2")$r2)
}

build_reg_row <- function(vname, label) {
  coefs <- c()
  ses <- c()
  for (mod in reg_models) {
    ct <- coeftable(mod)
    if (vname %in% rownames(ct)) {
      b <- ct[vname, "Estimate"]
      s <- ct[vname, "Std. Error"]
      p <- ct[vname, "Pr(>|t|)"]
      coefs <- c(coefs, paste0(" & ", sprintf("%.4f", b), "$", format_p_stars(p), "$"))
      ses <- c(ses, paste0(" & (", sprintf("%.4f", s), ")"))
    } else {
      coefs <- c(coefs, " & ")
      ses <- c(ses, " & ")
    }
  }
  c(
    paste0(label, paste(coefs, collapse = ""), " \\\\"),
    paste0(paste(rep(" ", nchar(label)), collapse = ""),
           paste(ses, collapse = ""), " \\\\[3pt]")
  )
}

reg_body <- c()
for (v in names(reg_var_labels)) {
  reg_body <- c(reg_body, build_reg_row(v, reg_var_labels[[v]]))
}

reg_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Low-Wage Draftee Destinations and Position-Rank Boosts}",
  "\\label{tab:rank-boost-regression-destination-wage}",
  "\\footnotesize",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Position-rank boost} & \\multicolumn{2}{c}{Position-rank change} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  reg_body,
  "\\midrule",
  "Controls & No & Yes & No & Yes \\\\",
  paste0("Observations & ", paste(sapply(reg_models, nobs), collapse = " & "), " \\\\"),
  paste0("$R^2$ & ", paste(sapply(reg_models, format_model_r2), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Sample consists of non-drafted workers who transferred to a draft-vacancy ",
    "destination office-year with observed drafted-worker salary, using the same destination wage split as ",
    "Model G. Columns 1--2 estimate linear probability models for whether the transferee's position rank ",
    "increased relative to baseline. Columns 3--4 estimate the level change in position rank. ",
    "The omitted category is high-wage draftee destination. Controls include gender, tenure, ",
    "baseline position rank, and an indicator for cross-kyoku transfer. Heteroskedasticity-robust ",
    "standard errors are in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)

writeLines(reg_tex, file.path(OUT_DIR, "Misallocation_RankBoost_Regression.tex"))

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.8in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{Misallocation_RankBoost_Regression.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_misallocation_rank_boost_regression.tex"))

cat("\nRegression models:\n")
etable(reg_models)
cat("\nExported rank-boost regression table to:",
    file.path(OUT_DIR, "Misallocation_RankBoost_Regression.tex"), "\n")


# ============================================================
# Relative draftee wage tables:
#   1. Replacement quality before transfer
#   2. Stretch / misallocation after transfer
# ============================================================

relative_reg_sample <- misalloc_sample %>%
  filter(dest_draft_relative_wage_group %in% c("high", "low")) %>%
  mutate(
    low_relative_wage_draftee = as.integer(dest_draft_relative_wage_group == "low"),
    pos_rank_boost_int = as.integer(pos_rank_boost),
    court_rank_boost_int = as.integer(court_rank_boost),
    salary_increase_int = as.integer(salary_increase),
    diff_kyoku = as.integer(move_type == "diff_kyoku")
  )

cat("\n========== RELATIVE DRAFTEE WAGE TABLES ==========\n\n")
cat("Relative wage cutoff (median office-year draftee wage percentile within occupation-year):",
    sprintf("%.3f", relative_wage_cutoff), "\n")
cat("Non-drafted transferees with relative draftee wage classification:",
    nrow(relative_reg_sample), "\n")
print(relative_reg_sample %>% count(relative_draftee_wage_group))
cat("\nBaseline salary coverage in relative-wage sample:\n")
print(relative_reg_sample %>%
  count(relative_draftee_wage_group, has_baseline_salary) %>%
  group_by(relative_draftee_wage_group) %>%
  mutate(share = n / sum(n)) %>%
  ungroup())

replacement_quality_models <- list(
  feols(
    baseline_wage_pct_occ_year ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample, vcov = "hetero"
  ),
  feols(
    lag_salary_num ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample %>% filter(has_baseline_salary), vcov = "hetero"
  ),
  feols(
    baseline_pos_rank_ext ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample, vcov = "hetero"
  ),
  feols(
    baseline_court_rank_ext ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample, vcov = "hetero"
  )
)

stretch_models <- list(
  feols(
    pos_rank_boost_int ~ low_relative_wage_draftee + female + own_tenure +
      baseline_pos_rank_ext + diff_kyoku,
    data = relative_reg_sample, vcov = "hetero"
  ),
  feols(
    court_rank_boost_int ~ low_relative_wage_draftee + female + own_tenure +
      baseline_court_rank_ext + diff_kyoku,
    data = relative_reg_sample, vcov = "hetero"
  ),
  feols(
    salary_increase_int ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample %>% filter(has_baseline_salary), vcov = "hetero"
  ),
  feols(
    salary_change ~ low_relative_wage_draftee + female + own_tenure + diff_kyoku,
    data = relative_reg_sample %>% filter(has_baseline_salary), vcov = "hetero"
  )
)

relative_var_labels <- c(
  low_relative_wage_draftee = "Low relative-wage draftee destination",
  female = "Female",
  own_tenure = "Tenure (yrs)",
  baseline_pos_rank_ext = "Baseline position rank",
  baseline_court_rank_ext = "Baseline court rank",
  diff_kyoku = "Different kyoku transfer"
)

build_model_row <- function(vname, label, models) {
  coefs <- c()
  ses <- c()
  for (mod in models) {
    ct <- coeftable(mod)
    if (vname %in% rownames(ct)) {
      b <- ct[vname, "Estimate"]
      s <- ct[vname, "Std. Error"]
      p <- ct[vname, "Pr(>|t|)"]
      coefs <- c(coefs, paste0(" & ", sprintf("%.4f", b), "$", format_p_stars(p), "$"))
      ses <- c(ses, paste0(" & (", sprintf("%.4f", s), ")"))
    } else {
      coefs <- c(coefs, " & ")
      ses <- c(ses, " & ")
    }
  }
  c(
    paste0(label, paste(coefs, collapse = ""), " \\\\"),
    paste0(paste(rep(" ", nchar(label)), collapse = ""),
           paste(ses, collapse = ""), " \\\\[3pt]")
  )
}

build_tex_reg_table <- function(models, vars, caption, label, col_header, notes, outfile) {
  body <- c()
  for (v in names(vars)) {
    body <- c(body, build_model_row(v, vars[[v]], models))
  }

  tex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    col_header,
    "\\midrule",
    body,
    "\\midrule",
    paste0("Observations & ", paste(sapply(models, nobs), collapse = " & "), " \\\\"),
    paste0("$R^2$ & ", paste(sapply(models, format_model_r2), collapse = " & "), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "}%",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )

  writeLines(tex, file.path(OUT_DIR, outfile))
}

relative_notes <- paste0(
  "Sample consists of non-drafted workers who transferred to a draft-vacancy destination office-year. ",
  "Draftee wage is measured as the drafted worker's wage percentile within occupation-by-year cells, ",
  "then averaged across drafted workers in the destination office-year. The omitted category is a ",
  "high relative-wage draftee destination, defined as at-or-above the median destination office-year ",
  "draftee wage percentile. Heteroskedasticity-robust standard errors are in parentheses. ",
  "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
)

build_tex_reg_table(
  replacement_quality_models,
  relative_var_labels[c("low_relative_wage_draftee", "female", "own_tenure", "diff_kyoku")],
  "Replacement Quality by Relative Draftee Wage",
  "tab:replacement-quality-relative-draftee-wage",
  paste0(
    " & Baseline wage percentile & Baseline salary & Baseline position rank & Baseline court rank \\\\",
    "\n & (1) & (2) & (3) & (4) \\\\"
  ),
  paste0(
    relative_notes,
    " Outcomes measure the transferee's pre-transfer quality using lagged/baseline characteristics. ",
    "Baseline wage percentile is measured within the transferee's own baseline occupation-year. ",
    "Baseline salary columns use only workers with observed lagged salary."
  ),
  "Misallocation_ReplacementQuality_RelativeDrafteeWage.tex"
)

build_tex_reg_table(
  stretch_models,
  relative_var_labels,
  "Stretch and Misallocation by Relative Draftee Wage",
  "tab:stretch-misallocation-relative-draftee-wage",
  paste0(
    " & Position-rank boost & Court-rank boost & Salary increase & Salary change \\\\",
    "\n & (1) & (2) & (3) & (4) \\\\"
  ),
  paste0(
    relative_notes,
    " Outcomes measure whether the transferee had to be elevated relative to baseline. ",
    "Columns 1 and 2 control for the corresponding baseline rank. Salary columns use only workers ",
    "with observed lagged salary, so they should be read as lower-powered robustness checks."
  ),
  "Misallocation_Stretch_RelativeDrafteeWage.tex"
)

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.8in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{caption}",
  "\\usepackage{graphicx}",
  "\\begin{document}",
  "\\input{Misallocation_ReplacementQuality_RelativeDrafteeWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_misallocation_replacement_quality_relative_wage.tex"))

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.8in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{caption}",
  "\\usepackage{graphicx}",
  "\\begin{document}",
  "\\input{Misallocation_Stretch_RelativeDrafteeWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_misallocation_stretch_relative_wage.tex"))

cat("\nReplacement quality models:\n")
etable(replacement_quality_models)
cat("\nStretch / misallocation models:\n")
etable(stretch_models)
cat("\nExported relative-wage replacement and stretch tables.\n")
