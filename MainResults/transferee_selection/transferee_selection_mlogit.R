################################################################################
# Transferee Selection into Draft-Induced Vacancies
#
# For each transfer-distance category, estimate a SEPARATE multinomial logit:
#   Outcome (3 categories per model):
#     0 = Not transferred at this distance (base)
#     1 = Transferred to draft-vacancy office
#     2 = Transferred to non-draft-vacancy office
#
#   Three models:
#     A. Retention: stay vs. leave to draft-vacancy vs. leave to non-draft
#     B. Within-kyoku (diff ka): among those who could move within kyoku
#     C. Cross-kyoku (diff kyoku): among those who could move across kyoku
#
# Predictors:
#   - Decoration rank (0-8, continuous; 0 = no decoration)
#   - Court rank (0-8, continuous; 0 = no rank)
#   - Female indicator
#   - Tenure (years in data)
#   - Log salary (current year)
#
# Unit: worker x year (all workers with prior-year record), 1938-1945
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(nnet)        # multinom()
  library(fixest)      # for comparison LPM
  library(broom)
  library(here)
})

OUT_DIR <- here("MainResults", "transferee_selection")

# ---- Load data ----
DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

cat("Loading data from:", DATA_PATH, "\n")
df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df <- df_raw %>% filter(is_name == TRUE)

# ---- Helper functions (from TransferOutcomes_DistDept.R) ----

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
    str_detect(k, "社会")       ~ "社会局",
    str_detect(k, "保健")       ~ "保健局",
    str_detect(k, "上木")       ~ "土木局",
    str_detect(k, "土木")       ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務")   ~ "府会事務局",
    str_detect(k, "發育")       ~ "教育局",
    str_detect(k, "教育")       ~ "教育局",
    str_detect(k, "水違")       ~ "水道局",
    str_detect(k, "水道")       ~ "水道局",
    str_detect(k, "電気")       ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務")       ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",
    str_detect(k, "経済")       ~ "経済局",
    str_detect(k, "厚生")       ~ "厚生局",
    str_detect(k, "市民")       ~ "市民局",
    str_detect(k, "中央卸売")   ~ "中央卸売市場",
    str_detect(k, "港準")       ~ "港湾局",
    str_detect(k, "港湾")       ~ "港湾局",
    str_detect(k, "監査")       ~ "監査局",
    str_detect(k, "産業")       ~ "産業局",
    str_detect(k, "城東病院")   ~ "健民局",
    str_detect(k, "健民")       ~ "健民局",
    str_detect(k, "戰時生活")   ~ "戰時生活局",
    str_detect(k, "経〓")       ~ "経理局",
    str_detect(k, "経理")       ~ "経理局",
    str_detect(k, "労働")       ~ "労働局",
    str_detect(k, "建築|御築")  ~ "建築局",
    str_detect(k, "道路")       ~ "道路局",
    str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通")       ~ "交通局",
    str_detect(k, "計[畫画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醉院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "技") ~ "engineer",
    str_detect(pos, "雇|傭") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

parse_court_rank <- function(r) {
  case_when(
    is.na(r) ~ NA_integer_,
    str_detect(r, "^正八")  ~ 1L,
    str_detect(r, "^正七")  ~ 2L,
    str_detect(r, "^正六")  ~ 3L,
    str_detect(r, "^正五")  ~ 4L,
    str_detect(r, "^正四")  ~ 5L,
    str_detect(r, "^正三")  ~ 6L,
    str_detect(r, "^正二")  ~ 7L,
    str_detect(r, "^正一")  ~ 8L,
    str_detect(r, "^正十")  ~ 0L,
    TRUE ~ NA_integer_
  )
}

parse_decoration <- function(d) {
  case_when(
    is.na(d)                  ~ NA_integer_,
    str_detect(d, "^勳八|^勳十") ~ 1L,
    str_detect(d, "^勳七")    ~ 2L,
    str_detect(d, "^勳六")    ~ 3L,
    str_detect(d, "^勳五")    ~ 4L,
    str_detect(d, "^勳四")    ~ 5L,
    str_detect(d, "^勳三")    ~ 6L,
    str_detect(d, "^勳二")    ~ 7L,
    str_detect(d, "^勳一")    ~ 8L,
    TRUE                      ~ NA_integer_
  )
}

# ---- Parse salary (Japanese kanji numerals) ----
# Salary is encoded as e.g. 月八五 (monthly 85 yen), 年三六〇 (annual 360 yen)
parse_salary <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c("〇"="0", "一"="1", "二"="2", "三"="3", "四"="4",
             "五"="5", "六"="6", "七"="7", "八"="8", "九"="9")
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }

  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    # Strip 月/年 prefix (monthly vs annual indicator)
    is_annual <- str_detect(x, "^年")
    cleaned <- str_remove(x, "^[月年]")
    if (nchar(cleaned) == 0) return(NA_real_)
    # Convert each kanji digit
    chars <- strsplit(cleaned, "")[[1]]
    digits <- sapply(chars, kanji_to_digit)
    if (any(is.na(digits))) return(NA_real_)
    val <- as.numeric(paste(digits, collapse = ""))
    # Normalize to monthly: if annual, divide by 12
    if (is_annual) val <- val / 12
    val
  }

  sapply(s, parse_one, USE.NAMES = FALSE)
}

# ---- Apply transformations ----
df <- df %>%
  mutate(
    norm_kyoku = normalize_kyoku(kyoku),
    pos_rank   = assign_rank(pos_norm, year_num),
    court_rank = parse_court_rank(rank),
    decor_rank = parse_decoration(decoration),
    salary_num = parse_salary(salary)
  )

# ---- Tenure: first year observed, by gov_level ----
# Compute tenure as years since first observed in the dataset
tenure_df <- df %>%
  group_by(staff_id) %>%
  summarise(first_yr = min(year_num), .groups = "drop")

# Compute tenure specifically in Shi vs Fu
# gov_level indicates whether observation is TokyoShi, TokyoFu, or TokyoTo
tenure_by_gov <- df %>%
  filter(gov_level %in% c("TokyoShi", "TokyoFu")) %>%
  group_by(staff_id, gov_level) %>%
  summarise(first_yr_gov = min(year_num),
            n_years_gov  = n_distinct(year_num),
            .groups = "drop") %>%
  pivot_wider(names_from = gov_level,
              values_from = c(first_yr_gov, n_years_gov),
              names_sep = "_")

years_of_interest <- 1938:1945

# ---- Identify draft-induced vacancies ----
# An office x year has a draft vacancy if >=1 worker was drafted that year.
# For the drafted-worker-wage split, classify office-years by the mean
# monthly salary of drafted workers in that office-year. The high/low cutoff
# is the median across draft-vacancy office-years with observed salaries.
office_draft_salary <- df %>%
  filter(drafted == TRUE) %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_drafted = n(),
    n_drafted_salary = sum(!is.na(salary_num) & salary_num > 0),
    draft_salary_mean = if_else(
      n_drafted_salary > 0,
      mean(salary_num[!is.na(salary_num) & salary_num > 0]),
      NA_real_
    ),
    .groups = "drop"
  )

draft_salary_cutoff <- median(office_draft_salary$draft_salary_mean, na.rm = TRUE)

office_drafts <- office_draft_salary %>%
  mutate(
    dest_has_draft_vacancy = 1L,
    dest_draft_wage_group = case_when(
      !is.na(draft_salary_mean) & draft_salary_mean >= draft_salary_cutoff ~ "high",
      !is.na(draft_salary_mean) & draft_salary_mean <  draft_salary_cutoff ~ "low",
      TRUE ~ NA_character_
    ),
    dest_has_high_wage_draft = as.integer(dest_draft_wage_group == "high"),
    dest_has_low_wage_draft  = as.integer(dest_draft_wage_group == "low")
  )

cat("Draft-vacancy office-years:", nrow(office_drafts), "\n")
cat("Drafted-worker wage cutoff (median office-year mean monthly salary):",
    sprintf("%.2f", draft_salary_cutoff), "\n")
cat("  High-wage draft office-years:",
    sum(office_drafts$dest_has_high_wage_draft == 1, na.rm = TRUE), "\n")
cat("  Low-wage draft office-years:",
    sum(office_drafts$dest_has_low_wage_draft == 1, na.rm = TRUE), "\n")

# ---- Build lag panel ----
staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, norm_kyoku, pos_norm,
         pos_rank, court_rank, decor_rank, salary_num, gov_level) %>%
  rename_with(~paste0("lag_", .), -c(staff_id, year_num)) %>%
  mutate(year_num = year_num + 1)

# ---- Selection panel: start from all workers in t-1, track what happens in t ----
current_yr <- df %>%
  filter(year_num %in% years_of_interest) %>%
  select(staff_id, year_num, ka, norm_kyoku, office_id, is_female,
         current_salary_num = salary_num)

selection_panel <- staff_lag %>%
  filter(year_num %in% years_of_interest,
         !is.na(lag_ka), !is.na(lag_norm_kyoku)) %>%
  left_join(current_yr, by = c("staff_id", "year_num")) %>%
  mutate(
    # Basic movement classification
    move_type = case_when(
      is.na(ka) | is.na(norm_kyoku)                  ~ "exit",
      lag_ka == ka & lag_norm_kyoku == norm_kyoku      ~ "same_ka",
      lag_ka != ka & lag_norm_kyoku == norm_kyoku      ~ "diff_ka",
      lag_norm_kyoku != norm_kyoku                     ~ "diff_kyoku"
    ),
    is_female = coalesce(is_female, FALSE),
    occupation = classify_occ(lag_pos_norm)
  ) %>%
  # Flag whether destination office had a draft-induced vacancy
  left_join(office_drafts, by = c("office_id" = "office_id", "year_num" = "year_num")) %>%
  mutate(
    dest_has_draft_vacancy = replace_na(dest_has_draft_vacancy, 0L),
    dest_has_high_wage_draft = replace_na(dest_has_high_wage_draft, 0L),
    dest_has_low_wage_draft = replace_na(dest_has_low_wage_draft, 0L)
  ) %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(
    own_tenure = year_num - first_yr,
    log_salary = log(current_salary_num + 1),
    has_salary = as.integer(!is.na(current_salary_num) & current_salary_num > 0),
    lag_decor_rank_ext = coalesce(as.numeric(lag_decor_rank), 0),
    lag_court_rank_ext = coalesce(as.numeric(lag_court_rank), 0),
    female = as.integer(is_female),
    yr_fac = factor(year_num)
  )

cat("\n========== TRANSFEREE SELECTION INTO DRAFT VACANCIES ==========\n\n")
cat("Full panel:", nrow(selection_panel), "worker-year obs\n")
cat("  Exit:        ", sum(selection_panel$move_type == "exit"), "\n")
cat("  Same ka:     ", sum(selection_panel$move_type == "same_ka"), "\n")
cat("  Diff ka:     ", sum(selection_panel$move_type == "diff_ka"), "\n")
cat("  Diff kyoku:  ", sum(selection_panel$move_type == "diff_kyoku"), "\n")
cat("  With salary: ", sum(selection_panel$has_salary == 1), "\n")
cat("  Dest has draft vacancy (among movers):",
    sum(selection_panel$dest_has_draft_vacancy == 1 & selection_panel$move_type != "exit"), "\n\n")


# ============================================================
# MODEL A: RETENTION
# Who leaves (to a draft-vacancy vs. non-draft-vacancy office)
# vs. who stays?
#
# Sample: all workers present in t-1 (excluding exits from data)
# Outcome: {stayed, left_to_draft_vacancy, left_to_non_draft}
# ============================================================

cat("========== MODEL A: RETENTION ==========\n\n")

dat_A <- selection_panel %>%
  filter(move_type != "exit") %>%        # keep workers who appear in t
  mutate(
    moved = as.integer(move_type != "same_ka"),
    outcome_A = case_when(
      moved == 0                       ~ "stayed",
      dest_has_draft_vacancy == 1      ~ "left_draft",
      TRUE                             ~ "left_nondraft"
    ),
    outcome_A = factor(outcome_A, levels = c("stayed", "left_draft", "left_nondraft"))
  )

cat("  Stayed:           ", sum(dat_A$outcome_A == "stayed"), "\n")
cat("  Left to draft:    ", sum(dat_A$outcome_A == "left_draft"), "\n")
cat("  Left to non-draft:", sum(dat_A$outcome_A == "left_nondraft"), "\n")

# With salary
dat_A_sal <- dat_A %>% filter(has_salary == 1)
cat("  With salary:      ", nrow(dat_A_sal), "\n\n")

mA_full <- multinom(
  outcome_A ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_A_sal, trace = FALSE, maxit = 500
)

# Same salary-inclusive specification kept for the legacy two-spec table export.
mA_nosal <- multinom(
  outcome_A ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_A_sal, trace = FALSE, maxit = 500
)

cat("--- Model A (with salary) ---\n")
summary(mA_full)
cat("\n--- Model A (with salary, duplicate legacy spec) ---\n")
summary(mA_nosal)


# ============================================================
# MODEL B: WITHIN-KYOKU TRANSFERS (diff ka, same kyoku)
# Among workers who could transfer within kyoku, who goes to
# a draft-vacancy office?
#
# Sample: stayed + diff_ka movers
# Outcome: {stayed, diff_ka_draft, diff_ka_nondraft}
# ============================================================

cat("\n========== MODEL B: WITHIN-KYOKU TRANSFERS ==========\n\n")

dat_B <- selection_panel %>%
  filter(move_type %in% c("same_ka", "diff_ka")) %>%
  mutate(
    outcome_B = case_when(
      move_type == "same_ka"            ~ "stayed",
      dest_has_draft_vacancy == 1       ~ "diffka_draft",
      TRUE                              ~ "diffka_nondraft"
    ),
    outcome_B = factor(outcome_B, levels = c("stayed", "diffka_draft", "diffka_nondraft"))
  )

cat("  Stayed:               ", sum(dat_B$outcome_B == "stayed"), "\n")
cat("  Diff-ka to draft:     ", sum(dat_B$outcome_B == "diffka_draft"), "\n")
cat("  Diff-ka to non-draft: ", sum(dat_B$outcome_B == "diffka_nondraft"), "\n")

dat_B_sal <- dat_B %>% filter(has_salary == 1)
cat("  With salary:          ", nrow(dat_B_sal), "\n")

# Check for complete separation: too few diffka_draft in salary subsample
n_diffka_draft_sal <- sum(dat_B_sal$outcome_B == "diffka_draft")
cat("  diffka_draft with salary:", n_diffka_draft_sal, "\n\n")

mB_nosal <- multinom(
  outcome_B ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_B_sal, trace = FALSE, maxit = 500
)

# Complete separation in salary subsample — too few diffka_draft events
# even without salary as regressor. Set mB_full = NULL to produce a
# single-spec table (full sample only).
cat("  *** Model B now uses the salary subsample with log_salary. ***\n")
mB_full <- NULL

cat("--- Model B (with salary) ---\n")
summary(mB_nosal)
cat("\n--- Model B (unused legacy full slot) ---\n")
summary(mB_full)


# ============================================================
# MODEL C: CROSS-KYOKU TRANSFERS (diff kyoku)
# Among workers who could transfer across kyoku, who goes to
# a draft-vacancy office?
#
# Sample: stayed + diff_kyoku movers
# Outcome: {stayed, diff_kyoku_draft, diff_kyoku_nondraft}
# ============================================================

cat("\n========== MODEL C: CROSS-KYOKU TRANSFERS ==========\n\n")

dat_C <- selection_panel %>%
  filter(move_type %in% c("same_ka", "diff_kyoku")) %>%
  mutate(
    outcome_C = case_when(
      move_type == "same_ka"            ~ "stayed",
      dest_has_draft_vacancy == 1       ~ "diffkyoku_draft",
      TRUE                              ~ "diffkyoku_nondraft"
    ),
    outcome_C = factor(outcome_C, levels = c("stayed", "diffkyoku_draft", "diffkyoku_nondraft"))
  )

cat("  Stayed:                  ", sum(dat_C$outcome_C == "stayed"), "\n")
cat("  Diff-kyoku to draft:     ", sum(dat_C$outcome_C == "diffkyoku_draft"), "\n")
cat("  Diff-kyoku to non-draft: ", sum(dat_C$outcome_C == "diffkyoku_nondraft"), "\n")

dat_C_sal <- dat_C %>% filter(has_salary == 1)
cat("  With salary:             ", nrow(dat_C_sal), "\n\n")

mC_full <- multinom(
  outcome_C ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_C_sal, trace = FALSE, maxit = 500
)

mC_nosal <- multinom(
  outcome_C ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_C_sal, trace = FALSE, maxit = 500
)

cat("--- Model C (with salary) ---\n")
summary(mC_full)
cat("\n--- Model C (with salary, duplicate legacy spec) ---\n")
summary(mC_nosal)


# ============================================================
# Export results to LaTeX — one table per model (A, B, C)
# Each table: 2 outcome columns (draft / non-draft) x 2 specs
# ============================================================

format_stars <- function(p) {
  case_when(
    p < 0.01  ~ "^{***}",
    p < 0.05  ~ "^{**}",
    p < 0.1   ~ "^{*}",
    TRUE       ~ ""
  )
}

var_names  <- c("lag_decor_rank_ext", "lag_court_rank_ext", "female",
                "own_tenure", "log_salary")
var_labels <- c("Decoration rank (0--8; high=better)",
                "Court rank (0--8; high=better)",
                "Female",
                "Tenure (yrs)",
                "Log salary (current year)")

# Generic: build LaTeX table for one panel (nosal vs full spec)
build_panel_table <- function(mod_nosal, mod_full, outcome_levels, outcome_labels,
                              caption, label, notes) {

  build_row <- function(vname, vlabel, models, outcomes) {
    coef_cells <- c()
    se_cells   <- c()
    for (mod in models) {
      cc <- coef(mod)
      ss <- summary(mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      for (o in outcomes) {
        if (vname %in% colnames(cc)) {
          b <- cc[o, vname]
          s <- ss[o, vname]
          p <- pp[o, vname]
          stars <- format_stars(p)
          coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
          se_cells   <- c(se_cells,   paste0(" & (", sprintf("%.4f", s), ")"))
        } else {
          coef_cells <- c(coef_cells, " & ")
          se_cells   <- c(se_cells,   " & ")
        }
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  models <- list(mod_nosal, mod_full)
  n_out  <- length(outcome_levels)

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v], models, outcome_levels))
  }

  header_top <- paste0(
    " & \\multicolumn{", n_out, "}{c}{(1) With salary}",
    " & \\multicolumn{", n_out, "}{c}{(2) With salary} \\\\"
  )
  header_sub <- paste0(
    " & ", paste(rep(outcome_labels, 2), collapse = " & "), " \\\\"
  )

  n_nosal <- nrow(mod_nosal$fitted.values)
  n_full  <- nrow(mod_full$fitted.values)

  footer <- c(
    paste0("Year FE & \\multicolumn{", n_out, "}{c}{Yes} & \\multicolumn{", n_out, "}{c}{Yes} \\\\"),
    paste0("Salary control & \\multicolumn{", n_out, "}{c}{Yes} & \\multicolumn{", n_out, "}{c}{Yes} \\\\"),
    paste0("Observations & \\multicolumn{", n_out, "}{c}{", format(n_nosal, big.mark = ","),
           "} & \\multicolumn{", n_out, "}{c}{", format(n_full, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", as.numeric(logLik(mod_nosal))),
           "} & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", as.numeric(logLik(mod_full))), "} \\\\"),
    paste0("AIC & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", AIC(mod_nosal)),
           "} & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", AIC(mod_full)), "} \\\\")
  )

  col_spec <- paste0("l", paste(rep("c", n_out * 2), collapse = ""))

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\footnotesize",
    "\\begin{threeparttable}",
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    header_top,
    paste0("\\cmidrule(lr){2-", 1 + n_out, "} \\cmidrule(lr){", 2 + n_out, "-", 1 + 2*n_out, "}"),
    header_sub,
    "\\midrule",
    body_lines,
    "\\midrule",
    footer,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

# ---- Table A: Retention ----
tex_A <- build_panel_table(
  mA_nosal, mA_full,
  outcome_levels = c("left_draft", "left_nondraft"),
  outcome_labels = c("Left: Draft Vac.", "Left: Non-Draft"),
  caption = "Retention: Who Leaves to Fill a Draft-Induced Vacancy?",
  label = "tab:selection-retention",
  notes = paste0(
    "Multinomial logit. Base category: stayed (same ka and kyoku as prior year). ",
    "``Left: Draft Vac.'' = worker moved to an office that had $\\geq$1 drafted worker that year; ",
    "``Left: Non-Draft'' = moved to an office without a draft vacancy. ",
    "Decoration and court rank coded 0 when unobserved. ",
    "All specifications use the salary subsample and include year FE, tenure, ",
    "and current-year log salary. ",
    "Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)
writeLines(tex_A, file.path(OUT_DIR, "MLogit_A_Retention.tex"))
cat("\nExported Table A to:", file.path(OUT_DIR, "MLogit_A_Retention.tex"), "\n")

# ---- Table B: Within-kyoku transfers (single salary-inclusive spec) ----
build_single_table <- function(mod, outcome_levels, outcome_labels,
                               caption, label, notes) {
  n_out <- length(outcome_levels)
  cc <- coef(mod); ss <- summary(mod)$standard.errors
  pp <- 2 * (1 - pnorm(abs(cc / ss)))

  body_lines <- c()
  for (v in seq_along(var_names)) {
    vname <- var_names[v]; vlabel <- var_labels[v]
    coef_cells <- c(); se_cells <- c()
    for (o in outcome_levels) {
      if (vname %in% colnames(cc)) {
        b <- cc[o, vname]; s <- ss[o, vname]; p <- pp[o, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells   <- c(se_cells,   paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells   <- c(se_cells,   " & ")
      }
    }
    body_lines <- c(body_lines,
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]"))
  }

  n_obs <- nrow(mod$fitted.values)
  col_spec <- paste0("l", paste(rep("c", n_out), collapse = ""))
  header <- paste0(" & ", paste(outcome_labels, collapse = " & "), " \\\\")

  c(
    "\\begin{table}[htbp]", "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\footnotesize", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule", header, "\\midrule",
    body_lines,
    "\\midrule",
    paste0("Year FE & \\multicolumn{", n_out, "}{c}{Yes} \\\\"),
    paste0("Observations & \\multicolumn{", n_out, "}{c}{", format(n_obs, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", as.numeric(logLik(mod))), "} \\\\"),
    paste0("AIC & \\multicolumn{", n_out, "}{c}{", sprintf("%.1f", AIC(mod)), "} \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_B <- build_single_table(
  mB_nosal,
  outcome_levels = c("diffka_draft", "diffka_nondraft"),
  outcome_labels = c("Diff Ka: Draft Vac.", "Diff Ka: Non-Draft"),
  caption = "Within-Kyoku Transfers: Selection into Draft-Induced Vacancies",
  label = "tab:selection-within-kyoku",
  notes = paste0(
    "Multinomial logit. Base category: stayed (same ka and kyoku). ",
    "Sample: workers who either stayed or transferred within the same kyoku (different ka). ",
    "``Draft Vac.'' = destination office had $\\geq$1 drafted worker that year. ",
    "All specifications use the salary subsample and include year FE, tenure, ",
    "and current-year log salary. ",
    "Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)
writeLines(tex_B, file.path(OUT_DIR, "MLogit_B_WithinKyoku.tex"))
cat("Exported Table B to:", file.path(OUT_DIR, "MLogit_B_WithinKyoku.tex"), "\n")

# ---- Table C: Cross-kyoku transfers ----
tex_C <- build_panel_table(
  mC_nosal, mC_full,
  outcome_levels = c("diffkyoku_draft", "diffkyoku_nondraft"),
  outcome_labels = c("Diff Kyoku: Draft", "Diff Kyoku: Non-Draft"),
  caption = "Cross-Kyoku Transfers: Selection into Draft-Induced Vacancies",
  label = "tab:selection-cross-kyoku",
  notes = paste0(
    "Multinomial logit. Base category: stayed (same ka and kyoku). ",
    "Sample restricted to workers who either stayed or transferred across kyoku. ",
    "``Draft'' = destination office had $\\geq$1 drafted worker. ",
    "All specifications use the salary subsample and include year FE, tenure, ",
    "and current-year log salary. ",
    "Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)
writeLines(tex_C, file.path(OUT_DIR, "MLogit_C_CrossKyoku.tex"))
cat("Exported Table C to:", file.path(OUT_DIR, "MLogit_C_CrossKyoku.tex"), "\n")

# ============================================================
# MODEL D: RESTRICTED TRANSFER-OUT OFFICES
# Restrict to origin office-units that transferred at least one worker
# to either (a) a different ka within the same kyoku or (b) a different
# kyoku. Report aggregate transfer choices and the within/cross
# decomposition side-by-side.
# ============================================================

cat("\n========== MODEL D: RESTRICTED TRANSFER-OUT OFFICES ==========\n\n")

transfer_out_units <- selection_panel %>%
  filter(move_type %in% c("diff_ka", "diff_kyoku")) %>%
  distinct(lag_office_id, lag_ka, lag_norm_kyoku, year_num)

dat_D <- selection_panel %>%
  filter(move_type != "exit") %>%
  semi_join(transfer_out_units,
            by = c("lag_office_id", "lag_ka", "lag_norm_kyoku", "year_num")) %>%
  mutate(
    outcome_D_agg = case_when(
      move_type == "same_ka"                              ~ "stayed",
      move_type %in% c("diff_ka", "diff_kyoku") &
        dest_has_draft_vacancy == 1                       ~ "transfer_draft",
      move_type %in% c("diff_ka", "diff_kyoku")            ~ "transfer_nondraft"
    ),
    outcome_D_agg = factor(outcome_D_agg,
                           levels = c("stayed", "transfer_draft", "transfer_nondraft")),
    outcome_D_decomp = case_when(
      move_type == "same_ka"                       ~ "stayed",
      move_type == "diff_ka" & dest_has_draft_vacancy == 1
                                                   ~ "diffka_draft",
      move_type == "diff_ka"                       ~ "diffka_nondraft",
      move_type == "diff_kyoku" & dest_has_draft_vacancy == 1
                                                   ~ "diffkyoku_draft",
      move_type == "diff_kyoku"                    ~ "diffkyoku_nondraft"
    ),
    outcome_D_decomp = factor(outcome_D_decomp,
                              levels = c("stayed",
                                         "diffka_draft", "diffka_nondraft",
                                         "diffkyoku_draft", "diffkyoku_nondraft"))
  )

cat("  Restricted office-units:", nrow(transfer_out_units), "\n")
cat("  Workers in restricted sample:", nrow(dat_D), "\n")
cat("  Aggregate draft transfers:    ", sum(dat_D$outcome_D_agg == "transfer_draft"), "\n")
cat("  Aggregate non-draft transfers:", sum(dat_D$outcome_D_agg == "transfer_nondraft"), "\n")
cat("  Diff-ka draft transfers:      ", sum(dat_D$outcome_D_decomp == "diffka_draft"), "\n")
cat("  Diff-ka non-draft transfers:  ", sum(dat_D$outcome_D_decomp == "diffka_nondraft"), "\n")
cat("  Diff-kyoku draft transfers:   ", sum(dat_D$outcome_D_decomp == "diffkyoku_draft"), "\n")
cat("  Diff-kyoku non-draft transfers:", sum(dat_D$outcome_D_decomp == "diffkyoku_nondraft"), "\n\n")

dat_D_sal <- dat_D %>% filter(has_salary == 1)
cat("  Workers in salary subsample:", nrow(dat_D_sal), "\n\n")

mD_agg <- multinom(
  outcome_D_agg ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_D_sal, trace = FALSE, maxit = 500
)

mD_decomp <- multinom(
  outcome_D_decomp ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_D_sal, trace = FALSE, maxit = 500
)

build_restricted_table <- function(mod_agg, mod_decomp, caption, label, notes) {
  outcome_specs <- list(
    list(mod = mod_agg, outcome = "transfer_draft", label = "Any: Draft Vac."),
    list(mod = mod_agg, outcome = "transfer_nondraft", label = "Any: Non-Draft"),
    list(mod = mod_decomp, outcome = "diffka_draft", label = "Diff Ka: Draft"),
    list(mod = mod_decomp, outcome = "diffka_nondraft", label = "Diff Ka: Non-Draft"),
    list(mod = mod_decomp, outcome = "diffkyoku_draft", label = "Diff Kyoku: Draft"),
    list(mod = mod_decomp, outcome = "diffkyoku_nondraft", label = "Diff Kyoku: Non-Draft")
  )

  build_row <- function(vname, vlabel) {
    coef_cells <- c()
    se_cells <- c()
    for (spec in outcome_specs) {
      cc <- coef(spec$mod)
      ss <- summary(spec$mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      if (vname %in% colnames(cc)) {
        b <- cc[spec$outcome, vname]
        s <- ss[spec$outcome, vname]
        p <- pp[spec$outcome, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells <- c(se_cells, paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells <- c(se_cells, " & ")
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v]))
  }

  n_obs <- nrow(mod_agg$fitted.values)
  col_labels <- paste(vapply(outcome_specs, `[[`, character(1), "label"), collapse = " & ")

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{0.92\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{All transfers} & \\multicolumn{2}{c}{Different ka, same kyoku} & \\multicolumn{2}{c}{Different kyoku} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    paste0(" & ", col_labels, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    "Year FE & \\multicolumn{6}{c}{Yes} \\\\",
    paste0("Observations & \\multicolumn{6}{c}{", format(n_obs, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_agg))),
           "} & \\multicolumn{4}{c}{", sprintf("%.1f", as.numeric(logLik(mod_decomp))), "} \\\\"),
    paste0("AIC & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_agg)),
           "} & \\multicolumn{4}{c}{", sprintf("%.1f", AIC(mod_decomp)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_D <- build_restricted_table(
  mD_agg, mD_decomp,
  caption = "Transfer-Out Offices: Selection into Draft-Induced Vacancies",
  label = "tab:selection-transferout-restricted",
  notes = paste0(
    "Multinomial logit. Sample restricted to origin office-unit $\\times$ year cells ",
    "that transferred at least one worker either to a different ka within the same kyoku ",
    "or to a different kyoku, further restricted to workers with observed current-year salary. ",
    "Columns 1--2 estimate aggregate transfer outcomes; ",
    "columns 3--6 estimate the decomposition into within-kyoku and cross-kyoku transfers. ",
    "Base category: stayed in the same ka and kyoku. Decoration and court rank are coded ",
    "0--8, where 0 is unobserved/no rank and higher values indicate higher status. ",
    "All specifications include year FE, tenure, and log current-year salary. ",
    "Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)

writeLines(tex_D, file.path(OUT_DIR, "MLogit_D_TransferOutRestricted.tex"))
cat("Exported Table D to:", file.path(OUT_DIR, "MLogit_D_TransferOutRestricted.tex"), "\n")


# ============================================================
# MODEL E: DRAFTED-WORKER WAGE SPLIT
# Same aggregate transfer choice, splitting draft-vacancy destinations
# by whether drafted workers in that destination office-year had above-
# or below-median salary.
# ============================================================

cat("\n========== MODEL E: DRAFTED-WORKER WAGE SPLIT ==========\n\n")

dat_E_high <- dat_D_sal %>%
  filter(
         !(move_type %in% c("diff_ka", "diff_kyoku") &
             dest_has_draft_vacancy == 1 & dest_has_high_wage_draft != 1)) %>%
  mutate(
    outcome_E_high = case_when(
      move_type == "same_ka" ~ "stayed",
      dest_has_high_wage_draft == 1 ~ "transfer_draft",
      TRUE ~ "transfer_nondraft"
    ),
    outcome_E_high = factor(outcome_E_high,
                            levels = c("stayed", "transfer_draft", "transfer_nondraft"))
)

dat_E_low <- dat_D_sal %>%
  filter(
         !(move_type %in% c("diff_ka", "diff_kyoku") &
             dest_has_draft_vacancy == 1 & dest_has_low_wage_draft != 1)) %>%
  mutate(
    outcome_E_low = case_when(
      move_type == "same_ka" ~ "stayed",
      dest_has_low_wage_draft == 1 ~ "transfer_draft",
      TRUE ~ "transfer_nondraft"
    ),
    outcome_E_low = factor(outcome_E_low,
                           levels = c("stayed", "transfer_draft", "transfer_nondraft"))
)

cat("  Workers in high-wage salary sample:", nrow(dat_E_high), "\n")
cat("  High-wage draft transfers:         ", sum(dat_E_high$outcome_E_high == "transfer_draft"), "\n")
cat("  High-wage non-draft transfers:     ", sum(dat_E_high$outcome_E_high == "transfer_nondraft"), "\n")
cat("  Workers in low-wage salary sample:", nrow(dat_E_low), "\n")
cat("  Low-wage draft transfers:         ", sum(dat_E_low$outcome_E_low == "transfer_draft"), "\n")
cat("  Low-wage non-draft transfers:     ", sum(dat_E_low$outcome_E_low == "transfer_nondraft"), "\n\n")

mE_high <- multinom(
  outcome_E_high ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_E_high, trace = FALSE, maxit = 500
)

mE_low <- multinom(
  outcome_E_low ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_E_low, trace = FALSE, maxit = 500
)

build_drafted_wage_table <- function(mod_all, mod_high, mod_low, caption, label, notes) {
  outcome_specs <- list(
    list(mod = mod_all, outcome = "transfer_draft", label = "Draft Vac."),
    list(mod = mod_all, outcome = "transfer_nondraft", label = "Non-Draft"),
    list(mod = mod_high, outcome = "transfer_draft", label = "Draft Vac."),
    list(mod = mod_high, outcome = "transfer_nondraft", label = "Non-Draft"),
    list(mod = mod_low, outcome = "transfer_draft", label = "Draft Vac."),
    list(mod = mod_low, outcome = "transfer_nondraft", label = "Non-Draft")
  )

  build_row <- function(vname, vlabel) {
    coef_cells <- c()
    se_cells <- c()
    for (spec in outcome_specs) {
      cc <- coef(spec$mod)
      ss <- summary(spec$mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      if (vname %in% colnames(cc)) {
        b <- cc[spec$outcome, vname]
        s <- ss[spec$outcome, vname]
        p <- pp[spec$outcome, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells <- c(se_cells, paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells <- c(se_cells, " & ")
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v]))
  }

  n_all <- nrow(mod_all$fitted.values)
  n_high <- nrow(mod_high$fitted.values)
  n_low <- nrow(mod_low$fitted.values)
  col_labels <- paste(vapply(outcome_specs, `[[`, character(1), "label"), collapse = " & ")

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{0.96\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{All draft vacancies} & \\multicolumn{2}{c}{High-wage drafted workers} & \\multicolumn{2}{c}{Low-wage drafted workers} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    paste0(" & ", col_labels, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    "Year FE & \\multicolumn{6}{c}{Yes} \\\\",
    paste0("Observations & \\multicolumn{2}{c}{", format(n_all, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_high, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_low, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_all))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_high))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_low))), "} \\\\"),
    paste0("AIC & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_all)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_high)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_low)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_E <- build_drafted_wage_table(
  mD_agg, mE_high, mE_low,
  caption = "Selection into Draft-Induced Vacancies by Drafted-Worker Salary",
  label = "tab:selection-drafted-worker-wage",
  notes = paste0(
    "Multinomial logit. Sample restricted to origin office-unit $\\times$ year cells ",
    "that transferred at least one worker either to a different ka within the same kyoku ",
    "or to a different kyoku, and to workers with observed current-year salary. ",
    "Columns 1--2 pool all draft-vacancy destinations. Columns 3--4 restrict the ",
    "draft-vacancy destination to office-years where the mean salary of drafted workers ",
    "is at or above the median among draft-vacancy office-years with observed drafted-worker salary; ",
    "columns 5--6 use the below-median group. In columns 3--6, transfers to draft-vacancy ",
    "office-years outside the indicated wage group are excluded. Base category: stayed in ",
    "the same ka and kyoku. Decoration and court rank are coded 0--8, where 0 is unobserved/no ",
    "rank and higher values indicate higher status. All specifications include year FE, tenure, ",
    "and log current-year salary. Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)

writeLines(tex_E, file.path(OUT_DIR, "MLogit_E_DraftedWorkerWage.tex"))
cat("Exported Table E to:", file.path(OUT_DIR, "MLogit_E_DraftedWorkerWage.tex"), "\n")

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.7in,landscape]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{graphicx}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{MLogit_E_DraftedWorkerWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_drafted_worker_wage.tex"))


# ============================================================
# MODEL F: DRAFTED-WORKER WAGE SPLIT WITH EXPANDED RISK SETS
# Candidate pools are expanded to (i) origin ka-within-kyoku x year
# and (ii) origin kyoku x year. This keeps the high/low wage
# destination split while restoring the denominator of unchosen workers.
# ============================================================

cat("\n========== MODEL F: DRAFTED-WORKER WAGE SPLIT, EXPANDED RISK SETS ==========\n\n")

make_wage_risk_data <- function(group_vars, outcome_name) {
  join_vars <- c(group_vars, "year_num")

  eligible_groups <- selection_panel %>%
    filter(move_type %in% c("diff_ka", "diff_kyoku"),
           dest_draft_wage_group %in% c("high", "low")) %>%
    distinct(across(all_of(join_vars)))

  selection_panel %>%
    semi_join(eligible_groups, by = join_vars) %>%
    filter(move_type %in% c("same_ka", "diff_ka", "diff_kyoku"),
           has_salary == 1,
           !(move_type %in% c("diff_ka", "diff_kyoku") &
               dest_has_draft_vacancy == 1 &
               !dest_draft_wage_group %in% c("high", "low"))) %>%
    mutate(
      outcome_tmp = case_when(
        move_type == "same_ka" ~ "not_selected",
        dest_draft_wage_group == "high" ~ "high_draft",
        dest_draft_wage_group == "low" ~ "low_draft",
        move_type %in% c("diff_ka", "diff_kyoku") ~ "nondraft_transfer"
      ),
      outcome_tmp = factor(outcome_tmp,
                           levels = c("not_selected", "high_draft",
                                      "low_draft", "nondraft_transfer"))
    ) %>%
    rename(!!outcome_name := outcome_tmp)
}

dat_F_ka <- make_wage_risk_data(
  c("lag_norm_kyoku", "lag_ka"),
  "outcome_F_ka"
)

dat_F_kyoku <- make_wage_risk_data(
  c("lag_norm_kyoku"),
  "outcome_F_kyoku"
)

cat("  Ka-year salary sample:", nrow(dat_F_ka), "\n")
cat("  Ka-year high-wage draft transfers:", sum(dat_F_ka$outcome_F_ka == "high_draft"), "\n")
cat("  Ka-year low-wage draft transfers: ", sum(dat_F_ka$outcome_F_ka == "low_draft"), "\n")
cat("  Ka-year non-draft transfers:      ", sum(dat_F_ka$outcome_F_ka == "nondraft_transfer"), "\n")
cat("  Kyoku-year salary sample:", nrow(dat_F_kyoku), "\n")
cat("  Kyoku-year high-wage draft transfers:", sum(dat_F_kyoku$outcome_F_kyoku == "high_draft"), "\n")
cat("  Kyoku-year low-wage draft transfers: ", sum(dat_F_kyoku$outcome_F_kyoku == "low_draft"), "\n")
cat("  Kyoku-year non-draft transfers:      ", sum(dat_F_kyoku$outcome_F_kyoku == "nondraft_transfer"), "\n\n")

mF_ka <- multinom(
  outcome_F_ka ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_F_ka, trace = FALSE, maxit = 500
)

mF_kyoku <- multinom(
  outcome_F_kyoku ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_F_kyoku, trace = FALSE, maxit = 500
)

build_wage_riskset_table <- function(mod_ka, mod_kyoku, caption, label, notes) {
  outcome_specs <- list(
    list(mod = mod_ka, outcome = "high_draft", label = "High Draft"),
    list(mod = mod_ka, outcome = "low_draft", label = "Low Draft"),
    list(mod = mod_ka, outcome = "nondraft_transfer", label = "Non-Draft"),
    list(mod = mod_kyoku, outcome = "high_draft", label = "High Draft"),
    list(mod = mod_kyoku, outcome = "low_draft", label = "Low Draft"),
    list(mod = mod_kyoku, outcome = "nondraft_transfer", label = "Non-Draft")
  )

  build_row <- function(vname, vlabel) {
    coef_cells <- c()
    se_cells <- c()
    for (spec in outcome_specs) {
      cc <- coef(spec$mod)
      ss <- summary(spec$mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      if (vname %in% colnames(cc)) {
        b <- cc[spec$outcome, vname]
        s <- ss[spec$outcome, vname]
        p <- pp[spec$outcome, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells <- c(se_cells, paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells <- c(se_cells, " & ")
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v]))
  }

  n_ka <- nrow(mod_ka$fitted.values)
  n_kyoku <- nrow(mod_kyoku$fitted.values)
  col_labels <- paste(vapply(outcome_specs, `[[`, character(1), "label"), collapse = " & ")

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{0.96\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{3}{c}{Origin ka $\\times$ year risk set} & \\multicolumn{3}{c}{Origin kyoku $\\times$ year risk set} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    paste0(" & ", col_labels, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    "Year FE & \\multicolumn{6}{c}{Yes} \\\\",
    paste0("Observations & \\multicolumn{3}{c}{", format(n_ka, big.mark = ","),
           "} & \\multicolumn{3}{c}{", format(n_kyoku, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{3}{c}{", sprintf("%.1f", as.numeric(logLik(mod_ka))),
           "} & \\multicolumn{3}{c}{", sprintf("%.1f", as.numeric(logLik(mod_kyoku))), "} \\\\"),
    paste0("AIC & \\multicolumn{3}{c}{", sprintf("%.1f", AIC(mod_ka)),
           "} & \\multicolumn{3}{c}{", sprintf("%.1f", AIC(mod_kyoku)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_F <- build_wage_riskset_table(
  mF_ka, mF_kyoku,
  caption = "Selection into Draft-Induced Vacancies by Drafted-Worker Salary: Expanded Risk Sets",
  label = "tab:selection-drafted-worker-wage-risksets",
  notes = paste0(
    "Multinomial logit. Columns 1--3 define candidate workers within origin ka-within-kyoku $\\times$ year cells; ",
    "columns 4--6 define candidate workers within origin kyoku $\\times$ year cells. ",
    "Risk-set cells are included if at least one worker transferred to a draft-vacancy office-year ",
    "with observed drafted-worker salary. High and low draft vacancies split destination office-years ",
    "by whether the mean monthly salary of drafted workers is at or above the median among draft-vacancy ",
    "office-years with observed drafted-worker salary. Base category: not selected/stayed in the origin ",
    "unit. Transfers to draft-vacancy office-years with missing drafted-worker salary are excluded. ",
    "Decoration and court rank are coded 0--8, where 0 is unobserved/no rank and higher values indicate ",
    "higher status. All specifications include year FE, tenure, and log current-year salary. ",
    "Standard errors in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)

writeLines(tex_F, file.path(OUT_DIR, "MLogit_F_DraftedWorkerWage_RiskSets.tex"))
cat("Exported Table F to:", file.path(OUT_DIR, "MLogit_F_DraftedWorkerWage_RiskSets.tex"), "\n")

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.7in,landscape]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{graphicx}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{MLogit_F_DraftedWorkerWage_RiskSets.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_drafted_worker_wage_risksets.tex"))


# ============================================================
# MODEL G: DESTINATION DRAFTEE WAGE OUTCOMES, BROAD SAMPLE
# Keep the broad transfer-out office-unit risk set. Split the
# draft-vacancy transfer outcome by the wage of the drafted worker(s)
# at the destination office-year.
# ============================================================

cat("\n========== MODEL G: DESTINATION DRAFTEE WAGE OUTCOMES ==========\n\n")

dat_G <- dat_D_sal %>%
  filter(!(move_type %in% c("diff_ka", "diff_kyoku") &
             dest_has_draft_vacancy == 1 &
             !dest_draft_wage_group %in% c("high", "low"))) %>%
  mutate(
    outcome_G = case_when(
      move_type == "same_ka" ~ "not_selected",
      dest_draft_wage_group == "high" ~ "high_draft",
      dest_draft_wage_group == "low" ~ "low_draft",
      move_type %in% c("diff_ka", "diff_kyoku") ~ "nondraft_transfer"
    ),
    outcome_G = factor(outcome_G,
                       levels = c("not_selected", "high_draft",
                                  "low_draft", "nondraft_transfer")),
    outcome_G_agg = case_when(
      outcome_G == "not_selected" ~ "not_selected",
      outcome_G %in% c("high_draft", "low_draft") ~ "draft",
      outcome_G == "nondraft_transfer" ~ "nondraft"
    ),
    outcome_G_high = case_when(
      outcome_G == "not_selected" ~ "not_selected",
      outcome_G == "high_draft" ~ "draft",
      outcome_G == "nondraft_transfer" ~ "nondraft",
      TRUE ~ NA_character_
    ),
    outcome_G_low = case_when(
      outcome_G == "not_selected" ~ "not_selected",
      outcome_G == "low_draft" ~ "draft",
      outcome_G == "nondraft_transfer" ~ "nondraft",
      TRUE ~ NA_character_
    ),
    outcome_G_agg = factor(outcome_G_agg, levels = c("not_selected", "draft", "nondraft")),
    outcome_G_high = factor(outcome_G_high, levels = c("not_selected", "draft", "nondraft")),
    outcome_G_low = factor(outcome_G_low, levels = c("not_selected", "draft", "nondraft"))
  )

cat("  Broad destination-wage sample:", nrow(dat_G), "\n")
cat("  Not selected/stayed:          ", sum(dat_G$outcome_G == "not_selected"), "\n")
cat("  High-wage draft transfers:    ", sum(dat_G$outcome_G == "high_draft"), "\n")
cat("  Low-wage draft transfers:     ", sum(dat_G$outcome_G == "low_draft"), "\n")
cat("  Non-draft transfers:          ", sum(dat_G$outcome_G == "nondraft_transfer"), "\n\n")

dat_G_agg <- dat_G %>% filter(!is.na(outcome_G_agg))
dat_G_high <- dat_G %>% filter(!is.na(outcome_G_high))
dat_G_low <- dat_G %>% filter(!is.na(outcome_G_low))

mG_agg <- multinom(
  outcome_G_agg ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_G_agg, trace = FALSE, maxit = 500
)

mG_high <- multinom(
  outcome_G_high ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_G_high, trace = FALSE, maxit = 500
)

mG_low <- multinom(
  outcome_G_low ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary + yr_fac,
  data = dat_G_low, trace = FALSE, maxit = 500
)

build_destination_wage_table <- function(mod_agg, mod_high, mod_low, caption, label, notes) {
  outcome_specs <- list(
    list(mod = mod_agg, outcome = "draft", label = "Draft"),
    list(mod = mod_agg, outcome = "nondraft", label = "Non-Draft"),
    list(mod = mod_high, outcome = "draft", label = "Draft"),
    list(mod = mod_high, outcome = "nondraft", label = "Non-Draft"),
    list(mod = mod_low, outcome = "draft", label = "Draft"),
    list(mod = mod_low, outcome = "nondraft", label = "Non-Draft")
  )

  build_row <- function(vname, vlabel) {
    coef_cells <- c()
    se_cells <- c()
    for (spec in outcome_specs) {
      cc <- coef(spec$mod)
      ss <- summary(spec$mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      if (vname %in% colnames(cc)) {
        b <- cc[spec$outcome, vname]
        s <- ss[spec$outcome, vname]
        p <- pp[spec$outcome, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells <- c(se_cells, paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells <- c(se_cells, " & ")
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v]))
  }

  n_agg <- nrow(mod_agg$fitted.values)
  n_high <- nrow(mod_high$fitted.values)
  n_low <- nrow(mod_low$fitted.values)
  col_labels <- paste(vapply(outcome_specs, `[[`, character(1), "label"), collapse = " & ")

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{0.96\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{All destinations} & \\multicolumn{2}{c}{High-wage draftee destination} & \\multicolumn{2}{c}{Low-wage draftee destination} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    paste0(" & ", col_labels, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    "Year FE & \\multicolumn{6}{c}{Yes} \\\\",
    paste0("Observations & \\multicolumn{2}{c}{", format(n_agg, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_high, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_low, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_agg))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_high))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_low))), "} \\\\"),
    paste0("AIC & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_agg)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_high)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_low)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_G <- build_destination_wage_table(
  mG_agg, mG_high, mG_low,
  caption = "Selection into Draft-Induced Vacancies by Destination Draftee Salary",
  label = "tab:selection-destination-draftee-wage",
  notes = paste0(
    "Multinomial logit. Sample uses the broad restricted transfer-out office-unit $\\times$ year ",
    "risk set and workers with observed current-year salary. Columns 1--2 split transfers into ",
    "drafted and non-drafted destination office-years. Columns 3--4 keep drafted destinations ",
    "whose drafted workers have at-or-above-median mean salary, plus non-drafted destinations. ",
    "Columns 5--6 analogously use below-median drafted-worker salary destinations plus non-drafted destinations. ",
    "Transfers to draft-vacancy office-years with missing drafted-worker salary are excluded. ",
    "Base category: not selected/stayed in the same ka and kyoku. Decoration and court rank are ",
    "coded 0--8, where 0 is unobserved/no rank and higher values indicate higher status. ",
    "All specifications include year FE, tenure, and log current-year salary. Standard errors ",
    "in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)

writeLines(tex_G, file.path(OUT_DIR, "MLogit_G_DestinationDrafteeWage.tex"))
cat("Exported Table G to:", file.path(OUT_DIR, "MLogit_G_DestinationDrafteeWage.tex"), "\n")

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.8in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{graphicx}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{MLogit_G_DestinationDrafteeWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_destination_draftee_wage.tex"))


# ============================================================
# MODEL H: 1944 DESTINATION DRAFTEE WAGE, BASELINE KA-OCC
# Choice sets are defined from 1943 Tokyo Shi and 1941 Tokyo Fu
# workers at the ka x occupation level. Outcomes and wages are
# measured from each worker's 1944 assignment.
# ============================================================

cat("\n========== MODEL H: 1944 DESTINATION DRAFTEE WAGE, KA-OCC CHOICE SETS ==========\n\n")

baseline_choice <- df %>%
  filter((year_num == 1943 & gov_level == "TokyoShi") |
           (year_num == 1941 & gov_level == "TokyoFu"),
         is_name == TRUE,
         !is.na(ka),
         !is.na(norm_kyoku)) %>%
  mutate(
    baseline_year = year_num,
    baseline_occ = classify_occ(pos_norm),
    choice_set_id = paste(gov_level, norm_kyoku, ka, baseline_occ, sep = "||")
  ) %>%
  arrange(staff_id, desc(baseline_year)) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id,
         baseline_year,
         baseline_gov_level = gov_level,
         baseline_office_id = office_id,
         baseline_norm_kyoku = norm_kyoku,
         baseline_ka = ka,
         baseline_occ,
         choice_set_id,
         baseline_pos_norm = pos_norm,
         baseline_pos_rank = pos_rank,
         baseline_court_rank = court_rank,
         baseline_decor_rank = decor_rank)

assignment_1944 <- df %>%
  filter(year_num == 1944, is_name == TRUE) %>%
  select(staff_id,
         office_id_1944 = office_id,
         ka_1944 = ka,
         norm_kyoku_1944 = norm_kyoku,
         kakari_1944 = kakari,
         female_1944 = is_female,
         salary_1944 = salary_num)

dat_H_base <- baseline_choice %>%
  inner_join(assignment_1944, by = "staff_id") %>%
  left_join(office_drafts %>% filter(year_num == 1944),
            by = c("office_id_1944" = "office_id")) %>%
  mutate(
    dest_has_draft_vacancy = replace_na(dest_has_draft_vacancy, 0L),
    dest_has_high_wage_draft = replace_na(dest_has_high_wage_draft, 0L),
    dest_has_low_wage_draft = replace_na(dest_has_low_wage_draft, 0L),
    move_type_1944 = case_when(
      !is.na(ka_1944) & baseline_ka == ka_1944 ~ "not_selected",
      !is.na(ka_1944) & baseline_ka != ka_1944 ~ "selected",
      is.na(ka_1944) & !is.na(office_id_1944) & baseline_office_id == office_id_1944 ~ "not_selected",
      is.na(ka_1944) & !is.na(office_id_1944) & baseline_office_id != office_id_1944 ~ "selected",
      TRUE ~ "exit"
    ),
    moved_out_1944 = move_type_1944 == "selected",
    log_salary = log(salary_1944 + 1),
    has_salary = as.integer(!is.na(salary_1944) & salary_1944 > 0),
    lag_decor_rank_ext = coalesce(as.numeric(baseline_decor_rank), 0),
    lag_court_rank_ext = coalesce(as.numeric(baseline_court_rank), 0),
    female = as.integer(coalesce(female_1944, FALSE)),
    yr_fac = factor(1944)
  ) %>%
  left_join(tenure_df, by = "staff_id") %>%
  mutate(own_tenure = 1944 - first_yr) %>%
  filter(has_salary == 1)

eligible_H_sets <- dat_H_base %>%
  distinct(choice_set_id)

dat_H <- dat_H_base %>%
  semi_join(eligible_H_sets, by = "choice_set_id") %>%
  filter(move_type_1944 %in% c("not_selected", "selected")) %>%
  mutate(
    outcome_H_agg = case_when(
      move_type_1944 == "not_selected" ~ "not_selected",
      dest_has_draft_vacancy == 1 ~ "draft",
      moved_out_1944 ~ "nondraft"
    ),
    outcome_H_high = case_when(
      move_type_1944 == "not_selected" ~ "not_selected",
      dest_draft_wage_group == "high" ~ "draft",
      dest_has_draft_vacancy == 0 & moved_out_1944 ~ "nondraft",
      TRUE ~ NA_character_
    ),
    outcome_H_low = case_when(
      move_type_1944 == "not_selected" ~ "not_selected",
      dest_draft_wage_group == "low" ~ "draft",
      dest_has_draft_vacancy == 0 & moved_out_1944 ~ "nondraft",
      TRUE ~ NA_character_
    ),
    outcome_H_agg = factor(outcome_H_agg, levels = c("not_selected", "draft", "nondraft")),
    outcome_H_high = factor(outcome_H_high, levels = c("not_selected", "draft", "nondraft")),
    outcome_H_low = factor(outcome_H_low, levels = c("not_selected", "draft", "nondraft"))
  )

dat_H_agg <- dat_H %>% filter(!is.na(outcome_H_agg))
dat_H_high <- dat_H %>% filter(!is.na(outcome_H_high))
dat_H_low <- dat_H %>% filter(!is.na(outcome_H_low))

cat("  Baseline workers with 1944 wage:", nrow(dat_H_base), "\n")
cat("  Baseline ka-occ choice sets:", nrow(eligible_H_sets), "\n")
cat("  Aggregate sample:", nrow(dat_H_agg), "\n")
cat("    Draft destinations:", sum(dat_H_agg$outcome_H_agg == "draft"), "\n")
cat("    Non-draft destinations:", sum(dat_H_agg$outcome_H_agg == "nondraft"), "\n")
cat("    Not selected:", sum(dat_H_agg$outcome_H_agg == "not_selected"), "\n")
cat("  High-wage sample:", nrow(dat_H_high), "\n")
cat("    Draft destinations:", sum(dat_H_high$outcome_H_high == "draft"), "\n")
cat("    Non-draft destinations:", sum(dat_H_high$outcome_H_high == "nondraft"), "\n")
cat("  Low-wage sample:", nrow(dat_H_low), "\n")
cat("    Draft destinations:", sum(dat_H_low$outcome_H_low == "draft"), "\n")
cat("    Non-draft destinations:", sum(dat_H_low$outcome_H_low == "nondraft"), "\n\n")

mH_agg <- multinom(
  outcome_H_agg ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary,
  data = dat_H_agg, trace = FALSE, maxit = 500
)

mH_high <- multinom(
  outcome_H_high ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary,
  data = dat_H_high, trace = FALSE, maxit = 500
)

mH_low <- multinom(
  outcome_H_low ~ lag_decor_rank_ext + lag_court_rank_ext + female +
    own_tenure + log_salary,
  data = dat_H_low, trace = FALSE, maxit = 500
)

build_baseline_choice_table <- function(mod_agg, mod_high, mod_low, caption, label, notes) {
  outcome_specs <- list(
    list(mod = mod_agg, outcome = "draft", label = "Draft"),
    list(mod = mod_agg, outcome = "nondraft", label = "Non-Draft"),
    list(mod = mod_high, outcome = "draft", label = "Draft"),
    list(mod = mod_high, outcome = "nondraft", label = "Non-Draft"),
    list(mod = mod_low, outcome = "draft", label = "Draft"),
    list(mod = mod_low, outcome = "nondraft", label = "Non-Draft")
  )

  build_row <- function(vname, vlabel) {
    coef_cells <- c()
    se_cells <- c()
    for (spec in outcome_specs) {
      cc <- coef(spec$mod)
      ss <- summary(spec$mod)$standard.errors
      pp <- 2 * (1 - pnorm(abs(cc / ss)))
      if (vname %in% colnames(cc)) {
        b <- cc[spec$outcome, vname]
        s <- ss[spec$outcome, vname]
        p <- pp[spec$outcome, vname]
        stars <- format_stars(p)
        coef_cells <- c(coef_cells, paste0(" & ", sprintf("%.4f", b), "$", stars, "$"))
        se_cells <- c(se_cells, paste0(" & (", sprintf("%.4f", s), ")"))
      } else {
        coef_cells <- c(coef_cells, " & ")
        se_cells <- c(se_cells, " & ")
      }
    }
    c(
      paste0(vlabel, paste(coef_cells, collapse = ""), " \\\\"),
      paste0(paste(rep(" ", nchar(vlabel)), collapse = ""),
             paste(se_cells, collapse = ""), " \\\\[3pt]")
    )
  }

  body_lines <- c()
  for (v in seq_along(var_names)) {
    body_lines <- c(body_lines, build_row(var_names[v], var_labels[v]))
  }

  n_agg <- nrow(mod_agg$fitted.values)
  n_high <- nrow(mod_high$fitted.values)
  n_low <- nrow(mod_low$fitted.values)
  col_labels <- paste(vapply(outcome_specs, `[[`, character(1), "label"), collapse = " & ")

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\scriptsize",
    "\\begin{threeparttable}",
    "\\resizebox{0.96\\textwidth}{!}{%",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{All destinations} & \\multicolumn{2}{c}{High-wage draftee destination} & \\multicolumn{2}{c}{Low-wage draftee destination} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    paste0(" & ", col_labels, " \\\\"),
    "\\midrule",
    body_lines,
    "\\midrule",
    paste0("Observations & \\multicolumn{2}{c}{", format(n_agg, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_high, big.mark = ","),
           "} & \\multicolumn{2}{c}{", format(n_low, big.mark = ","), "} \\\\"),
    paste0("Log-likelihood & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_agg))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_high))),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", as.numeric(logLik(mod_low))), "} \\\\"),
    paste0("AIC & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_agg)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_high)),
           "} & \\multicolumn{2}{c}{", sprintf("%.1f", AIC(mod_low)), "} \\\\"),
    "\\bottomrule",
    "\\end{tabular}}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item \\textit{Notes:} ", notes),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
}

tex_H <- build_baseline_choice_table(
  mH_agg, mH_high, mH_low,
  caption = "Selection into 1944 Destinations by Destination Draftee Salary",
  label = "tab:selection-1944-ka-occ-destination-wage",
  notes = paste0(
    "Multinomial logit. Choice sets are defined by all workers' baseline ka $\\times$ occupation cells ",
    "from 1943 Tokyo Shi and 1941 Tokyo Fu rosters. Outcomes and salaries are measured from the worker's ",
    "1944 assignment; Tokyo Fu baseline workers therefore use their 1944 salary. A worker is selected ",
    "if assigned to a different 1944 ka; when 1944 ka is missing, office assignment is used instead. Columns 1--2 split ",
    "1944 transfers into drafted and non-drafted destination office-years. Columns 3--4 keep drafted ",
    "destinations whose drafted workers have at-or-above-median mean salary, plus non-drafted destinations. ",
    "Columns 5--6 analogously use below-median drafted-worker salary destinations plus non-drafted destinations. ",
    "Base category: not selected from the baseline ka-occupation choice set. Decoration and court rank ",
    "are baseline values coded 0--8, where 0 is unobserved/no rank and higher values indicate higher status. ",
    "All specifications include tenure and log 1944 salary. Standard errors in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  )
)

writeLines(tex_H, file.path(OUT_DIR, "MLogit_H_1944KaOcc_DestinationWage.tex"))
cat("Exported Table H to:", file.path(OUT_DIR, "MLogit_H_1944KaOcc_DestinationWage.tex"), "\n")

writeLines(c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.7in,landscape]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{threeparttable}",
  "\\usepackage{graphicx}",
  "\\usepackage{caption}",
  "\\begin{document}",
  "\\input{MLogit_H_1944KaOcc_DestinationWage.tex}",
  "\\end{document}"
), file.path(OUT_DIR, "compile_1944_ka_occ_destination_wage.tex"))


# ============================================================
# Export coefficient CSV for plotting
# ============================================================

extract_coefs <- function(mod, model_name, outcome_levels) {
  if (is.null(mod)) return(tibble())
  cc <- coef(mod)
  ss <- summary(mod)$standard.errors
  pp <- 2 * (1 - pnorm(abs(cc / ss)))

  bind_rows(
    lapply(outcome_levels, function(o) {
      tibble(
        model = model_name,
        outcome = o,
        variable = colnames(cc),
        coef = cc[o, ],
        se = ss[o, ],
        pval = pp[o, ]
      )
    })
  )
}

all_coefs <- bind_rows(
  extract_coefs(mA_nosal, "A_Retention_NoSal",     c("left_draft", "left_nondraft")),
  extract_coefs(mA_full,  "A_Retention_Sal",        c("left_draft", "left_nondraft")),
  extract_coefs(mB_nosal, "B_WithinKyoku_NoSal",    c("diffka_draft", "diffka_nondraft")),
  if (!is.null(mB_full)) extract_coefs(mB_full, "B_WithinKyoku_Sal", c("diffka_draft", "diffka_nondraft")),
  extract_coefs(mC_nosal, "C_CrossKyoku_NoSal",     c("diffkyoku_draft", "diffkyoku_nondraft")),
  extract_coefs(mC_full,  "C_CrossKyoku_Sal",       c("diffkyoku_draft", "diffkyoku_nondraft"))
) %>%
  filter(!str_detect(variable, "Intercept|yr_fac"))

write_csv(all_coefs, file.path(OUT_DIR, "mlogit_coefficients.csv"))
cat("Exported coefficients CSV to:", file.path(OUT_DIR, "mlogit_coefficients.csv"), "\n")


# ============================================================
# Marginal effects at means (Model A with salary as example)
# ============================================================

cat("\n========== MARGINAL EFFECTS AT MEANS (Model A, with salary) ==========\n\n")

compute_marginal_effects <- function(mod, var, data, outcome_names) {
  probs <- predict(mod, type = "probs")
  if (is.vector(probs)) probs <- matrix(probs, ncol = length(outcome_names))
  mean_probs <- colMeans(probs)
  cat("Mean predicted probabilities:", round(mean_probs, 4), "\n")

  eps <- 0.5
  d_up <- data; d_up[[var]] <- d_up[[var]] + eps
  d_dn <- data; d_dn[[var]] <- d_dn[[var]] - eps
  p_up <- predict(mod, newdata = d_up, type = "probs")
  p_dn <- predict(mod, newdata = d_dn, type = "probs")
  if (is.vector(p_up)) p_up <- matrix(p_up, ncol = length(outcome_names))
  if (is.vector(p_dn)) p_dn <- matrix(p_dn, ncol = length(outcome_names))
  me <- colMeans((p_up - p_dn) / (2 * eps))
  names(me) <- outcome_names
  me
}

me_vars <- c("lag_decor_rank_ext", "lag_court_rank_ext", "female", "own_tenure", "log_salary")
outcome_A_names <- c("stayed", "left_draft", "left_nondraft")

me_list <- list()
for (v in me_vars) {
  cat(paste0("ME of ", v, ":\n"))
  me_list[[v]] <- compute_marginal_effects(mA_full, v, dat_A_sal, outcome_A_names)
  print(round(me_list[[v]], 6))
  cat("\n")
}

me_df <- bind_rows(lapply(me_vars, function(v) {
  tibble(model = "A_Retention_Sal", variable = v,
         me_stayed = me_list[[v]]["stayed"],
         me_left_draft = me_list[[v]]["left_draft"],
         me_left_nondraft = me_list[[v]]["left_nondraft"])
}))

write_csv(me_df, file.path(OUT_DIR, "mlogit_marginal_effects.csv"))
cat("Exported marginal effects to:", file.path(OUT_DIR, "mlogit_marginal_effects.csv"), "\n")

cat("\n========== DONE ==========\n")
