################################################################################
# Bonus/Promotion Offers, Retention, Productivity, and Draft Vacancies
#
# Outputs:
#   MainResults/BonusPromotion/results/BonusPromotion_Results.{tex,pdf}
#   MainResults/BonusPromotion/results/BonusPromotion_*.tex
#   MainResults/BonusPromotion/results/bonuspromotion_coefficients.csv
#   MainResults/BonusPromotion/descriptions/BonusPromotion_Description.txt
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
result_dir <- file.path(root_dir, "MainResults", "BonusPromotion", "results")
description_dir <- file.path(root_dir, "MainResults", "BonusPromotion", "descriptions")

dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(description_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

years_of_interest <- 1938:1945

assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^主事$|^技師$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^嘱託$")   ~ 1L,
    yr < 1948 & str_detect(pos, "^ä¸»äº‹$|^æŠ€å¸«$") ~ 3L,
    yr < 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$")   ~ 1L,
    yr < 1948                                      ~ 2L,
    yr >= 1948 & str_detect(pos, "係長")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^雇$|^嘱託$")   ~ 1L,
    yr >= 1948 & str_detect(pos, "ä¿‚é•·")           ~ 3L,
    yr >= 1948 & str_detect(pos, "^é›‡$|^å›‘è¨—$")   ~ 1L,
    yr >= 1948                                      ~ 2L
  )
}

parse_court_rank <- function(r) {
  case_when(
    is.na(r) ~ NA_integer_,
    str_detect(r, "^正一") ~ 1L,
    str_detect(r, "^正二") ~ 2L,
    str_detect(r, "^正三") ~ 3L,
    str_detect(r, "^正四") ~ 4L,
    str_detect(r, "^正五") ~ 5L,
    str_detect(r, "^正六") ~ 6L,
    str_detect(r, "^正七") ~ 7L,
    str_detect(r, "^正八") ~ 8L,
    str_detect(r, "^正九") ~ 9L,
    str_detect(r, "^正十") ~ 10L,
    str_detect(r, "^æ­£ä¸€") ~ 1L,
    str_detect(r, "^æ­£äºŒ") ~ 2L,
    str_detect(r, "^æ­£ä¸‰") ~ 3L,
    str_detect(r, "^æ­£å››") ~ 4L,
    str_detect(r, "^æ­£äº”") ~ 5L,
    str_detect(r, "^æ­£å…­") ~ 6L,
    str_detect(r, "^æ­£ä¸ƒ") ~ 7L,
    str_detect(r, "^æ­£å…«") ~ 8L,
    str_detect(r, "^æ­£å") ~ 10L,
    TRUE ~ NA_integer_
  )
}

parse_decoration <- function(d) {
  case_when(
    is.na(d) ~ NA_integer_,
    str_detect(d, "^勳一|^功一") ~ 1L,
    str_detect(d, "^勳二|^功二") ~ 2L,
    str_detect(d, "^勳三|^功三") ~ 3L,
    str_detect(d, "^勳四|^功四") ~ 4L,
    str_detect(d, "^勳五|^功五") ~ 5L,
    str_detect(d, "^勳六|^功六") ~ 6L,
    str_detect(d, "^勳七|^功七") ~ 7L,
    str_detect(d, "^勳八|^功八") ~ 8L,
    str_detect(d, "^勳九|^功九") ~ 9L,
    str_detect(d, "^勳十|^功十") ~ 10L,
    str_detect(d, "^å‹³ä¸€") ~ 1L,
    str_detect(d, "^å‹³äºŒ") ~ 2L,
    str_detect(d, "^å‹³ä¸‰") ~ 3L,
    str_detect(d, "^å‹³å››") ~ 4L,
    str_detect(d, "^å‹³äº”") ~ 5L,
    str_detect(d, "^å‹³å…­") ~ 6L,
    str_detect(d, "^å‹³ä¸ƒ") ~ 7L,
    str_detect(d, "^å‹³å…«") ~ 8L,
    str_detect(d, "^å‹³å") ~ 10L,
    TRUE ~ NA_integer_
  )
}

rank_to_prestige_score <- function(x) {
  if_else(is.na(x), 0, pmax(0, 9 - as.numeric(x)))
}

parse_salary <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c("〇" = "0", "○" = "0", "一" = "1", "二" = "2", "三" = "3",
             "四" = "4", "五" = "5", "六" = "6", "七" = "7",
             "八" = "8", "九" = "9",
             "ã€‡" = "0", "ä¸€" = "1", "äºŒ" = "2", "ä¸‰" = "3",
             "å››" = "4", "äº”" = "5", "å…­" = "6", "ä¸ƒ" = "7",
             "å…«" = "8", "ä¹" = "9")
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }

  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    is_annual <- str_detect(x, "^年|^å¹´")
    cleaned <- str_remove(x, "^[月年æœˆå¹´]")
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

cat("Loading master data from:", DATA_PATH, "\n")
df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(coalesce(position, ""), "\\s+", ""),
    pos_rank = assign_rank(pos_norm, year_num),
    court_rank = parse_court_rank(rank),
    decor_rank = parse_decoration(decoration),
    salary_num = parse_salary(salary)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df <- df_raw %>% filter(is_name == TRUE)

drafted_ids <- df_raw %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

office_drafts <- df_raw %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_drafted = n(),
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_kyoku = kyoku,
    lag_ka = ka,
    lag_kakari = kakari,
    lag_pos_norm = pos_norm,
    lag_pos_rank = pos_rank,
    lag_salary_num = salary_num,
    lag_court_rank = court_rank,
    lag_decor_rank = decor_rank
  ) %>%
  mutate(year_num = year_num + 1)

worker_year <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  filter(!is.na(lag_office_id), !(staff_id %in% drafted_ids)) %>%
  left_join(office_drafts, by = c("lag_office_id" = "office_id", "year_num")) %>%
  mutate(
    n_drafted = replace_na(n_drafted, 0L),
    n_drafted_male = replace_na(n_drafted_male, 0L),
    lag_has_draft = as.integer(n_drafted_male > 0),
    retained = 1L,
    promoted = as.integer(!is.na(pos_rank) & !is.na(lag_pos_rank) & pos_rank > lag_pos_rank),
    salary_raise = case_when(
      is.na(salary_num) | is.na(lag_salary_num) ~ NA_integer_,
      salary_num > lag_salary_num ~ 1L,
      TRUE ~ 0L
    ),
    lag_salary_by_rank = if_else(!is.na(lag_salary_num) & lag_salary_num > 0, lag_salary_num, NA_real_),
    lag_salary_z = as.numeric(scale(lag_salary_by_rank)),
    productivity_score = rowSums(
      cbind(
        rank_to_prestige_score(lag_decor_rank),
        rank_to_prestige_score(lag_court_rank),
        replace_na(lag_salary_z, 0)
      ),
      na.rm = TRUE
    ),
    productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
    tenure = year_num - first_year,
    lag_assignment = paste(lag_office_id, lag_ka, lag_kakari, sep = "_")
  )

# Add prior-year workers who exit before t so the retained regressor and fixed
# effects use the same worker-risk set. Promotion and salary-raise are zero if
# the worker was not retained.
exit_rows <- staff_lag %>%
  filter(year_num %in% years_of_interest, !(staff_id %in% drafted_ids)) %>%
  anti_join(df %>% distinct(staff_id, year_num), by = c("staff_id", "year_num")) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(office_drafts, by = c("lag_office_id" = "office_id", "year_num")) %>%
  mutate(
    n_drafted = replace_na(n_drafted, 0L),
    n_drafted_male = replace_na(n_drafted_male, 0L),
    lag_has_draft = as.integer(n_drafted_male > 0),
    retained = 0L,
    promoted = 0L,
    salary_raise = 0L,
    lag_salary_by_rank = if_else(!is.na(lag_salary_num) & lag_salary_num > 0, lag_salary_num, NA_real_),
    lag_salary_z = as.numeric(scale(lag_salary_by_rank)),
    productivity_score = rowSums(
      cbind(
        rank_to_prestige_score(lag_decor_rank),
        rank_to_prestige_score(lag_court_rank),
        replace_na(lag_salary_z, 0)
      ),
      na.rm = TRUE
    ),
    productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
    tenure = year_num - first_year,
    lag_assignment = paste(lag_office_id, lag_ka, lag_kakari, sep = "_"),
    is_female = NA
  )

retention_panel <- bind_rows(
  worker_year %>%
    select(staff_id, year_num, lag_office_id, lag_assignment, lag_pos_norm, lag_pos_rank,
           lag_salary_num, lag_decor_rank, lag_court_rank,
           lag_has_draft, n_drafted_male, retained, promoted, salary_raise, productive, tenure, is_female),
  exit_rows %>%
    select(staff_id, year_num, lag_office_id, lag_assignment, lag_pos_norm, lag_pos_rank,
           lag_salary_num, lag_decor_rank, lag_court_rank,
           lag_has_draft, n_drafted_male, retained, promoted, salary_raise, productive, tenure, is_female)
) %>%
  filter(!is.na(lag_assignment), !is.na(lag_pos_norm), !is.na(lag_pos_rank)) %>%
  mutate(
    lag_salary_z = if_else(
      !is.na(lag_salary_num) & lag_salary_num > 0,
      as.numeric(scale(lag_salary_num)),
      NA_real_
    ),
    productivity_score = rowSums(
      cbind(
        rank_to_prestige_score(lag_decor_rank),
        rank_to_prestige_score(lag_court_rank),
        replace_na(lag_salary_z, 0)
      ),
      na.rm = TRUE
    ),
    productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
    productive = replace_na(productive, 0L),
    retained = as.integer(retained),
    promoted = as.integer(promoted),
    salary_raise = as.integer(salary_raise)
  )

cat("Retention/promotion panel:", nrow(retention_panel), "worker-years\n")
cat("  Retained:", sum(retention_panel$retained), "\n")
cat("  Promoted:", sum(retention_panel$promoted), "\n")
cat("  Salary raise:", sum(retention_panel$salary_raise, na.rm = TRUE), "\n")

ret_promo <- feols(
  promoted ~ retained + lag_has_draft * productive + tenure |
    year_num + lag_assignment + lag_pos_norm + lag_pos_rank,
  data = retention_panel,
  cluster = ~lag_office_id
)

ret_raise <- feols(
  salary_raise ~ retained + lag_has_draft * productive + tenure |
    year_num + lag_assignment + lag_pos_norm + lag_pos_rank,
  data = retention_panel,
  cluster = ~lag_office_id
)

staff_lag_transfer <- df %>%
  select(staff_id, year_num, lag_office_id = office_id, lag_kyoku = kyoku,
         lag_ka = ka, lag_kakari = kakari, lag_pos_norm = pos_norm,
         lag_pos_rank = pos_rank, lag_salary_num = salary_num,
         lag_court_rank = court_rank, lag_decor_rank = decor_rank) %>%
  mutate(year_num = year_num + 1)

transfers <- df %>%
  filter(year_num %in% years_of_interest, !(staff_id %in% drafted_ids)) %>%
  left_join(staff_lag_transfer, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office_id), lag_office_id != office_id) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(office_drafts, by = c("office_id", "year_num")) %>%
  mutate(
    n_drafted = replace_na(n_drafted, 0L),
    n_drafted_male = replace_na(n_drafted_male, 0L),
    dest_has_draft = as.integer(n_drafted_male > 0),
    promoted = as.integer(!is.na(pos_rank) & !is.na(lag_pos_rank) & pos_rank > lag_pos_rank),
    salary_raise = case_when(
      is.na(salary_num) | is.na(lag_salary_num) ~ NA_integer_,
      salary_num > lag_salary_num ~ 1L,
      TRUE ~ 0L
    ),
    lag_salary_by_rank = if_else(!is.na(lag_salary_num) & lag_salary_num > 0, lag_salary_num, NA_real_),
    lag_salary_z = as.numeric(scale(lag_salary_by_rank)),
    productivity_score = rowSums(
      cbind(
        rank_to_prestige_score(lag_decor_rank),
        rank_to_prestige_score(lag_court_rank),
        replace_na(lag_salary_z, 0)
      ),
      na.rm = TRUE
    ),
    productive = as.integer(productivity_score >= median(productivity_score, na.rm = TRUE)),
    tenure = year_num - first_year,
    donor_assignment = paste(lag_office_id, lag_ka, lag_kakari, sep = "_"),
    dest_assignment = paste(office_id, ka, kakari, sep = "_")
  ) %>%
  filter(!is.na(donor_assignment), !is.na(lag_pos_norm), !is.na(lag_pos_rank))

cat("Internal-transfer panel:", nrow(transfers), "transfers\n")
cat("  To draft-vacancy offices:", sum(transfers$dest_has_draft), "\n")
cat("  Promoted at transfer:", sum(transfers$promoted), "\n")
cat("  Salary raise at transfer:", sum(transfers$salary_raise, na.rm = TRUE), "\n")

tr_promo <- feols(
  promoted ~ dest_has_draft * productive + tenure |
    year_num + donor_assignment + lag_pos_norm + lag_pos_rank,
  data = transfers,
  cluster = ~lag_office_id
)

tr_raise <- feols(
  salary_raise ~ dest_has_draft * productive + tenure |
    year_num + donor_assignment + lag_pos_norm + lag_pos_rank,
  data = transfers,
  cluster = ~lag_office_id
)

dict <- c(
  retained = "Retained",
  lag_has_draft = "Drafted peers in prior office",
  productive = "High productivity",
  "lag_has_draft:productive" = "Drafted peers $\\times$ High productivity",
  dest_has_draft = "Destination draft office",
  "dest_has_draft:productive" = "Destination draft $\\times$ High productivity",
  tenure = "Tenure"
)

etable(
  ret_promo, ret_raise,
  tex = TRUE,
  file = file.path(result_dir, "BonusPromotion_Retention_Regressions.tex"),
  replace = TRUE,
  title = "Retention, Peer Drafting, and Promotion or Salary Raises",
  dict = dict,
  headers = c("Promotion", "Salary raise"),
  drop = "tenure",
  fitstat = ~n + r2,
  notes = "Linear probability models at the worker-year level. The sample is non-drafted workers observed in the prior year; outcomes equal one if the worker is retained and receives a rank promotion or salary raise in year t. Fixed effects: year, prior office-section assignment, prior position, and prior rank. Standard errors clustered by prior office."
)

etable(
  tr_promo, tr_raise,
  tex = TRUE,
  file = file.path(result_dir, "BonusPromotion_Transfer_Regressions.tex"),
  replace = TRUE,
  title = "Internal Transfers into Draft Offices and Promotion or Salary Raises",
  dict = dict,
  headers = c("Promotion", "Salary raise"),
  drop = "tenure",
  fitstat = ~n + r2,
  notes = "Linear probability models at the internal-transfer level. Destination draft office equals one if the destination office-year had at least one male drafted worker. Fixed effects: year, donor office-section assignment, prior position, and prior rank. Standard errors clustered by donor office."
)

standalone_tex <- file.path(result_dir, "BonusPromotion_Results.tex")
writeLines(
  c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{caption}",
    "\\begin{document}",
    "\\section*{Bonus Salaries, Promotions, and Draft-Related Retention or Transfers}",
    "\\input{BonusPromotion_Retention_Regressions.tex}",
    "\\input{BonusPromotion_Transfer_Regressions.tex}",
    "\\end{document}"
  ),
  standalone_tex
)

compile_pdf <- function(tex_file) {
  old <- setwd(dirname(tex_file))
  on.exit(setwd(old), add = TRUE)
  pdflatex <- Sys.which("pdflatex")
  if (pdflatex == "" && file.exists("C:/TinyTeX/bin/windows/pdflatex.exe")) {
    pdflatex <- "C:/TinyTeX/bin/windows/pdflatex.exe"
  }
  if (pdflatex == "") stop("pdflatex not found")
  system2(
    pdflatex,
    c("-interaction=nonstopmode", "-halt-on-error", basename(tex_file)),
    stdout = TRUE,
    stderr = TRUE
  )
}

tryCatch(
  compile_pdf(standalone_tex),
  error = function(e) message("PDF compilation failed: ", conditionMessage(e))
)

coef_csv <- bind_rows(
  tidy(ret_promo) %>% mutate(model = "Retention: promotion"),
  tidy(ret_raise) %>% mutate(model = "Retention: salary raise"),
  tidy(tr_promo) %>% mutate(model = "Transfers: promotion"),
  tidy(tr_raise) %>% mutate(model = "Transfers: salary raise")
)
write_csv(coef_csv, file.path(result_dir, "bonuspromotion_coefficients.csv"))

desc_txt <- file.path(description_dir, "BonusPromotion_Description.txt")
writeLines(
  c(
    "Bonus/Promotion Regressions",
    "",
    paste0("Data: ", DATA_PATH),
    "",
    "Outcome definitions:",
    "Promotion equals one when the worker's rank in year t is higher than their rank in t-1. Salary raise is the bonus-salary proxy and equals one when parsed monthly-equivalent salary in year t exceeds parsed salary in t-1. Annual salary entries are divided by 12 before comparison.",
    "",
    "Productivity definition:",
    "High productivity is a prior-year indicator based on an observable productivity score: prior decoration class, prior court-rank class, and standardized prior salary. Japanese court and decoration classes are parsed as actual class numbers where lower numbers are higher status (for example, class 1 is better than class 8). The productivity score converts those classes to prestige points using max(0, 9 - class), so class 1 contributes more than class 8. Missing decoration/court ranks contribute zero, and workers at or above the sample median score are coded as high productivity.",
    "",
    "Regression 1: retention/peer draft exposure:",
    "The worker-year panel begins with non-drafted workers observed in the prior year. Promotion and salary-raise outcomes are regressed on retained status, a dummy for male drafted peers in the worker's prior office-year, high productivity, and the interaction of draft exposure with productivity. Fixed effects are year, prior office-section assignment, prior position, and prior rank; standard errors are clustered by prior office.",
    "",
    "Regression 2: internal transfers:",
    "The transfer panel includes non-drafted workers who moved to a different office from t-1 to t. Promotion and salary-raise outcomes are regressed on whether the destination office-year had a male draft vacancy, high productivity, and their interaction. Fixed effects are year, donor office-section assignment, prior position, and prior rank; standard errors are clustered by donor office.",
    "",
    paste0("Results TeX/PDF base: ", standalone_tex)
  ),
  desc_txt
)

cat("Wrote results TeX:", standalone_tex, "\n")
cat("Wrote description:", desc_txt, "\n")
cat("Done.\n")
