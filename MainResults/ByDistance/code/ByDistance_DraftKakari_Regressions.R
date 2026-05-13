################################################################################
# Drafted Workers and Replacement Margins
#
# Unit: destination office x kakari x occupation x year, 1938--1945.
# Treatment: number of drafted male workers in that destination
#            kakari-occupation-year.
#
# Outcomes:
#   1. Total internal replacement
#   2. Same-section retention (same ka group)
#   3. Same-department transfer (same kyoku group, different ka group)
#   4. Different-department transfer (different kyoku group)
#   5. New hires (excluding first observed office-years)
#
# Geographic benchmark:
#   Internal replacements split by prior-office distance to destination:
#   <=2 km, >2 km, or unknown distance.
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(broom)
})

root_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
code_dir <- file.path(root_dir, "MainResults", "ByDistance", "code")
result_dir <- file.path(root_dir, "MainResults", "ByDistance", "results")
description_dir <- file.path(root_dir, "MainResults", "ByDistance", "descriptions")

dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(description_dir, recursive = TRUE, showWarnings = FALSE)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)
KA_GROUP_PATH <- file.path(root_dir, "Regressions", "ka_group_map.csv")
OFFICE_DISTANCE_PATH <- file.path(
  root_dir, "Replacements", "MainResults", "DescriptiveData", "office_ward_distance.csv"
)

years_of_interest <- 1938:1945

load_department_helpers <- function() {
  helper_file <- file.path(root_dir, "Regressions", "NewTable1c_TransferType.R")
  helper_lines <- readLines(helper_file, warn = FALSE)
  eval(parse(text = helper_lines[55:126]), envir = parent.frame())
}

load_department_helpers()

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.10 ~ "$^{*}$",
    TRUE ~ ""
  )
}

fmt_est <- function(model) {
  tt <- tidy(model) %>% filter(term == "n_drafted_male")
  sprintf("%.4f%s", tt$estimate, stars(tt$p.value))
}

fmt_se <- function(model) {
  tt <- tidy(model) %>% filter(term == "n_drafted_male")
  sprintf("(%.4f)", tt$std.error)
}

compile_pdf <- function(tex_file) {
  old <- setwd(dirname(tex_file))
  on.exit(setwd(old), add = TRUE)
  pdflatex <- Sys.which("pdflatex")
  if (pdflatex == "" && file.exists("C:/TinyTeX/bin/windows/pdflatex.exe")) {
    pdflatex <- "C:/TinyTeX/bin/windows/pdflatex.exe"
  }
  if (pdflatex == "") stop("pdflatex not found")
  system2(pdflatex, c("-interaction=nonstopmode", "-halt-on-error", basename(tex_file)))
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371.0088
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

cat("Loading master data...\n")
df_names <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    kyoku_clean = replace_na(kyoku, ""),
    ka_clean = replace_na(ka, ""),
    norm_kyoku = normalize_kyoku(kyoku),
    kyoku_group = assign_kyoku_group(norm_kyoku)
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", "")
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

ka_group_raw <- read_csv(KA_GROUP_PATH, show_col_types = FALSE) %>%
  mutate(kyoku = replace_na(kyoku, ""))

df_names <- df_names %>%
  left_join(
    ka_group_raw,
    by = c("year_num" = "year", "kyoku_clean" = "kyoku", "ka_clean" = "ka")
  )

office_geo <- read_csv(OFFICE_DISTANCE_PATH, show_col_types = FALSE, na = c("", "NA")) %>%
  mutate(
    office_id = as.integer(office_id),
    ward_centroid_lat = as.numeric(ward_centroid_lat),
    ward_centroid_lon = as.numeric(ward_centroid_lon),
    distance_to_yurakucho_km = as.numeric(distance_to_yurakucho_km)
  ) %>%
  filter(!is.na(office_id)) %>%
  arrange(is.na(ward_current), year_num) %>%
  group_by(office_id) %>%
  summarise(
    ward_current = first(ward_current),
    ward_centroid_lat = first(ward_centroid_lat),
    ward_centroid_lon = first(ward_centroid_lon),
    distance_to_yurakucho_km = first(distance_to_yurakucho_km),
    .groups = "drop"
  )

office_initial_year <- df_names %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

staff_first_year <- df_names %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

staff_lag <- df_names %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(
    staff_id, year_num,
    lag_office_id = office_id,
    lag_kyoku_group = kyoku_group,
    lag_ka = ka,
    lag_ka_group = ka_group
  ) %>%
  mutate(year_num = year_num + 1)

staff_transitions <- df_names %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  left_join(
    office_geo %>% rename_with(~paste0("dest_", .), -office_id),
    by = "office_id"
  ) %>%
  left_join(
    office_geo %>% rename_with(~paste0("origin_", .), -office_id),
    by = c("lag_office_id" = "office_id")
  ) %>%
  mutate(
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group) ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) & ka_group == lag_ka_group ~ "same_section",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka ~ "same_section",
      lag_kyoku_group == kyoku_group ~ "same_department",
      lag_kyoku_group != kyoku_group ~ "different_department",
      TRUE ~ NA_character_
    ),
    # Match Table7_NewHires.R: do not treat first observed office-years as new hires.
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    ),
    is_internal = arrival_type %in% c(
      "same_section", "same_department", "different_department"
    ),
    origin_dest_distance_km = if_else(
      is_internal & !is.na(lag_office_id),
      haversine_km(
        origin_ward_centroid_lat, origin_ward_centroid_lon,
        dest_ward_centroid_lat, dest_ward_centroid_lon
      ),
      NA_real_
    ),
    internal_distance_bin = case_when(
      !is_internal ~ NA_character_,
      is.na(origin_dest_distance_km) ~ "unknown_distance",
      origin_dest_distance_km <= 2 ~ "near_0_2km",
      origin_dest_distance_km > 2 ~ "far_over_2km"
    )
  )

position_outcomes <- staff_transitions %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_same_section = sum(arrival_type == "same_section", na.rm = TRUE),
    n_same_department = sum(arrival_type == "same_department", na.rm = TRUE),
    n_different_department = sum(arrival_type == "different_department", na.rm = TRUE),
    n_internal_near = sum(internal_distance_bin == "near_0_2km", na.rm = TRUE),
    n_internal_far = sum(internal_distance_bin == "far_over_2km", na.rm = TRUE),
    n_internal_unknown_distance = sum(internal_distance_bin == "unknown_distance", na.rm = TRUE),
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(n_total_internal = n_same_section + n_same_department + n_different_department)

cumul_male_stock <- map_dfr(years_of_interest, function(yr) {
  df_names %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

position_drafts <- df_all %>%
  filter(year_num %in% years_of_interest, drafted == TRUE) %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_drafted = n(),
    n_drafted_male = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

panel <- position_outcomes %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(cumul_n_male, n_drafted, n_drafted_male), ~replace_na(.x, 0)),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  ) %>%
  filter(!is.na(ka_id))

cat("Regression panel:", nrow(panel), "destination occupation-kakari-years\n")
cat("Total internal:", sum(panel$n_total_internal), "\n")
cat("Same section:", sum(panel$n_same_section), "\n")
cat("Same department:", sum(panel$n_same_department), "\n")
cat("Different department:", sum(panel$n_different_department), "\n")
cat("Internal <=2 km:", sum(panel$n_internal_near), "\n")
cat("Internal >2 km:", sum(panel$n_internal_far), "\n")
cat("Internal unknown distance:", sum(panel$n_internal_unknown_distance), "\n")
cat("New hires:", sum(panel$n_new_hires), "\n")

run_model <- function(y) {
  feols(
    as.formula(paste0(y, " ~ n_drafted_male + log(cumul_n_male + 1) | year_num + ka_id + pos_norm")),
    data = panel,
    cluster = ~office_id
  )
}

models <- list(
  total_internal = run_model("n_total_internal"),
  same_section = run_model("n_same_section"),
  same_department = run_model("n_same_department"),
  different_department = run_model("n_different_department"),
  internal_near = run_model("n_internal_near"),
  internal_far = run_model("n_internal_far"),
  internal_unknown_distance = run_model("n_internal_unknown_distance"),
  new_hires = run_model("n_new_hires")
)

coef_file <- file.path(result_dir, "bydistance_draftkakari_coefficients.csv")
imap_dfr(models, ~tidy(.x) %>% mutate(model = .y), .id = NULL) %>%
  write_csv(coef_file)

table_tex <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Drafted Workers and Replacement Margins}",
  "\\label{tab:bydistance-draft-kakari}",
  "\\small",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "\\multicolumn{6}{l}{\\textbf{Panel A. Replacement margins}} \\\\",
  " & Total internal & Same section & Same dept. & Diff. dept. & New hires \\\\",
  "\\midrule",
  paste0(
    "No. drafted workers & ",
    fmt_est(models$total_internal), " & ",
    fmt_est(models$same_section), " & ",
    fmt_est(models$same_department), " & ",
    fmt_est(models$different_department), " & ",
    fmt_est(models$new_hires), " \\\\"
  ),
  paste0(
    " & ",
    fmt_se(models$total_internal), " & ",
    fmt_se(models$same_section), " & ",
    fmt_se(models$same_department), " & ",
    fmt_se(models$different_department), " & ",
    fmt_se(models$new_hires), " \\\\"
  ),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textbf{Panel B. Geographic benchmark for internal replacements}} \\\\",
  " & Total internal & $\\leq$2 km & $>$2 km & Unknown dist. & New hires \\\\",
  "\\midrule",
  paste0(
    "No. drafted workers & ",
    fmt_est(models$total_internal), " & ",
    fmt_est(models$internal_near), " & ",
    fmt_est(models$internal_far), " & ",
    fmt_est(models$internal_unknown_distance), " & ",
    fmt_est(models$new_hires), " \\\\"
  ),
  paste0(
    " & ",
    fmt_se(models$total_internal), " & ",
    fmt_se(models$internal_near), " & ",
    fmt_se(models$internal_far), " & ",
    fmt_se(models$internal_unknown_distance), " & ",
    fmt_se(models$new_hires), " \\\\"
  ),
  "\\midrule",
  paste0("Observations & \\multicolumn{5}{c}{", format(nobs(models$total_internal), big.mark = ","), "} \\\\"),
  "Year FE & \\multicolumn{5}{c}{Yes} \\\\",
  "Section FE & \\multicolumn{5}{c}{Yes} \\\\",
  "Position FE & \\multicolumn{5}{c}{Yes} \\\\",
  "Log cumulative male baseline & \\multicolumn{5}{c}{Yes} \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regressions. ",
    "Unit of observation: destination office $\\times$ kakari $\\times$ occupation $\\times$ year, 1938--1945. ",
    "Each cell reports the coefficient on the number of drafted male workers in the same destination kakari-occupation-year. ",
    "``Same section'' counts workers retained in the same ka group. ",
    "``Same dept.'' counts workers from a different ka group within the same normalized kyoku group. ",
    "``Diff. dept.'' counts workers from a different normalized kyoku group. ",
    "``Total internal'' is the sum of same section, same department, and different department. ",
    "Panel B splits the same internal replacements by great-circle distance between prior-year and current-year office ward centroids, using the cached office-ward assignment file in Replacements/MainResults/DescriptiveData. ",
    "New hires follow Table7\\_NewHires.R and exclude first observed office-years. ",
    "All specifications include year, section, and position fixed effects, and control for log cumulative male baseline employment. ",
    "Standard errors clustered by office in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

tex_file <- file.path(result_dir, "ByDistance_DraftKakari_Results.tex")
writeLines(table_tex, tex_file)

standalone_tex <- file.path(result_dir, "ByDistance_DraftKakari_Results_standalone.tex")
writeLines(
  c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{caption}",
    "\\begin{document}",
    "\\input{ByDistance_DraftKakari_Results.tex}",
    "\\end{document}"
  ),
  standalone_tex
)

tryCatch(
  compile_pdf(standalone_tex),
  error = function(e) message("PDF compilation failed: ", conditionMessage(e))
)

final_pdf <- file.path(result_dir, "ByDistance_DraftKakari_Results.pdf")
compiled_pdf <- file.path(result_dir, "ByDistance_DraftKakari_Results_standalone.pdf")
if (file.exists(compiled_pdf)) {
  file.copy(compiled_pdf, final_pdf, overwrite = TRUE)
}

unlink(file.path(
  result_dir,
  c(
    "ByDistance_DraftKakari_Results_standalone.aux",
    "ByDistance_DraftKakari_Results_standalone.log",
    "ByDistance_DraftKakari_Results_standalone.tex",
    "ByDistance_DraftKakari_Results_standalone.pdf"
  )
))

desc_file <- file.path(description_dir, "ByDistance_DraftKakari_Description.txt")
writeLines(
  c(
    "Drafted Workers and Replacement Margins",
    "",
    paste0("Code: ", file.path(code_dir, "ByDistance_DraftKakari_Regressions.R")),
    paste0("Data: ", DATA_PATH),
    paste0("Table: ", tex_file),
    paste0("PDF: ", final_pdf),
    paste0("Coefficient CSV: ", coef_file),
    "",
    "Purpose:",
    "This table distinguishes internal replacement margins from external new hiring after drafting at the destination office x kakari x occupation x year level.",
    "",
    "Definitions:",
    "Same section: worker remains in the same ka group as the prior year.",
    "Same department: worker moves from a different ka group within the same normalized kyoku group.",
    "Different department: worker moves from a different normalized kyoku group.",
    "Total internal: same section + same department + different department.",
    "Geographic benchmark: internal replacements are split by prior-office distance to the destination office: <=2 km, >2 km, or unknown distance.",
    "New hires: worker's first observed year, excluding first observed office-years following Table7_NewHires.R.",
    "",
    "Regression specification:",
    "Outcome = number of workers in the replacement margin. Treatment = number of drafted male workers in the same destination kakari-occupation-year. All regressions include year, section, and position fixed effects and control for log cumulative male baseline employment. Standard errors are clustered by office.",
    "",
    paste0("Regression observations after singleton removal: ", format(nobs(models$total_internal), big.mark = ",")),
    paste0("Total internal workers: ", format(sum(panel$n_total_internal), big.mark = ",")),
    paste0("Same-section workers: ", format(sum(panel$n_same_section), big.mark = ",")),
    paste0("Same-department transfers: ", format(sum(panel$n_same_department), big.mark = ",")),
    paste0("Different-department transfers: ", format(sum(panel$n_different_department), big.mark = ",")),
    paste0("Internal replacements from <=2 km: ", format(sum(panel$n_internal_near), big.mark = ",")),
    paste0("Internal replacements from >2 km: ", format(sum(panel$n_internal_far), big.mark = ",")),
    paste0("Internal replacements with unknown distance: ", format(sum(panel$n_internal_unknown_distance), big.mark = ",")),
    paste0("New hires: ", format(sum(panel$n_new_hires), big.mark = ","))
  ),
  desc_file
)

cat("\n===== REPLACEMENT MARGIN RESULTS =====\n")
cat(paste(table_tex, collapse = "\n"), "\n")
cat("Wrote TeX:", tex_file, "\n")
cat("Wrote PDF:", final_pdf, "\n")
cat("Wrote coefficient CSV:", coef_file, "\n")
cat("Wrote description:", desc_file, "\n")
cat("Done.\n")
