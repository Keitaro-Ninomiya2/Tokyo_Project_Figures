################################################################################
# Donor Office Stats: Lagged New Hiring Test
#
# Tests whether the new-hiring gap between donor and non-donor offices
# exists in the PREVIOUS YEAR (t-1), before the transfer occurs at t.
#
# If donor offices already hired more in t-1 → selection (not vacancy chains).
# If the gap only exists at t → consistent with vacancy chains.
#
# Unit: ka x position x year. FE: kyoku + position + year.
#
# Panel A: Unconditional comparison of t-1 outcomes by donor status at t
# Panel B: Conditional comparison (kyoku + position + year FE)
################################################################################

library(tidyverse)
library(fixest)
library(here)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

years_of_interest <- 1938:1945

# ============================================================
# KYOKU NORMALIZATION
# ============================================================

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "\u696d\u6240\u9577.*\u5065\u6c11|\u5bb6\u6240\u9577.*\u5065\u6c11") ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u696d\u6240\u9577.*\u539a\u751f")              ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u6771\u4eac\u5e02\u4e3b\u4e8b.*\u539a\u751f")  ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u6e05\u6383\u76e3\u7763.*\u539a\u751f")        ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u793e\u4f1a")       ~ "\u793e\u4f1a\u5c40",
    str_detect(k, "\u4fdd\u5065")       ~ "\u4fdd\u5065\u5c40",
    str_detect(k, "\u4e0a\u6728")       ~ "\u571f\u6728\u5c40",
    str_detect(k, "\u571f\u6728")       ~ "\u571f\u6728\u5c40",
    str_detect(k, "\u5e02\u4f1a\u4e8b\u52d9|Y\u4e8b\u52d9") ~ "\u5e02\u4f1a\u4e8b\u52d9\u5c40",
    str_detect(k, "\u5e9c\u4f1a\u4e8b\u52d9")   ~ "\u5e9c\u4f1a\u4e8b\u52d9\u5c40",
    str_detect(k, "\u767c\u80b2")       ~ "\u6559\u80b2\u5c40",
    str_detect(k, "\u6559\u80b2")       ~ "\u6559\u80b2\u5c40",
    str_detect(k, "\u6c34\u9055")       ~ "\u6c34\u9053\u5c40",
    str_detect(k, "\u6c34\u9053")       ~ "\u6c34\u9053\u5c40",
    str_detect(k, "\u96fb\u6c17")       ~ "\u96fb\u6c17\u5c40",
    str_detect(k, "\u990a\u80b2|\u6cbc\u52d9\u6240") ~ "\u990a\u80b2\u9662",
    str_detect(k, "\u8ca1\u52d9")       ~ "\u8ca1\u52d9\u5c40",
    str_detect(k, "\u7e70\u6e08|\u767c\u7d4c\u6e08|\u300a\u7d4c\u6e08|\u5b66\u6821\u4f53\u80b2\u8ab2\u7d4c\u6e08") ~ "\u7d4c\u6e08\u5c40",
    str_detect(k, "\u7d4c\u6e08")       ~ "\u7d4c\u6e08\u5c40",
    str_detect(k, "\u539a\u751f")       ~ "\u539a\u751f\u5c40",
    str_detect(k, "\u5e02\u6c11")       ~ "\u5e02\u6c11\u5c40",
    str_detect(k, "\u4e2d\u592e\u5378\u58f2")   ~ "\u4e2d\u592e\u5378\u58f2\u5e02\u5834",
    str_detect(k, "\u6e2f\u6e96")       ~ "\u6e2f\u6e7e\u5c40",
    str_detect(k, "\u6e2f\u6e7e")       ~ "\u6e2f\u6e7e\u5c40",
    str_detect(k, "\u76e3\u67fb")       ~ "\u76e3\u67fb\u5c40",
    str_detect(k, "\u7523\u696d")       ~ "\u7523\u696d\u5c40",
    str_detect(k, "\u57ce\u6771\u75c5\u9662")   ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u5065\u6c11")       ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u6230\u6642\u751f\u6d3b")   ~ "\u6230\u6642\u751f\u6d3b\u5c40",
    str_detect(k, "\u7d4c\u3013")       ~ "\u7d4c\u7406\u5c40",
    str_detect(k, "\u7d4c\u7406")       ~ "\u7d4c\u7406\u5c40",
    str_detect(k, "\u52b4\u50cd")       ~ "\u52b4\u50cd\u5c40",
    str_detect(k, "\u5efa\u7bc9|\u5fa1\u7bc9")  ~ "\u5efa\u7bc9\u5c40",
    str_detect(k, "\u9053\u8def")       ~ "\u9053\u8def\u5c40",
    str_detect(k, "\u5fa9\u8208|\u4f0a\u8208\u4e8b\u696d") ~ "\u5fa9\u8208\u4e8b\u696d\u5c40",
    str_detect(k, "\u4ea4\u901a")       ~ "\u4ea4\u901a\u5c40",
    str_detect(k, "\u8a08[\u6641\u753b]")   ~ "\u8a08\u753b\u5c40",
    str_detect(k, "\u6c11\u5c40")       ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u5f8c\u9189\u9662")     ~ "\u96fb\u6c17\u5c40",
    str_detect(k, "\u7bc9\u5730\u7523\u9662|\u8352\u7523\u9662") ~ "\u5065\u6c11\u5c40",
    str_detect(k, "\u9632\u885b")       ~ "\u9632\u885b\u5c40",
    str_detect(k, "\u6c11\u751f")       ~ "\u6c11\u751f\u5c40",
    str_detect(k, "\u9577\u5b98\u5b98\u623f")   ~ "\u9577\u5b98\u5b98\u623f",
    TRUE ~ NA_character_
  )
}

df <- df %>%
  mutate(norm_kyoku = normalize_kyoku(kyoku))

# ============================================================
# IDENTIFY TRANSFERS OUT (at ka level)
# ============================================================

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

df <- df %>% mutate(pos_rank = assign_rank(pos_norm))

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, pos_norm, norm_kyoku, pos_rank) %>%
  rename(lag_office = office_id, lag_ka = ka, lag_pos = pos_norm,
         lag_kyoku = norm_kyoku, lag_rank = pos_rank) %>%
  mutate(year_num = year_num + 1)

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

worker_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire    = (year_num == first_year),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id | lag_ka != ka | lag_pos != pos_norm),
    is_promoted    = case_when(
      is.na(lag_rank) ~ NA,
      TRUE            ~ pos_rank > lag_rank
    )
  )

# Transfers OUT at year t: only count workers who moved to a DIFFERENT kyoku
transfers_out <- worker_transitions %>%
  filter(!is.na(lag_office), !is.na(lag_ka), !is.na(lag_kyoku), !is.na(norm_kyoku),
         lag_kyoku != norm_kyoku) %>%
  group_by(lag_office, lag_ka, lag_pos, year_num) %>%
  summarise(n_transfers_out = n(), .groups = "drop") %>%
  rename(office_id = lag_office, ka = lag_ka, pos_norm = lag_pos)

# ============================================================
# BUILD OUTCOME PANEL (including t-1 years for lagged outcomes)
# ============================================================

outcome_years <- (min(years_of_interest) - 1):max(years_of_interest)

worker_transitions_expanded <- df %>%
  filter(year_num %in% outcome_years) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  mutate(is_new_hire = (year_num == first_year))

tenure_data <- df %>%
  filter(year_num %in% outcome_years) %>%
  group_by(staff_id) %>%
  mutate(first_seen = min(year_num)) %>%
  ungroup() %>%
  mutate(tenure = year_num - first_seen + 1)

panel_outcomes <- tenure_data %>%
  group_by(norm_kyoku, office_id, ka, pos_norm, year_num) %>%
  summarise(
    n_workers     = n(),
    n_female      = sum(is_female, na.rm = TRUE),
    female_share  = mean(is_female, na.rm = TRUE),
    avg_tenure    = mean(tenure, na.rm = TRUE),
    .groups = "drop"
  )

worker_transitions_expanded <- worker_transitions_expanded %>%
  left_join(
    df %>% select(staff_id, year_num, pos_rank) %>%
      rename(lag_rank = pos_rank) %>% mutate(year_num = year_num + 1),
    by = c("staff_id", "year_num")
  ) %>%
  mutate(is_promoted = case_when(
    is.na(lag_rank) ~ NA,
    TRUE            ~ assign_rank(pos_norm) > lag_rank
  ))

transition_agg <- worker_transitions_expanded %>%
  group_by(office_id, ka, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    n_promoted  = sum(is_promoted, na.rm = TRUE),
    .groups = "drop"
  )

panel_outcomes <- panel_outcomes %>%
  left_join(transition_agg, by = c("office_id", "ka", "pos_norm", "year_num")) %>%
  mutate(
    n_new_hires  = replace_na(n_new_hires, 0),
    n_promoted   = replace_na(n_promoted, 0),
    has_new_hire = as.integer(n_new_hires > 0)
  )

# ============================================================
# MERGE: donor status at t with outcomes at t-1
# ============================================================

donor_status <- transfers_out %>%
  mutate(is_donor = as.integer(n_transfers_out > 0)) %>%
  select(office_id, ka, pos_norm, year_num, is_donor, n_transfers_out)

# Shift: match donor status at year t with outcomes at year t-1
donor_status_for_lag <- donor_status %>%
  mutate(outcome_year = year_num - 1) %>%
  rename(donor_year = year_num) %>%
  select(office_id, ka, pos_norm, outcome_year, donor_year, is_donor, n_transfers_out)

panel_lagged <- panel_outcomes %>%
  filter(!is.na(ka), !is.na(norm_kyoku)) %>%
  left_join(donor_status_for_lag,
            by = c("office_id", "ka", "pos_norm", "year_num" = "outcome_year")) %>%
  mutate(
    is_donor         = replace_na(is_donor, 0L),
    n_transfers_out  = replace_na(n_transfers_out, 0L),
    donor_year       = if_else(is.na(donor_year), year_num + 1L, as.integer(donor_year))
  ) %>%
  filter(year_num %in% ((min(years_of_interest) - 1):(max(years_of_interest) - 1)))

# Add office size at t (the drafting year) as a control
size_at_t <- panel_outcomes %>%
  group_by(office_id, ka, pos_norm, year_num) %>%
  summarise(n_workers_t = sum(n_workers), .groups = "drop") %>%
  rename(donor_year = year_num)

panel_lagged <- panel_lagged %>%
  left_join(size_at_t, by = c("office_id", "ka", "pos_norm", "donor_year")) %>%
  mutate(log_n_workers_t = log(replace_na(n_workers_t, 0) + 1))

cat("Lagged panel:", nrow(panel_lagged), "obs\n")
cat("  Future donor (transfers out at t > 0):", sum(panel_lagged$is_donor == 1), "\n")
cat("  Future non-donor:", sum(panel_lagged$is_donor == 0), "\n")

# ============================================================
# PANEL A: UNCONDITIONAL COMPARISON (t-1 outcomes by donor status at t)
# ============================================================

cat("\n\n========================================\n")
cat("PANEL A: UNCONDITIONAL COMPARISON\n")
cat("(Outcomes at t-1, donor status at t)\n")
cat("========================================\n\n")

vars <- c("n_new_hires", "avg_tenure", "female_share", "n_promoted", "n_workers")
var_labels <- c("No. New Hires", "Avg Tenure (years)", "Female Share", "No. Promoted", "No. Workers")

results_uncond <- map2_dfr(vars, var_labels, function(v, lab) {
  d1 <- panel_lagged %>% filter(is_donor == 1) %>% pull(!!sym(v))
  d0 <- panel_lagged %>% filter(is_donor == 0) %>% pull(!!sym(v))

  tt <- t.test(d1, d0)

  tibble(
    Variable   = lab,
    `Donor (mean)` = mean(d1, na.rm = TRUE),
    `Donor (sd)`   = sd(d1, na.rm = TRUE),
    `Non-Donor (mean)` = mean(d0, na.rm = TRUE),
    `Non-Donor (sd)`   = sd(d0, na.rm = TRUE),
    `Diff`     = mean(d1, na.rm = TRUE) - mean(d0, na.rm = TRUE),
    `p-value`  = tt$p.value
  )
})

print(results_uncond, width = Inf)

# ============================================================
# PANEL B: CONDITIONAL COMPARISON — two specs per outcome
#   (1) Office (Ka) FE + Position + Year
#   (2) Department (Kyoku) FE + Position + Year
# ============================================================

run_spec_lag <- function(v, fe_str) {
  if (v == "n_workers") {
    fml <- as.formula(paste0(v, " ~ is_donor | ", fe_str))
  } else {
    fml <- as.formula(paste0(v, " ~ is_donor + log_n_workers_t | ", fe_str))
  }
  tryCatch({
    m <- feols(fml, data = panel_lagged, cluster = ~office_id)
    tibble(
      Coef   = coef(m)["is_donor"],
      SE     = se(m)["is_donor"],
      p_val  = pvalue(m)["is_donor"],
      N      = m$nobs,
      Signif = case_when(
        pvalue(m)["is_donor"] < 0.01 ~ "***",
        pvalue(m)["is_donor"] < 0.05 ~ "**",
        pvalue(m)["is_donor"] < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
  }, error = function(e) {
    tibble(Coef = NA_real_, SE = NA_real_, p_val = NA_real_, N = NA_integer_, Signif = "")
  })
}

results_lag_office <- map2_dfr(vars, var_labels, function(v, lab) {
  bind_cols(tibble(Variable = lab), run_spec_lag(v, "ka + pos_norm + year_num"))
})

results_lag_dept <- map2_dfr(vars, var_labels, function(v, lab) {
  bind_cols(tibble(Variable = lab), run_spec_lag(v, "norm_kyoku + pos_norm + year_num"))
})

cat("\nLagged — Office (Ka) FE:\n"); print(results_lag_office, width = Inf)
cat("\nLagged — Dept (Kyoku) FE:\n"); print(results_lag_dept, width = Inf)

n_clusters_lagged <- n_distinct(panel_lagged$office_id)
cat("Number of clusters (offices):", n_clusters_lagged, "\n")

# ============================================================
# EXPORT LaTeX TABLE
# ============================================================

fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
fmt_p <- function(x) {
  case_when(
    x < 0.001 ~ "$<$0.001",
    TRUE ~ formatC(x, digits = 3, format = "f")
  )
}

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Lagged Hiring Test: Prior-Year Outcomes by Future Donor Status}",
  "\\label{tab:donor_lagged}",
  "\\small",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: Unconditional Comparison}",
  "\\medskip",
  "",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Future Donor} & \\multicolumn{2}{c}{Non-Donor} & & \\\\",
  " & Mean & SD & Mean & SD & Diff & $p$-value \\\\",
  "\\midrule"
)

for (i in 1:nrow(results_uncond)) {
  r <- results_uncond[i, ]
  tex_lines <- c(tex_lines,
    paste0(r$Variable, " & ",
           fmt(r$`Donor (mean)`), " & ",
           fmt(r$`Donor (sd)`), " & ",
           fmt(r$`Non-Donor (mean)`), " & ",
           fmt(r$`Non-Donor (sd)`), " & ",
           fmt(r$Diff), " & ",
           fmt_p(r$`p-value`), " \\\\")
  )
}

fmt_row <- function(res, i) {
  r <- res[i, ]
  sig <- ifelse(is.na(r$Signif) | r$Signif == "", "", paste0("$^{", r$Signif, "}$"))
  list(
    coef = if (is.na(r$Coef)) "---" else paste0(fmt(r$Coef, 4), sig),
    se   = if (is.na(r$SE))   "---" else paste0("(", fmt(r$SE, 4), ")")
  )
}

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Conditional Comparison at $t{-}1$ (Position $\\times$ Year FE)}",
  "\\medskip",
  "",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Office FE & Dept.\\ FE \\\\",
  "\\midrule"
)

for (i in 1:nrow(results_lag_office)) {
  ro <- fmt_row(results_lag_office, i)
  rd <- fmt_row(results_lag_dept, i)
  tex_lines <- c(tex_lines,
    paste0(results_lag_office$Variable[i], " & ", ro$coef, " & ", rd$coef, " \\\\"),
    paste0(" & ", ro$se, " & ", rd$se, " \\\\")
  )
}

tex_lines <- c(tex_lines,
  "\\midrule",
  paste0("Observations & ",
         format(results_lag_office$N[1], big.mark = ","), " & ",
         format(results_lag_dept$N[1],   big.mark = ","), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} Outcomes measured at year $t{-}1$; donor status at year $t$. ",
         "Panel~A reports unconditional means. Panel~B reports OLS coefficients on a donor indicator ",
         "with position and year fixed effects plus either office (ka) or department (kyoku) FE, ",
         "controlling for log headcount at $t$. ",
         "Standard errors clustered at the office level (", n_clusters_lagged, " clusters). ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_lines, here("DonorOfficeStats_Lagged.tex"))
writeLines(tex_lines, here("..", "Tokyo_Project", "Tables_Figures", "DonorOfficeStats_Lagged.tex"))
cat("\n\nTable exported to DonorOfficeStats_Lagged.tex\n")
