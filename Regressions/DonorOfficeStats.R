################################################################################
# Donor Office Descriptive Statistics
#
# Compares offices (kakari x position) that had transfers OUT vs. those that
# did not, conditional on ka x position FE.
#
# Panel A: Unconditional comparison of means
# Panel B: Conditional comparison (ka + position + year FE)
#
# Variables: has_new_hire (binary), n_new_hires (count), avg_tenure,
#            female_share
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
# KYOKU NORMALIZATION (same as NewTable1_Reallocation.R)
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

assign_kyoku_group <- function(nk) {
  case_when(
    is.na(nk) ~ NA_character_,
    nk %in% c("\u96fb\u6c17\u5c40", "\u4ea4\u901a\u5c40") ~ "transport",
    nk == "\u6c34\u9053\u5c40" ~ "water",
    nk == "\u6e2f\u6e7e\u5c40" ~ "port",
    nk == "\u6559\u80b2\u5c40" ~ "education",
    nk %in% c("\u571f\u6728\u5c40", "\u8a08\u753b\u5c40", "\u7d4c\u7406\u5c40", "\u5efa\u7bc9\u5c40", "\u9053\u8def\u5c40") ~ "infrastructure",
    nk %in% c("\u6230\u6642\u751f\u6d3b\u5c40", "\u7d4c\u6e08\u5c40", "\u4e2d\u592e\u5378\u58f2\u5e02\u5834", "\u7523\u696d\u5c40") ~ "economy",
    nk %in% c("\u5065\u6c11\u5c40", "\u539a\u751f\u5c40", "\u793e\u4f1a\u5c40", "\u4fdd\u5065\u5c40", "\u6c11\u751f\u5c40", "\u990a\u80b2\u9662", "\u52b4\u50cd\u5c40") ~ "welfare",
    nk %in% c("\u5e02\u4f1a\u4e8b\u52d9\u5c40", "\u5e9c\u4f1a\u4e8b\u52d9\u5c40") ~ "assembly",
    nk %in% c("\u8ca1\u52d9\u5c40", "\u76e3\u67fb\u5c40", "\u9577\u5b98\u5b98\u623f") ~ "finance_admin",
    nk == "\u5fa9\u8208\u4e8b\u696d\u5c40" ~ "reconstruction",
    nk == "\u5e02\u6c11\u5c40" ~ "citizen",
    nk == "\u9632\u885b\u5c40" ~ "defense",
    TRUE ~ NA_character_
  )
}

df <- df %>%
  mutate(norm_kyoku  = normalize_kyoku(kyoku),
         kyoku_group = assign_kyoku_group(norm_kyoku))

# ============================================================
# IDENTIFY TRANSFERS OUT
# ============================================================

# A worker transferred out of (office_id, kakari, pos_norm) in year t
# if they were at that unit in year t-1 but moved to a different office/kakari in year t

staff_lag <- df %>%
  select(staff_id, year_num, office_id, ka, kakari, pos_norm, kyoku_group) %>%
  rename(
    lag_office = office_id, lag_ka = ka, lag_kakari = kakari,
    lag_pos = pos_norm, lag_kyoku_group = kyoku_group
  ) %>%
  mutate(year_num = year_num + 1)

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

worker_transitions <- df %>%
  filter(year_num %in% years_of_interest) %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = (year_num == first_year),
    # This worker transferred INTO current position from somewhere else
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id | lag_kakari != kakari | lag_pos != pos_norm)
  )

# Compute transfers OUT: for each (lag_office, lag_kakari, lag_pos, year_num),
# count workers who left that unit
transfers_out <- worker_transitions %>%
  filter(!is.na(lag_office), !is.na(lag_kakari),
         (lag_office != office_id | lag_kakari != kakari | lag_pos != pos_norm)) %>%
  group_by(lag_office, lag_kakari, lag_pos, year_num) %>%
  summarise(n_transfers_out = n(), .groups = "drop") %>%
  rename(office_id = lag_office, kakari = lag_kakari, pos_norm = lag_pos)

# Also count workers who exited the dataset entirely (attrition, not just transfers)
# Workers present in t-1 but not in t (from that unit's perspective)
all_staff_by_unit <- df %>%
  filter(year_num %in% (min(years_of_interest) - 1):max(years_of_interest)) %>%
  select(staff_id, year_num, office_id, kakari, pos_norm)

exits <- all_staff_by_unit %>%
  mutate(next_year = year_num + 1) %>%
  anti_join(df %>% select(staff_id, year_num), by = c("staff_id", "next_year" = "year_num")) %>%
  filter(next_year %in% years_of_interest) %>%
  group_by(office_id, kakari, pos_norm, next_year) %>%
  summarise(n_exits = n(), .groups = "drop") %>%
  rename(year_num = next_year)

# ============================================================
# BUILD PANEL
# ============================================================

# Tenure: count of years each worker has appeared up to current year
tenure_data <- df %>%
  filter(year_num %in% years_of_interest) %>%
  group_by(staff_id) %>%
  mutate(first_seen = min(year_num)) %>%
  ungroup() %>%
  mutate(tenure = year_num - first_seen + 1)

# Aggregate to kakari x pos x year
panel <- tenure_data %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_workers     = n(),
    n_female      = sum(is_female, na.rm = TRUE),
    female_share  = mean(is_female, na.rm = TRUE),
    avg_tenure    = mean(tenure, na.rm = TRUE),
    .groups = "drop"
  )

# Add new hire info
new_hire_agg <- worker_transitions %>%
  group_by(office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    .groups = "drop"
  )

panel <- panel %>%
  left_join(new_hire_agg, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(transfers_out, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    n_new_hires     = replace_na(n_new_hires, 0),
    n_transfers_out = replace_na(n_transfers_out, 0),
    has_new_hire     = as.integer(n_new_hires > 0),
    is_donor         = as.integer(n_transfers_out > 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  )

panel_ka <- panel %>% filter(!is.na(ka_id))

cat("Panel:", nrow(panel_ka), "obs\n")
cat("  Donor (transfers out > 0):", sum(panel_ka$is_donor == 1), "\n")
cat("  Non-donor:", sum(panel_ka$is_donor == 0), "\n")

# ============================================================
# PANEL A: UNCONDITIONAL COMPARISON
# ============================================================

cat("\n\n========================================\n")
cat("PANEL A: UNCONDITIONAL COMPARISON\n")
cat("========================================\n\n")

vars <- c("has_new_hire", "n_new_hires", "avg_tenure", "female_share")
var_labels <- c("Has New Hire (=1)", "No. New Hires", "Avg Tenure (years)", "Female Share")

results_uncond <- map2_dfr(vars, var_labels, function(v, lab) {
  d1 <- panel_ka %>% filter(is_donor == 1) %>% pull(!!sym(v))
  d0 <- panel_ka %>% filter(is_donor == 0) %>% pull(!!sym(v))

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
# PANEL B: CONDITIONAL COMPARISON (Ka + Position + Year FE)
# ============================================================

cat("\n\n========================================\n")
cat("PANEL B: CONDITIONAL COMPARISON\n")
cat("(Ka + Position + Year FE, clustered at office level)\n")
cat("========================================\n\n")

results_cond <- map2_dfr(vars, var_labels, function(v, lab) {
  fml <- as.formula(paste0(v, " ~ is_donor | ka_id + pos_norm + year_num"))

  tryCatch({
    m <- feols(fml, data = panel_ka, cluster = ~office_id)

    coef_val <- coef(m)["is_donor"]
    se_val   <- se(m)["is_donor"]
    t_val    <- coef_val / se_val
    p_val    <- pvalue(m)["is_donor"]
    n_obs    <- m$nobs

    tibble(
      Variable  = lab,
      Coef      = coef_val,
      SE        = se_val,
      `t-stat`  = t_val,
      `p-value` = p_val,
      N         = n_obs,
      Signif    = case_when(
        p_val < 0.01 ~ "***",
        p_val < 0.05 ~ "**",
        p_val < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
  }, error = function(e) {
    cat("  Error for", lab, ":", conditionMessage(e), "\n")
    tibble(Variable = lab, Coef = NA_real_, SE = NA_real_,
           `t-stat` = NA_real_, `p-value` = NA_real_, N = NA_integer_, Signif = "")
  })
})

print(results_cond, width = Inf)

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

n_donor   <- panel_ka %>% filter(is_donor == 1) %>% distinct(office_id, kakari) %>% nrow()
n_nondonor <- panel_ka %>% filter(is_donor == 0) %>% distinct(office_id, kakari) %>% nrow()

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Characteristics of Donor Offices (Offices with Transfers Out)}",
  "\\label{tab:donor}",
  "\\small",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: Unconditional Comparison}",
  "\\medskip",
  "",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Donor} & \\multicolumn{2}{c}{Non-Donor} & & \\\\",
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

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Conditional Comparison (Ka + Position + Year FE)}",
  "\\medskip",
  "",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & Coef & SE & $t$-stat & $p$-value & $N$ \\\\",
  "\\midrule"
)

for (i in 1:nrow(results_cond)) {
  r <- results_cond[i, ]
  if (is.na(r$Coef)) {
    tex_lines <- c(tex_lines,
      paste0(r$Variable, " & --- & --- & --- & --- & --- \\\\"))
  } else {
    sig <- ifelse(is.na(r$Signif) | r$Signif == "", "", paste0("$^{", r$Signif, "}$"))
    tex_lines <- c(tex_lines,
      paste0(r$Variable, " & ",
             fmt(r$Coef, 4), sig, " & ",
             fmt(r$SE, 4), " & ",
             fmt(r$`t-stat`, 2), " & ",
             fmt_p(r$`p-value`), " & ",
             format(r$N, big.mark = ","), " \\\\"))
  }
}

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} Sample: kakari $\\times$ position $\\times$ year (1938--1945). A \\emph{donor} office is one that had at least one worker transfer out in that year. Panel~A reports unconditional means. Panel~B reports coefficients from OLS regressions of each characteristic on a donor indicator, with ka, position, and year fixed effects. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_lines, here("DonorOfficeStats.tex"))
writeLines(tex_lines, here("..", "Tokyo_Project", "Tables_Figures", "DonorOfficeStats.tex"))
cat("\n\nTable exported to DonorOfficeStats.tex\n")
