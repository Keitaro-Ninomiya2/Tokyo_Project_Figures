################################################################################
# Balance Test: Pre-war characteristics by future drafting status (1944)
#
# Unit: office_id x kakari x pos_norm x year (matching regression panel)
# Sample: restricted to offices observable in 1944
# Treatment: whether that office experienced any drafting in 1944
#
# Panel A: Unconditional comparison of means
# Panel B: Conditional comparison with ka x occupation + year FE
################################################################################

library(tidyverse)
library(fixest)

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years_v2.csv"
)

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", ""))

# ============================================================
# 1. IDENTIFY OFFICES WITH DRAFTING IN 1944
# ============================================================

# Offices that exist in 1944
offices_in_1944 <- df %>%
  filter(year_num == 1944) %>%
  distinct(office_id)

# Which of those offices had drafting
drafted_1944 <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  distinct(office_id) %>%
  mutate(ever_drafted_1944 = 1L)

cat("Offices observable in 1944:", nrow(offices_in_1944), "\n")
cat("Offices with drafting in 1944:", nrow(drafted_1944), "\n")

# ============================================================
# 2. BUILD PRE-WAR PANEL (1937-1942) AT KAKARI x POS LEVEL
# ============================================================

prewar_years <- 1937:1942

# --- Staff lag for transitions ---
staff_lag <- df %>%
  select(staff_id, year_num, lag_office = office_id, lag_ka = ka,
         lag_kyoku = kyoku, lag_kakari = kakari, lag_pos = pos_norm) %>%
  mutate(year_num = year_num + 1)

staff_first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

office_initial_year <- df %>%
  group_by(office_id) %>%
  summarise(office_first_year = min(year_num), .groups = "drop")

# Enriched rank (pre-1948)
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^局長$") ~ 5L,
    str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    TRUE ~ 2L
  )
}

# Occupation classification
classify_occ <- function(pos) {
  case_when(
    str_detect(pos, "技") ~ "engineer",
    str_detect(pos, "雇|傭") ~ "yato",
    TRUE ~ "non_engineer"
  )
}

# Worker-level transitions (pre-war only, offices that exist in 1944)
workers <- df %>%
  filter(year_num %in% prewar_years) %>%
  semi_join(offices_in_1944, by = "office_id") %>%   # <-- restrict to 1944-observable offices
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(staff_lag, by = c("staff_id", "year_num"),
            relationship = "many-to-many") %>%
  mutate(
    rank_e = assign_rank(pos_norm),
    occupation = classify_occ(pos_norm),
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    is_transfer_in = !is.na(lag_office) & (lag_office != office_id),
    is_external_transfer = is_transfer_in & (!is.na(lag_ka) & lag_ka != ka),
    lag_rank = assign_rank(lag_pos),
    is_promoted = case_when(
      is.na(lag_office) ~ NA,
      lag_office == office_id ~ rank_e > lag_rank,
      TRUE ~ FALSE
    )
  )

# ============================================================
# 3. AGGREGATE TO KAKARI x POS_NORM x YEAR LEVEL
# ============================================================

panel <- workers %>%
  group_by(office_id, ka, kyoku, kakari, pos_norm, occupation, year_num) %>%
  summarise(
    n_workers          = n(),
    n_female           = sum(is_female, na.rm = TRUE),
    female_share       = mean(is_female, na.rm = TRUE),
    n_new_hires        = sum(is_new_hire, na.rm = TRUE),
    new_hire_share     = mean(is_new_hire, na.rm = TRUE),
    n_promoted         = sum(is_promoted, na.rm = TRUE),
    promoted_share     = mean(is_promoted, na.rm = TRUE),
    n_ext_transfer     = sum(is_external_transfer, na.rm = TRUE),
    ext_transfer_share = mean(is_external_transfer, na.rm = TRUE),
    avg_rank           = mean(rank_e, na.rm = TRUE),
    .groups = "drop"
  )

# Merge drafting status
panel <- panel %>%
  left_join(drafted_1944, by = "office_id") %>%
  mutate(ever_drafted_1944 = replace_na(ever_drafted_1944, 0L))

cat("\nPanel obs (kakari x pos x year):", nrow(panel), "\n")
cat("  Drafted=1:", sum(panel$ever_drafted_1944 == 1), "\n")
cat("  Drafted=0:", sum(panel$ever_drafted_1944 == 0), "\n")
cat("  Unique offices (drafted):", panel %>% filter(ever_drafted_1944 == 1) %>%
      distinct(office_id) %>% nrow(), "\n")
cat("  Unique offices (not drafted):", panel %>% filter(ever_drafted_1944 == 0) %>%
      distinct(office_id) %>% nrow(), "\n")

# ============================================================
# 4. PANEL A: UNCONDITIONAL COMPARISON
# ============================================================

cat("\n\n========================================\n")
cat("PANEL A: UNCONDITIONAL COMPARISON\n")
cat("(kakari x pos_norm x year level)\n")
cat("========================================\n\n")

vars <- c("female_share", "new_hire_share", "promoted_share",
           "ext_transfer_share", "n_workers", "avg_rank")
var_labels <- c("Female Share", "New Hire Share", "Promoted Share",
                "Ext. Transfer Share", "N Workers", "Avg Rank")

results_uncond <- map2_dfr(vars, var_labels, function(v, lab) {
  d1 <- panel %>% filter(ever_drafted_1944 == 1) %>% pull(!!sym(v))
  d0 <- panel %>% filter(ever_drafted_1944 == 0) %>% pull(!!sym(v))

  tt <- t.test(d1, d0)

  tibble(
    Variable   = lab,
    `Drafted (mean)` = mean(d1, na.rm = TRUE),
    `Drafted (sd)`   = sd(d1, na.rm = TRUE),
    `Not Drafted (mean)` = mean(d0, na.rm = TRUE),
    `Not Drafted (sd)`   = sd(d0, na.rm = TRUE),
    `Diff`     = mean(d1, na.rm = TRUE) - mean(d0, na.rm = TRUE),
    `t-stat`   = tt$statistic,
    `p-value`  = tt$p.value
  )
})

print(results_uncond, width = Inf)

# ============================================================
# 5. PANEL B: CONDITIONAL COMPARISON (ka x occupation + year FE)
# ============================================================

cat("\n\n========================================\n")
cat("PANEL B: CONDITIONAL COMPARISON\n")
cat("(ka x occupation + year FE, clustered at office level)\n")
cat("========================================\n\n")

results_cond <- map2_dfr(vars, var_labels, function(v, lab) {
  fml <- as.formula(paste0(v, " ~ ever_drafted_1944 | ka + occupation + year_num"))

  tryCatch({
    m <- feols(fml, data = panel, cluster = ~office_id)

    coef_val <- coef(m)["ever_drafted_1944"]
    se_val   <- se(m)["ever_drafted_1944"]
    t_val    <- coef_val / se_val
    p_val    <- pvalue(m)["ever_drafted_1944"]
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
# 6. EXPORT LaTeX TABLE
# ============================================================

fmt <- function(x, d = 3) formatC(x, digits = d, format = "f")
fmt_p <- function(x) {
  case_when(
    x < 0.001 ~ "$<$0.001",
    TRUE ~ formatC(x, digits = 3, format = "f")
  )
}

n_drafted_offices <- panel %>% filter(ever_drafted_1944 == 1) %>% distinct(office_id) %>% nrow()
n_not_offices     <- panel %>% filter(ever_drafted_1944 == 0) %>% distinct(office_id) %>% nrow()
n_obs_d   <- sum(panel$ever_drafted_1944 == 1)
n_obs_nd  <- sum(panel$ever_drafted_1944 == 0)

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Balance Test: Pre-War Characteristics (1937--1942) by Future Drafting Status (1944)}",
  "\\label{tab:balance}",
  "\\small",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: Unconditional Comparison}",
  "\\medskip",
  "",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Drafted in 1944} & \\multicolumn{2}{c}{Not Drafted} & & \\\\",
  " & Mean & SD & Mean & SD & Diff & $p$-value \\\\",
  "\\midrule"
)

for (i in 1:nrow(results_uncond)) {
  r <- results_uncond[i, ]
  tex_lines <- c(tex_lines,
    paste0(r$Variable, " & ",
           fmt(r$`Drafted (mean)`), " & ",
           fmt(r$`Drafted (sd)`), " & ",
           fmt(r$`Not Drafted (mean)`), " & ",
           fmt(r$`Not Drafted (sd)`), " & ",
           fmt(r$Diff), " & ",
           fmt_p(r$`p-value`), " \\\\")
  )
}

tex_lines <- c(tex_lines,
  "\\midrule",
  paste0("Offices & \\multicolumn{2}{c}{", n_drafted_offices, "} & \\multicolumn{2}{c}{", n_not_offices, "} & & \\\\"),
  paste0("Obs (kakari $\\times$ pos $\\times$ year) & \\multicolumn{2}{c}{", format(n_obs_d, big.mark = ","), "} & \\multicolumn{2}{c}{", format(n_obs_nd, big.mark = ","), "} & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Conditional Comparison (Ka + Occupation + Year FE)}",
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
  "\\item \\textit{Notes:} Sample restricted to offices observable in both the pre-war period (1937--1942) and in 1944. Unit of observation: kakari $\\times$ position $\\times$ year, matching the regression panel. Panel~A reports unconditional means split by whether the office experienced any military drafting in 1944. Panel~B reports coefficients from OLS regressions of each characteristic on a drafted-in-1944 indicator, with ka, occupation, and year fixed effects. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_lines, "../BalanceTest.tex")
cat("\n\nLaTeX table exported to BalanceTest.tex\n")
