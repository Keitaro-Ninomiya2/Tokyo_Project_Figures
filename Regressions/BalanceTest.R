################################################################################
# Balance Test: Pre-war characteristics by future drafting intensity (1944)
#
# Unit: office_id x kakari x pos_norm x year (matching regression panel)
# Sample: restricted to offices observable in 1944
# Treatment:
#   Panel A — binary: any drafting in 1944 (unconditional means comparison)
#   Panel B — continuous: count of males drafted from the same office in 1944
#             (matches the treatment variable in the main regressions)
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
  "Tokyo_Personnel_Master_All_Years.csv"
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
# 1. IDENTIFY OFFICES WITH DRAFTING IN 1944 (binary + count)
# ============================================================

# Offices that exist in 1944
offices_in_1944 <- df %>%
  filter(year_num == 1944) %>%
  distinct(office_id)

# Binary: any drafting in 1944
drafted_1944 <- df_all %>%
  filter(year_num == 1944, drafted == TRUE) %>%
  distinct(office_id) %>%
  mutate(ever_drafted_1944 = 1L)

# Continuous: count of males drafted from each office in 1944
draft_count_1944 <- df_all %>%
  filter(year_num == 1944, drafted == TRUE, is_female == FALSE) %>%
  group_by(office_id) %>%
  summarise(n_drafted_male_1944 = n(), .groups = "drop")

cat("Offices observable in 1944:", nrow(offices_in_1944), "\n")
cat("Offices with drafting in 1944:", nrow(drafted_1944), "\n")
cat("Draft count distribution (among drafted offices):\n")
print(summary(draft_count_1944$n_drafted_male_1944))

# ============================================================
# 2. BUILD PRE-WAR PANEL (1937-1942) AT KAKARI x POS LEVEL
# ============================================================

# Use strictly pre-treatment years (before any drafting in 1938)
prewar_years <- 1934:1937

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
  semi_join(offices_in_1944, by = "office_id") %>%
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
    is_internal_transfer = is_transfer_in,
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
    n_internal_transfer = sum(is_internal_transfer, na.rm = TRUE),
    n_ext_transfer     = sum(is_external_transfer, na.rm = TRUE),
    ext_transfer_share = mean(is_external_transfer, na.rm = TRUE),
    avg_rank           = mean(rank_e, na.rm = TRUE),
    .groups = "drop"
  )

# Merge both binary and continuous drafting measures
panel <- panel %>%
  left_join(drafted_1944, by = "office_id") %>%
  left_join(draft_count_1944, by = "office_id") %>%
  mutate(ever_drafted_1944 = replace_na(ever_drafted_1944, 0L),
         n_drafted_male_1944 = replace_na(n_drafted_male_1944, 0L),
         log_n_workers = log(n_workers + 1))

cat("\nPanel obs (kakari x pos x year):", nrow(panel), "\n")
cat("  Drafted=1:", sum(panel$ever_drafted_1944 == 1), "\n")
cat("  Drafted=0:", sum(panel$ever_drafted_1944 == 0), "\n")
cat("  Unique offices (drafted):", panel %>% filter(ever_drafted_1944 == 1) %>%
      distinct(office_id) %>% nrow(), "\n")
cat("  Unique offices (not drafted):", panel %>% filter(ever_drafted_1944 == 0) %>%
      distinct(office_id) %>% nrow(), "\n")

# ============================================================
# 3b. IDENTIFY FUTURE DONOR OFFICES (wartime 1938-1944)
#     A donor office is one that sent at least one worker to
#     an office that experienced military drafting in 1944.
# ============================================================

# Drafted offices in 1944
drafted_office_ids <- drafted_1944 %>% pull(office_id)

# Wartime lag: where each worker came from in t-1
staff_lag_war <- df %>%
  select(staff_id, year_num, lag_office = office_id) %>%
  mutate(year_num = year_num + 1)

# Workers who arrived at a drafted office during 1938-1944,
# and came from a different office => that origin is a donor
future_donor_offices <- df %>%
  filter(year_num %in% 1938:1944,
         office_id %in% drafted_office_ids) %>%
  left_join(staff_lag_war, by = c("staff_id", "year_num")) %>%
  filter(!is.na(lag_office), lag_office != office_id) %>%
  distinct(lag_office) %>%
  rename(office_id = lag_office) %>%
  mutate(future_donor = 1L)

panel <- panel %>%
  left_join(future_donor_offices, by = "office_id") %>%
  mutate(future_donor = replace_na(future_donor, 0L))

cat("\nFuture donor offices in pre-war panel:",
    panel %>% filter(future_donor == 1) %>% distinct(office_id) %>% nrow(), "\n")
cat("Non-donor offices:",
    panel %>% filter(future_donor == 0) %>% distinct(office_id) %>% nrow(), "\n")

# ============================================================
# 4. PANEL A: UNCONDITIONAL COMPARISON (binary split)
# ============================================================

cat("\n\n========================================\n")
cat("PANEL A: UNCONDITIONAL COMPARISON\n")
cat("(kakari x pos_norm x year level)\n")
cat("========================================\n\n")

vars <- c("female_share", "n_new_hires", "n_promoted",
          "n_internal_transfer", "n_workers", "avg_rank")
var_labels <- c("Female Share", "No. New Hires", "No. Promoted",
                "No. Internal Transfers", "N Workers", "Avg Rank")

results_uncond <- map2_dfr(vars, var_labels, function(v, lab) {
  d1 <- panel %>% filter(ever_drafted_1944 == 1) %>% pull(!!sym(v))
  d0 <- panel %>% filter(ever_drafted_1944 == 0) %>% pull(!!sym(v))

  # Drop NAs for t-test
  d1 <- d1[!is.na(d1)]
  d0 <- d0[!is.na(d0)]

  if (length(d1) < 2 | length(d0) < 2) {
    return(tibble(Variable = lab, `Drafted (mean)` = NA, `Drafted (sd)` = NA,
                  `Not Drafted (mean)` = NA, `Not Drafted (sd)` = NA,
                  Diff = NA, `p-value` = NA))
  }

  tt <- t.test(d1, d0)

  tibble(
    Variable   = lab,
    `Drafted (mean)` = mean(d1, na.rm = TRUE),
    `Drafted (sd)`   = sd(d1, na.rm = TRUE),
    `Not Drafted (mean)` = mean(d0, na.rm = TRUE),
    `Not Drafted (sd)`   = sd(d0, na.rm = TRUE),
    `Diff`     = mean(d1, na.rm = TRUE) - mean(d0, na.rm = TRUE),
    `p-value`  = tt$p.value
  )
})

print(results_uncond, width = Inf)

# ============================================================
# 5. PANEL B: CONDITIONAL COMPARISON
#    Treatment: continuous draft count (matching main regressions)
#    FE: ka + pos_norm + year (matching main regression FE)
#    Control: log(n_workers + 1) as size control
#    Clustered at office level
# ============================================================

cat("\n\n========================================\n")
cat("PANEL B: CONDITIONAL COMPARISON\n")
cat("Treatment: n_drafted_male_1944 (continuous)\n")
cat("FE: ka + position + year, control: log headcount\n")
cat("========================================\n\n")

results_cond <- map2_dfr(vars, var_labels, function(v, lab) {
  # For n_workers outcome, don't control for log_n_workers (would be circular)
  if (v == "n_workers") {
    fml <- as.formula(paste0(v, " ~ n_drafted_male_1944 | ka + pos_norm + year_num"))
  } else {
    fml <- as.formula(paste0(v, " ~ n_drafted_male_1944 + log_n_workers | ka + pos_norm + year_num"))
  }

  tryCatch({
    # Drop rows with NA outcome before running
    panel_clean <- panel %>% filter(!is.na(!!sym(v)))

    if (nrow(panel_clean) < 10) {
      cat("  Skipping", lab, ": too few non-NA observations\n")
      return(tibble(Variable = lab, Coef = NA_real_, SE = NA_real_,
                    `t-stat` = NA_real_, `p-value` = NA_real_, N = NA_integer_, Signif = ""))
    }

    m <- feols(fml, data = panel_clean, cluster = ~office_id)

    coef_val <- coef(m)["n_drafted_male_1944"]
    se_val   <- se(m)["n_drafted_male_1944"]
    t_val    <- coef_val / se_val
    p_val    <- pvalue(m)["n_drafted_male_1944"]
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

# Count clusters
n_clusters <- panel %>% distinct(office_id) %>% nrow()
cat("\nNumber of clusters (offices):", n_clusters, "\n")

# ============================================================
# 5b. CONDITIONAL COMPARISON (EXTENSIVE MARGIN, BINARY TREATMENT)
#     Treatment: ever_drafted_1944 (binary)
#     FE: ka + pos_norm + year
#     Control: log(n_workers + 1) as size control
#     Clustered at office level
# ============================================================

cat("\n\n========================================\n")
cat("PANEL B (ALT): CONDITIONAL EXTENSIVE COMPARISON\n")
cat("Treatment: ever_drafted_1944 (binary)\n")
cat("FE: ka + position + year, control: log headcount\n")
cat("========================================\n\n")

results_cond_ext <- map2_dfr(vars, var_labels, function(v, lab) {
  if (v == "n_workers") {
    fml <- as.formula(paste0(v, " ~ ever_drafted_1944 | ka + pos_norm + year_num"))
  } else {
    fml <- as.formula(paste0(v, " ~ ever_drafted_1944 + log_n_workers | ka + pos_norm + year_num"))
  }

  tryCatch({
    panel_clean <- panel %>% filter(!is.na(!!sym(v)))

    if (nrow(panel_clean) < 10) {
      return(tibble(Variable = lab, Coef = NA_real_, SE = NA_real_,
                    `t-stat` = NA_real_, `p-value` = NA_real_, N = NA_integer_, Signif = ""))
    }

    m <- feols(fml, data = panel_clean, cluster = ~office_id)

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
    tibble(Variable = lab, Coef = NA_real_, SE = NA_real_,
           `t-stat` = NA_real_, `p-value` = NA_real_, N = NA_integer_, Signif = "")
  })
})

print(results_cond_ext, width = Inf)

# ============================================================
# 6. PANEL C: PRE-WAR PROMOTION BY FUTURE DONOR STATUS
#    Unit: kakari x occupation x year (pre-war)
#    Outcome: n_promoted (count)
#    Treatment: future_donor (binary)
#    FE: Ka + occupation + year
# ============================================================

cat("\n\n========================================\n")
cat("PANEL C: PRE-WAR PROMOTION BY FUTURE DONOR STATUS\n")
cat("Unit: kakari x occupation x year\n")
cat("FE: Ka + occupation + year\n")
cat("========================================\n\n")

# Re-aggregate existing pre-war panel to office x occupation x year
# (kakari field is sparse; office_id is the finest reliable unit)
panel_c_cluster <- workers %>%
  left_join(future_donor_offices, by = "office_id") %>%
  mutate(future_donor = replace_na(future_donor, 0L),
         ka_id = if_else(!is.na(ka) & !is.na(kyoku),
                         paste(kyoku, ka, sep = "_"), NA_character_)) %>%
  filter(!is.na(ka_id)) %>%
  group_by(office_id, ka_id, occupation, year_num, future_donor) %>%
  summarise(
    n_workers  = n(),
    n_promoted = sum(is_promoted, na.rm = TRUE),
    .groups    = "drop"
  )

panel_c <- panel_c_cluster  # same object, used for both reporting and regression

cat("Panel C obs (office x occupation x year):", nrow(panel_c), "\n")
cat("  future_donor=1:", sum(panel_c$future_donor == 1), "\n")
cat("  future_donor=0:", sum(panel_c$future_donor == 0), "\n")

# Unconditional means
cat("\nUnconditional means of n_promoted:\n")
panel_c %>%
  group_by(future_donor) %>%
  summarise(mean_n_promoted = mean(n_promoted, na.rm = TRUE),
            sd_n_promoted   = sd(n_promoted, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  print()

mc <- feols(n_promoted ~ future_donor + log(n_workers + 1) |
              ka_id + occupation + year_num,
            data = panel_c_cluster, cluster = ~office_id)

cat("\nPanel C regression result:\n")
etable(mc, se.below = TRUE, fitstat = ~n + r2)

# Store results for LaTeX
coef_c  <- coef(mc)["future_donor"]
se_c    <- se(mc)["future_donor"]
t_c     <- coef_c / se_c
p_c     <- pvalue(mc)["future_donor"]
n_c     <- mc$nobs
sig_c   <- case_when(p_c < 0.01 ~ "***", p_c < 0.05 ~ "**",
                     p_c < 0.10 ~ "*", TRUE ~ "")

n_donor_offices    <- panel_c_cluster %>% filter(future_donor == 1) %>%
                        distinct(office_id) %>% nrow()
n_nondnr_offices   <- panel_c_cluster %>% filter(future_donor == 0) %>%
                        distinct(office_id) %>% nrow()
n_clusters_c       <- panel_c_cluster %>% distinct(office_id) %>% nrow()

# ============================================================
# 6. EXPORT LaTeX TABLE
# ============================================================

fmt <- function(x, d = 3) {
  ifelse(is.na(x), "---", formatC(x, digits = d, format = "f"))
}
fmt_p <- function(x) {
  case_when(
    is.na(x) ~ "---",
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
  "\\caption{Balance Test: Pre-War Characteristics (1934--1937) by Future Drafting Status (1944)}",
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

fmt_c <- function(x, d = 4) ifelse(is.na(x), "---", formatC(x, digits = d, format = "f"))

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel C: Pre-War Promotion by Future Donor Status (Ka + Occupation + Year FE)}",
  "\\medskip",
  "",
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  " & Coef & SE & $t$-stat & $p$-value & $N$ \\\\",
  "\\midrule",
  paste0("No.\\ promoted workers & ",
         fmt_c(coef_c), ifelse(sig_c == "", "", paste0("$^{", sig_c, "}$")), " & ",
         fmt_c(se_c), " & ",
         formatC(t_c, digits = 2, format = "f"), " & ",
         fmt_p(p_c), " & ",
         format(n_c, big.mark = ","), " \\\\"),
  "\\midrule",
  paste0("Future donor offices & \\multicolumn{5}{l}{", n_donor_offices, " offices} \\\\"),
  paste0("Non-donor offices    & \\multicolumn{5}{l}{", n_nondnr_offices, " offices} \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} Sample restricted to offices observable in both the pre-war period (1934--1937, before any military drafting) and in 1944. Unit of observation: kakari $\\times$ position $\\times$ year, matching the regression panel. Panel~A reports unconditional means split by whether the office experienced any military drafting in 1944. Panel~B reports coefficients from OLS regressions of each pre-war characteristic on the 1944 male draft count (continuous, matching the treatment variable in the main regressions), with section, position, and year fixed effects, controlling for log office headcount (except when outcome is headcount). Transfer and hiring outcomes are entered in raw counts (internal transfers and new hires, not shares). Standard errors clustered at the office level (", n_clusters, " clusters). Panel~C reports the coefficient from an OLS regression of pre-war number of promoted workers (at the office $\\times$ occupation $\\times$ year level) on a future donor indicator (offices that sent at least one worker to a drafted office during 1938--1944), with section, occupation, and year fixed effects, clustered at the office level (", n_clusters_c, " clusters). $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

# Write to both locations
writeLines(tex_lines, "../BalanceTest.tex")

paper_path <- file.path(Sys.getenv("USERPROFILE"),
  "Documents", "GitHub", "Tokyo_Project", "Tables_Figures", "BalanceTest.tex")
if (dir.exists(dirname(paper_path))) {
  writeLines(tex_lines, paper_path)
  cat("Table also exported to Tokyo_Project/Tables_Figures/BalanceTest.tex\n")
}

cat("\nLaTeX table exported to BalanceTest.tex\n")
