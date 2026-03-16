################################################################################
# Summary Statistics Table
# 4 columns: Variable | Full Sample | Drafted Years | 1944
# "Drafted Years" = years with any drafted workers in Tokyo-Fu
################################################################################

library(tidyverse)
library(here)

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
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year),
         is_female = gender_modern == "female") %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Rank assignment
assign_rank <- function(pos, yr) {
  case_when(
    yr < 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr < 1948 & str_detect(pos, "^部長$|^次長$|^課長$|課長書記官|課長技師") ~ 4L,
    yr < 1948 & str_detect(pos, "^主事$|^技師$|^事務官$|^地方事務官$|^地方技師$|^地方農林技師$|^所長$|^校長$|^區長$") ~ 3L,
    yr < 1948 & str_detect(pos, "^雇$|^囑託員$|^臨時$|^土木雇$") ~ 1L,
    yr < 1948 ~ 2L,
    yr >= 1948 & str_detect(pos, "^局長$") ~ 5L,
    yr >= 1948 & str_detect(pos, "^部長$|^次長$") ~ 4L,
    yr >= 1948 & str_detect(pos, "^課長$|^所長$|^校長$|^場長$|^園長$|^支所長$|^區長$|^看護婦長$|^寮長$") ~ 3L,
    yr >= 1948 & str_detect(pos, "^係長$") ~ 2L,
    yr >= 1948 ~ 1L
  )
}

df <- df %>% mutate(rank_e = assign_rank(pos_norm, year_num))

# Identify years with any drafted workers
draft_years <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(year_num) %>%
  pull(year_num) %>%
  sort()

cat("Years with drafted workers:", draft_years, "\n")

# Three samples
df_full    <- df
df_drafted <- df %>% filter(year_num %in% draft_years)
df_1944    <- df %>% filter(year_num == 1944)

cat("Drafted-year sample years:", sort(unique(df_drafted$year_num)), "\n")
cat("Drafted-year sample size:", nrow(df_drafted), "\n")

# ============================================================
# PANEL A: COUNTS
# ============================================================

fmt_n <- function(x) format(x, big.mark = ",")

compute_counts <- function(d) {
  list(
    workers = fmt_n(n_distinct(d$staff_id)),
    kyoku   = fmt_n(n_distinct(d$kyoku)),
    ka      = fmt_n(n_distinct(paste(d$kyoku, d$ka))),
    kakari  = fmt_n(n_distinct(paste(d$kyoku, d$ka, d$kakari))),
    women   = fmt_n(n_distinct(d$staff_id[d$is_female])),
    rank3   = fmt_n(sum(d$rank_e == 3)),
    rank2   = fmt_n(sum(d$rank_e == 2)),
    rank1   = fmt_n(sum(d$rank_e == 1))
  )
}

c_full    <- compute_counts(df_full)
c_drafted <- compute_counts(df_drafted)
c_1944    <- compute_counts(df_1944)

var_names <- c(
  "Unique workers",
  "Unique kyoku",
  "Unique ka (ka $\\times$ kyoku)",
  "Unique kakari (kakari $\\times$ ka $\\times$ kyoku)",
  "Unique women",
  "Workers at rank 3 (shiji/gishi)",
  "Workers at rank 2 (shoki/gite)",
  "Workers at rank 1 (yatoi/shokutaku)"
)

field_names <- c("workers", "kyoku", "ka", "kakari", "women", "rank3", "rank2", "rank1")

counts <- tibble(
  Variable = var_names,
  Full     = map_chr(field_names, ~c_full[[.x]]),
  Drafted  = map_chr(field_names, ~c_drafted[[.x]]),
  Y1944    = map_chr(field_names, ~c_1944[[.x]])
)

cat("\n========================================\n")
cat("PANEL A: COUNTS\n")
cat("========================================\n\n")
print(counts, width = Inf)

# ============================================================
# PANEL B: KAKARI-LEVEL AVERAGES (Full, Drafted, 1944)
# ============================================================

cat("\n\n========================================\n")
cat("PANEL B: KAKARI-LEVEL AVERAGES\n")
cat("========================================\n\n")

fmt_ms <- function(m, s) paste0(formatC(m, digits = 1, format = "f"),
                                 " (", formatC(s, digits = 1, format = "f"), ")")

compute_kakari_avgs <- function(d) {
  kakari_stats <- d %>%
    group_by(kyoku, ka, kakari, year_num) %>%
    summarise(
      n_workers = n(),
      n_roles   = n_distinct(pos_norm),
      .groups = "drop"
    )

  role_stats <- d %>%
    group_by(kyoku, ka, kakari, pos_norm, rank_e, year_num) %>%
    summarise(n_workers = n(), .groups = "drop") %>%
    group_by(rank_e) %>%
    summarise(
      mean_per_role = mean(n_workers),
      sd_per_role   = sd(n_workers),
      .groups = "drop"
    )

  get_rs <- function(r) {
    row <- role_stats %>% filter(rank_e == r)
    if (nrow(row) == 0) return("--")
    fmt_ms(row$mean_per_role, row$sd_per_role)
  }

  list(
    workers_kakari = fmt_ms(mean(kakari_stats$n_workers), sd(kakari_stats$n_workers)),
    roles_kakari   = fmt_ms(mean(kakari_stats$n_roles), sd(kakari_stats$n_roles)),
    rank3          = get_rs(3),
    rank2          = get_rs(2),
    rank1          = get_rs(1)
  )
}

b_full    <- compute_kakari_avgs(df_full)
b_drafted <- compute_kakari_avgs(df_drafted)
b_1944    <- compute_kakari_avgs(df_1944)

b_var_names <- c(
  "Workers per kakari",
  "Unique positions per kakari",
  "Workers per rank-3 role",
  "Workers per rank-2 role",
  "Workers per rank-1 role"
)
b_fields <- c("workers_kakari", "roles_kakari", "rank3", "rank2", "rank1")

avgs <- tibble(
  Variable = b_var_names,
  Full     = map_chr(b_fields, ~b_full[[.x]]),
  Drafted  = map_chr(b_fields, ~b_drafted[[.x]]),
  Y1944    = map_chr(b_fields, ~b_1944[[.x]])
)

print(avgs, width = Inf)

# ============================================================
# EXPORT LaTeX
# ============================================================

# Column header for drafted years
drafted_years_label <- paste0("Drafted Years (", paste(draft_years[draft_years < 1945], collapse = ", "), ")")

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Summary Statistics}",
  "\\label{tab:summary}",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: Sample Counts}",
  "\\medskip",
  "",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  paste0(" & Full Sample & \\makecell{Drafted Years} & 1944 \\\\"),
  "\\midrule"
)

for (i in 1:nrow(counts)) {
  tex <- c(tex,
    paste0(counts$Variable[i], " & ", counts$Full[i], " & ",
           counts$Drafted[i], " & ", counts$Y1944[i], " \\\\"))
}

tex <- c(tex,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Kakari-Level Averages}",
  "\\medskip",
  "",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  paste0(" & Full Sample & \\makecell{Drafted Years} & 1944 \\\\"),
  "\\midrule"
)

for (i in 1:nrow(avgs)) {
  tex <- c(tex, paste0(avgs$Variable[i], " & ", avgs$Full[i], " & ",
                       avgs$Drafted[i], " & ", avgs$Y1944[i], " \\\\"))
}

tex <- c(tex,
  "\\bottomrule",
  "\\end{tabular}",
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} Panel~A reports unique counts for the full sample, ",
         "the subset of years with any drafted workers (",
         paste(draft_years[draft_years < 1945], collapse = ", "),
         "), and 1944 alone. ",
         "Rank 3 = \\emph{shiji}/\\emph{gishi} (supervisory); ",
         "Rank 2 = \\emph{shoki}/\\emph{gite} (clerical/technical staff); ",
         "Rank 1 = \\emph{yatoi}/\\emph{shokutaku} (temporary/contract). ",
         "Panel~B reports kakari-level averages (standard deviations in parentheses). ",
         "A kakari is the lowest organizational unit (team), nested within ka (section) and kyoku (bureau)."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex, here("SummaryStats.tex"))
tp_path <- file.path(dirname(here()), "Tokyo_Project", "Tables_Figures", "SummaryStats.tex")
writeLines(tex, tp_path)
cat("\n\nSummary stats table exported to SummaryStats.tex and Tokyo_Project\n")
