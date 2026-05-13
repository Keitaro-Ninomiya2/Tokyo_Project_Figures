################################################################################
# Voluntary Male Exit Rate → Female Worker Volume
#
# Unit of observation: office_id × year, war years 1937–1944
#
# For each office in year t:
#   exit_rate = (males in office at t-1 not present anywhere at t, not drafted)
#               / (total males in office at t-1)
#   Offices with no t-1 presence get exit_rate = 0.
#
# DV: n_female_t — total female workers in office at year t
# FE: year   Cluster: kyoku
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

# ── Load data ─────────────────────────────────────────────────────────────────
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num   = as.numeric(year),
    is_female  = gender_modern == "female",
    is_drafted = drafted == TRUE
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
    str_detect(k, "社会")       ~ "社会局",
    str_detect(k, "保健")       ~ "保健局",
    str_detect(k, "上木|土木")  ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務")   ~ "府会事務局",
    str_detect(k, "發育|教育")  ~ "教育局",
    str_detect(k, "水違|水道")  ~ "水道局",
    str_detect(k, "電気")       ~ "電気局",
    TRUE ~ k
  )
}

df <- df %>% mutate(kyoku_norm = normalize_kyoku(kyoku))

years_of_interest <- 1937:1944

# ── Who is present anywhere in each year? ─────────────────────────────────────
present_each_year <- df %>%
  filter(year_num %in% c(years_of_interest - 1, max(years_of_interest))) %>%
  select(staff_id, year_num) %>%
  distinct()

# ── Office-level outcomes at year t ───────────────────────────────────────────
office_year_t <- df %>%
  filter(year_num %in% years_of_interest, !is.na(office_id)) %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_female_t  = sum(is_female, na.rm = TRUE),
    n_workers_t = n(),
    kyoku_norm  = first(kyoku_norm),
    gov_level   = first(gov_level),
    .groups = "drop"
  )

# ── Voluntary exits: males in office at t-1, absent anywhere at t ─────────────
males_t1 <- df %>%
  filter(year_num %in% (years_of_interest - 1), !is.na(office_id)) %>%
  filter(!is_female) %>%
  mutate(year_t = year_num + 1) %>%
  select(staff_id, office_id, year_t, is_drafted)

office_exits <- males_t1 %>%
  left_join(
    present_each_year %>%
      rename(year_t = year_num) %>%
      mutate(present_in_t = TRUE),
    by = c("staff_id", "year_t")
  ) %>%
  mutate(
    present_in_t   = coalesce(present_in_t, FALSE),
    voluntary_exit = !is_drafted & !present_in_t
  ) %>%
  group_by(office_id, year_t) %>%
  summarise(
    n_voluntary_exits = sum(voluntary_exit, na.rm = TRUE),
    n_male_t1         = n(),
    .groups = "drop"
  ) %>%
  rename(year_num = year_t)

# ── Build panel (left join: keep all offices in year t) ───────────────────────
panel <- office_year_t %>%
  left_join(office_exits, by = c("office_id", "year_num")) %>%
  mutate(
    n_voluntary_exits = coalesce(n_voluntary_exits, 0L),
    n_male_t1         = coalesce(n_male_t1, 0L),
    exit_rate         = n_voluntary_exits / pmax(n_male_t1, 1)
  )

cat("Panel:", nrow(panel), "office × year obs\n")
cat("Years:", sort(unique(panel$year_num)), "\n")
cat("Distinct offices:", length(unique(panel$office_id)), "\n\n")
cat("Year distribution:\n")
print(count(panel, year_num))
cat("\nMean female workers per office-year:", round(mean(panel$n_female_t), 2), "\n")
cat("Mean exit rate per office-year:", round(mean(panel$exit_rate), 3), "\n")

# ── Regressions ───────────────────────────────────────────────────────────────
# C1: full sample
c1 <- feols(n_female_t ~ exit_rate | year_num,
            data = panel, cluster = ~kyoku_norm)

# C2: Tokyo-Fu only
c2 <- feols(n_female_t ~ exit_rate | year_num,
            data = filter(panel, gov_level == "TokyoFu"), cluster = ~kyoku_norm)

# C3: Tokyo-Shi only
c3 <- feols(n_female_t ~ exit_rate | year_num,
            data = filter(panel, gov_level == "TokyoShi"), cluster = ~kyoku_norm)

cat("\n===== Voluntary Male Exit Rate → Female Worker Volume =====\n")
cat("DV: female workers in office | FE: year | Cluster: kyoku\n\n")

etable(c1, c2, c3,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Full", "Tokyo-Fu", "Tokyo-Shi"))

# ── Export LaTeX ──────────────────────────────────────────────────────────────
extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end   <- regexpr("\\\\end\\{tabular\\}",   tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else tex_raw
}

tex <- etable(c1, c2, c3,
              dict = c(exit_rate = "Voluntary exit rate"),
              headers = c("Full", "Tokyo-Fu", "Tokyo-Shi"),
              se.below = TRUE, fitstat = ~n + r2, tex = TRUE)

tc <- extract_tabular(tex)
tc <- tc[!grepl("Dependent Var\\.", tc)]

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Voluntary Male Exits and Female Worker Volume}",
  "\\label{tab:voluntary-exits}",
  "\\small",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: office $\\times$ year ",
         "(1937--1944). Dependent variable is the total number of female workers in the office. ",
         "Treatment is the voluntary male exit rate: the share of male workers present in the office at $t-1$ ",
         "who do not appear anywhere in the directory at $t$ and were not drafted. ",
         "Offices with no $t-1$ presence receive an exit rate of zero. ",
         "All specifications include year fixed effects. ",
         "Column~(1) uses the full sample; columns~(2) and~(3) restrict to Tokyo-Fu ",
         "(prefectural government) and Tokyo-Shi (city government) offices respectively. ",
         "Standard errors clustered at the bureau (kyoku) level. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("VoluntaryExits_FemaleHires.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "VoluntaryExits_FemaleHires.tex"))
cat("\nTable exported to VoluntaryExits_FemaleHires.tex\n")
