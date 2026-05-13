################################################################################
# Reduced Form: Wartime Draft Shock → Postwar Female Integration
#
# For each postwar kakari-year, link to the kakaricho's wartime office.
# Regress postwar female outcomes directly on the wartime draft shock
# (share of males drafted) in the kakaricho's wartime office,
# conditional on wartime ka and position FE.
################################################################################

library(tidyverse)
library(fixest)
library(here)

# ============================================================
# 0. LOAD DATA
# ============================================================

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

df <- read_csv(DATA_PATH,
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num   = as.numeric(year),
    is_female  = gender_modern == "female",
    pos_norm   = str_replace_all(position, "\\s+", ""),
    is_kakacho = str_detect(pos_norm, "\u4fc2\u9577")
  )

wartime_start <- 1937
wartime_end   <- 1945
postwar_start <- 1947

# ============================================================
# 1. WARTIME DRAFT SHOCK AT POSITION x KAKARI x YEAR
# ============================================================

wartime_cells <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end) %>%
  group_by(office_id, ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_workers      = n(),
    n_male         = sum(!is_female, na.rm = TRUE),
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share    = ifelse(n_male > 0, n_drafted_male / n_male, 0),
    female_share   = mean(is_female, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 2. KAKARICHO'S WARTIME OFFICE (for linking)
# ============================================================

# Postwar kakaricho observations
kakaricho_postwar <- df %>%
  filter(is_kakacho, year_num >= postwar_start) %>%
  select(staff_id, office_id, ka, kakari, year_num) %>%
  distinct()

# Where each kakaricho worked during the war
kakaricho_wartime <- df %>%
  filter(year_num >= wartime_start, year_num <= wartime_end,
         staff_id %in% kakaricho_postwar$staff_id) %>%
  select(staff_id, wt_office = office_id, wt_ka = ka,
         wt_kakari = kakari, wt_pos = pos_norm, wt_year = year_num) %>%
  distinct()

# Aggregate wartime draft shock across all wartime cells for each kakaricho
kakaricho_wartime_agg <- kakaricho_wartime %>%
  left_join(wartime_cells,
            by = c("wt_office" = "office_id", "wt_ka" = "ka",
                   "wt_kakari" = "kakari", "wt_pos" = "pos_norm",
                   "wt_year" = "year_num")) %>%
  filter(!is.na(draft_share)) %>%
  filter(!is.na(wt_ka), !is.na(wt_pos)) %>%
  group_by(staff_id) %>%
  summarise(
    draft_shock_mean = mean(draft_share, na.rm = TRUE),
    draft_shock_max  = max(draft_share, na.rm = TRUE),
    # Keep modal wartime ka and position for FE
    wt_ka_modal  = names(sort(table(wt_ka), decreasing = TRUE))[1],
    wt_pos_modal = names(sort(table(wt_pos), decreasing = TRUE))[1],
    .groups = "drop"
  )

cat("Kakaricho with wartime draft shock data:", nrow(kakaricho_wartime_agg), "\n")

# ============================================================
# 3. POSTWAR KAKARI OUTCOMES
# ============================================================

kakari_outcome <- df %>%
  filter(year_num >= postwar_start) %>%
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    has_female    = as.integer(any(is_female, na.rm = TRUE)),
    n_female      = sum(is_female, na.rm = TRUE),
    female_share  = mean(is_female, na.rm = TRUE),
    kakari_size   = n(),
    .groups = "drop"
  )

# ============================================================
# 4. LINK: postwar kakari → kakaricho → wartime draft shock
# ============================================================

# For each postwar kakari-year, get the kakaricho's wartime draft shock
linked <- kakaricho_postwar %>%
  left_join(kakaricho_wartime_agg, by = "staff_id") %>%
  filter(!is.na(draft_shock_mean)) %>%
  # Average across kakaricho if multiple in same kakari
  group_by(office_id, ka, kakari, year_num) %>%
  summarise(
    draft_shock_mean = mean(draft_shock_mean, na.rm = TRUE),
    draft_shock_max  = mean(draft_shock_max, na.rm = TRUE),
    wt_ka            = first(wt_ka_modal),
    wt_pos           = first(wt_pos_modal),
    n_kakacho        = n(),
    .groups = "drop"
  )

panel <- kakari_outcome %>%
  inner_join(linked, by = c("office_id", "ka", "kakari", "year_num"))

cat("\n--- Panel diagnostics ---\n")
cat("Total postwar kakari-years with linked kakaricho:", nrow(panel), "\n")
cat("Unique wartime ka:", n_distinct(panel$wt_ka), "\n")
cat("Unique wartime pos:", n_distinct(panel$wt_pos), "\n")

# ============================================================
# 5. REDUCED-FORM REGRESSIONS
# ============================================================

# (1) has_female ~ draft_shock | wt_ka + wt_pos
rf1 <- feols(
  has_female ~ draft_shock_mean + kakari_size | wt_ka + wt_pos,
  data = panel, cluster = ~office_id
)

# (2) has_female ~ draft_shock | wt_ka + wt_pos + year
rf2 <- feols(
  has_female ~ draft_shock_mean + kakari_size | wt_ka + wt_pos + year_num,
  data = panel, cluster = ~office_id
)

# (3) n_female ~ draft_shock | wt_ka + wt_pos
rf3 <- feols(
  n_female ~ draft_shock_mean + kakari_size | wt_ka + wt_pos,
  data = panel, cluster = ~office_id
)

# (4) n_female ~ draft_shock | wt_ka + wt_pos + year
rf4 <- feols(
  n_female ~ draft_shock_mean + kakari_size | wt_ka + wt_pos + year_num,
  data = panel, cluster = ~office_id
)

# (5) female_share ~ draft_shock | wt_ka + wt_pos
rf5 <- feols(
  female_share ~ draft_shock_mean + kakari_size | wt_ka + wt_pos,
  data = panel, cluster = ~office_id
)

# (6) female_share ~ draft_shock | wt_ka + wt_pos + year
rf6 <- feols(
  female_share ~ draft_shock_mean + kakari_size | wt_ka + wt_pos + year_num,
  data = panel, cluster = ~office_id
)

cat("\n========== REDUCED FORM: POSTWAR FEMALES ~ WARTIME DRAFT SHOCK ==========\n")
etable(
  rf1, rf2, rf3, rf4, rf5, rf6,
  headers = c("Any F", "Any F", "N Female", "N Female", "F Share", "F Share"),
  se.below = TRUE, fitstat = ~n + r2
)

# ============================================================
# 6. EXPORT LaTeX TABLE
# ============================================================

extract_tabular <- function(tex_vec) {
  # tex_vec is already a character vector from etable(..., tex = TRUE)
  start <- grep("\\\\begin\\{tabular\\}", tex_vec)[1]
  end   <- grep("\\\\end\\{tabular\\}", tex_vec)
  end   <- end[end >= start][1]
  tex_vec[start:end]
}
clean_depvar <- function(tex_content) {
  drop_idx <- grep("Dependent Var", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  tex_content
}

dict <- c(
  draft_shock_mean = "Draft shock (kakaricho wartime)",
  kakari_size      = "Kakari size"
)

tex <- etable(
  rf1, rf2, rf3, rf4, rf5, rf6,
  dict = dict,
  order = c("Draft shock"),
  headers = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
  se.below = TRUE, fitstat = ~n + r2 + G, tex = TRUE
)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var header
midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_row <- " & \\multicolumn{2}{c}{Any female (=1)} & \\multicolumn{2}{c}{N females} & \\multicolumn{2}{c}{Female share} \\\\"
tc <- append(tc, c(depvar_row, "\\midrule"), after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Reduced Form: Wartime Draft Shock and Postwar Female Integration}",
  "\\label{tab:reduced-form-female}",
  "\\small",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item \\textit{Notes:} OLS regressions. Unit of observation: kakari $\\times$ year (1947 onward). ",
         "The sample includes postwar kakari-years whose \\emph{kakaricho} (subsection chief) can be linked to a wartime office. ",
         "The treatment variable is the mean share of males drafted from the kakaricho's wartime position $\\times$ kakari $\\times$ year cells. ",
         "Fixed effects condition on the kakaricho's wartime \\emph{ka} (section) and wartime position; ",
         "even-numbered columns add postwar year fixed effects. ",
         "Standard errors clustered at the office level in parentheses. ",
         "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("ReducedForm_FemaleExposure.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "ReducedForm_FemaleExposure.tex"))
cat("\nReduced-form table exported.\n")
