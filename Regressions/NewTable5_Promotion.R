################################################################################
# Table 5: Short-Run Promotion in Response to Military Drafting
# Did drafting cause remaining workers in the same office to get promoted?
# Unit: individual worker x year
# Sample: non-drafted workers present in year t-1 AND t, same office
# Outcome: promoted (=1 if rank increased from t-1 to t)
# Treatment: total male workers drafted from same OFFICE in year t
#            (office-level, not position-level -- drafts create upward vacancies)
#
# Columns: Baseline | x Gender | x Rank | x Engineer
# All columns use full sample with interactions (no split samples).
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

df_all <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(year_num = as.numeric(year), is_female = gender_modern == "female",
         pos_norm = str_replace_all(position, "\\s+", "")) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

# Rank assignment (3-level: 1 = yatoi/shokutaku, 2 = regular, 3 = shiji/gishi)
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

df <- df %>% mutate(pos_rank = assign_rank(pos_norm, year_num))

# Identify years with any draft info
draft_years <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(year_num) %>%
  pull(year_num) %>%
  sort()
cat("Years with draft info:", paste(draft_years, collapse = ", "), "\n")

# Exclude anyone ever drafted
drafted_ids <- df_all %>%
  filter(drafted == TRUE) %>%
  distinct(staff_id) %>%
  pull(staff_id)

# ============================================================
# 1. BUILD PROMOTION PANEL
# ============================================================

# Draft counts at OFFICE level per year (not office x position)
office_drafts <- df_all %>%
  filter(year_num %in% draft_years) %>%
  group_by(office_id, year_num) %>%
  summarise(n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
            .groups = "drop")

# Cumulative male baseline at office level
cumul_male_stock <- map_dfr(draft_years, function(yr) {
  df %>% filter(year_num < yr, !is_female) %>%
    group_by(office_id) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

# Check consecutive year overlap
cat("\nConsecutive year overlaps at same office:\n")
for (t in draft_years) {
  prev <- df %>% filter(year_num == t - 1, !(staff_id %in% drafted_ids))
  curr <- df %>% filter(year_num == t) %>% select(staff_id, office_id_t = office_id)
  matched <- prev %>% inner_join(curr, by = "staff_id") %>% filter(office_id == office_id_t)
  cat(sprintf("  %d->%d: %d workers\n", t-1, t, nrow(matched)))
}

# For each draft year t: find workers in same office in t-1 and t
promotion_rows <- list()
for (t in draft_years) {
  workers_prev <- df %>%
    filter(year_num == t - 1, !(staff_id %in% drafted_ids)) %>%
    select(staff_id, office_id, pos_norm, pos_rank, is_female, ka, kyoku)

  workers_curr <- df %>%
    filter(year_num == t) %>%
    select(staff_id, office_id_t = office_id, pos_norm_t = pos_norm,
           pos_rank_t = pos_rank)

  matched <- workers_prev %>%
    inner_join(workers_curr, by = "staff_id") %>%
    filter(office_id == office_id_t) %>%
    mutate(
      promoted = as.integer(pos_rank_t > pos_rank),
      rank_change = pos_rank_t - pos_rank,
      pos_changed = as.integer(pos_norm_t != pos_norm),
      draft_year = t
    )

  promotion_rows[[as.character(t)]] <- matched
}

promo_panel <- bind_rows(promotion_rows) %>%
  left_join(office_drafts, by = c("office_id", "draft_year" = "year_num")) %>%
  left_join(cumul_male_stock, by = c("office_id", "draft_year" = "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    year_num = draft_year
  )

cat("\nFull panel:", nrow(promo_panel), "obs\n")
cat("With any drafts at office:", sum(promo_panel$n_drafted_male > 0), "\n")

panel_ka <- promo_panel %>%
  filter(!is.na(ka_id)) %>%
  mutate(
    is_rank1   = as.integer(pos_rank == 1),
    is_rank3   = as.integer(pos_rank == 3),
    is_engineer = as.integer(str_detect(pos_norm, "技"))
  )

cat("With ka_id:", nrow(panel_ka), "obs\n")
cat("N female:", sum(panel_ka$is_female), " N male:", sum(!panel_ka$is_female), "\n")
cat("With drafts:", sum(panel_ka$n_drafted_male > 0), "\n")
cat("Mean promoted:", round(mean(panel_ka$promoted), 4), "\n")
cat("Rank distribution: rank1 =", sum(panel_ka$is_rank1),
    " rank2 =", sum(panel_ka$pos_rank == 2),
    " rank3 =", sum(panel_ka$is_rank3), "\n")
cat("N engineer:", sum(panel_ka$is_engineer), "\n")

# ============================================================
# 2. REGRESSIONS
# ============================================================

cat("\n===== TABLE 5: PROMOTION (INTERACTION DESIGN) =====\n\n")

# C1: Baseline
m1 <- feols(promoted ~ n_drafted_male + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C2: x Gender
m2 <- feols(promoted ~ n_drafted_male * is_female + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C3: x Rank (rank 2 = base; rank main effects absorbed by pos_norm FE)
m3 <- feols(promoted ~ n_drafted_male + n_drafted_male:is_rank1 +
              n_drafted_male:is_rank3 + log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

# C4: x Engineer (engineer main effect absorbed by pos_norm FE)
m4 <- feols(promoted ~ n_drafted_male + n_drafted_male:is_engineer +
              log(cumul_n_male + 1) |
              year_num + ka_id + pos_norm,
            data = panel_ka, cluster = ~office_id)

etable(m1, m2, m3, m4,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Baseline", "x Gender", "x Rank", "x Engineer"))

# ============================================================
# 3. EXPORT LaTeX
# ============================================================

dict <- c(
  n_drafted_male              = "No. drafted workers (office)",
  "is_femaleTRUE"             = "Female",
  "n_drafted_male:is_femaleTRUE" = "No. drafted $\\times$ Female",
  "n_drafted_male:is_rank1"   = "No. drafted $\\times$ Rank 1",
  "n_drafted_male:is_rank3"   = "No. drafted $\\times$ Rank 3",
  "n_drafted_male:is_engineer" = "No. drafted $\\times$ Engineer",
  ka_id = "Ka"
)

extract_tabular <- function(tex_raw) {
  tex_str <- paste(tex_raw, collapse = "\n")
  m_start <- regexpr("\\\\begin\\{tabular\\}", tex_str)
  m_end <- regexpr("\\\\end\\{tabular\\}", tex_str)
  if (m_start > 0 && m_end > 0) {
    end_len <- attr(m_end, "match.length")
    strsplit(substr(tex_str, m_start, m_end + end_len - 1), "\n")[[1]]
  } else {
    tex_raw
  }
}

clean_depvar <- function(tex_content) {
  drop_idx <- grep("\\bpromoted\\b", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  drop_idx2 <- grep("Dependent Var", tex_content)
  if (length(drop_idx2) > 0) tex_content <- tex_content[-drop_idx2]
  tex_content
}

tex <- etable(m1, m2, m3, m4,
              dict = dict,
              order = c("No. drafted w", "No. drafted.*Female",
                        "^Female", "No. drafted.*Rank", "No. drafted.*Engineer"),
              drop = "log",
              headers = c("Baseline", "$\\times$ Gender",
                          "$\\times$ Rank", "$\\times$ Engineer"),
              se.below = TRUE, fitstat = ~n,
              tex = TRUE)

tc <- clean_depvar(extract_tabular(tex))

# Insert dep-var row after the first \midrule
midrule_idx <- grep("\\\\midrule", tc)[1]
depvar_line <- "\\multicolumn{5}{l}{\\textit{Dep.\\ var: Promoted (=1 if rank increased from $t{-}1$ to $t$)}} \\\\"
tc <- append(tc, depvar_line, after = midrule_idx)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Short-Run Promotion in Response to Drafting}",
  "\\label{tab:promotion}",
  "\\begin{threeparttable}",
  tc,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} OLS regressions (linear probability model). ",
    "Dependent variable: promoted (=1 if worker's position rank increased ",
    "from year $t{-}1$ to $t$). Sample: non-drafted workers present in the ",
    "same office in consecutive years. Treatment: total number of male workers ",
    "drafted from the same office in year $t$ (office-level variation). ",
    "Column~2 interacts treatment with a female indicator; Column~3 interacts ",
    "with rank indicators (rank~2 is the omitted category; rank~1 = temporary ",
    "\\emph{yatoi/shokutaku}, rank~3 = supervisory \\emph{shiji/gishi}); ",
    "Column~4 interacts with an engineer indicator (positions containing ",
    "\\begin{CJK}{UTF8}{min}技\\end{CJK}). ",
    "Rank and engineer main effects are absorbed by position fixed effects. ",
    "All specifications include year, ka, and position fixed effects, and ",
    "control for log cumulative male baseline (not shown). ",
    "Standard errors clustered at the office level in parentheses. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable5_Promotion.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable5_Promotion.tex"))
cat("\nTable 5 exported to NewTable5_Promotion.tex and Tokyo_Project\n")
