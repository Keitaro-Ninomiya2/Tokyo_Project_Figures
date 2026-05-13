################################################################################
# Post-War Retention IV: Effect of Wartime Hiring on Post-War Retention
#
# IV Strategy: At the office level, use draft intensity as an instrument
# for new hires during the war. Then regress post-war retention/promotion
# of wartime hires on the instrumented hiring.
#
# Question: Do workers hired during wartime drafting stay in the civil
# service after the war? Are they promoted? Run separately for men and women.
# For women, focus on low-class admin (rank 1-2) jobs.
#
# Unit: office (ka) x position level
# First stage: n_new_hires ~ n_drafted_male (+ controls, FE)
# Second stage: postwar_share_retained ~ n_new_hires_hat
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

# ============================================================
# KYOKU NORMALIZATION
# ============================================================

normalize_kyoku <- function(k) {
  case_when(
    is.na(k) ~ NA_character_,
    str_detect(k, "業所長.*健民|家所長.*健民") ~ "健民局",
    str_detect(k, "業所長.*厚生")              ~ "厚生局",
    str_detect(k, "東京市主事.*厚生")          ~ "厚生局",
    str_detect(k, "清掃監督.*厚生")            ~ "厚生局",
    str_detect(k, "社会")       ~ "社会局",
    str_detect(k, "保健")       ~ "保健局",
    str_detect(k, "上木")       ~ "土木局",
    str_detect(k, "土木")       ~ "土木局",
    str_detect(k, "市会事務|Y事務") ~ "市会事務局",
    str_detect(k, "府会事務")   ~ "府会事務局",
    str_detect(k, "發育")       ~ "教育局",
    str_detect(k, "教育")       ~ "教育局",
    str_detect(k, "水違")       ~ "水道局",
    str_detect(k, "水道")       ~ "水道局",
    str_detect(k, "電気")       ~ "電気局",
    str_detect(k, "養育|沼務所") ~ "養育院",
    str_detect(k, "財務")       ~ "財務局",
    str_detect(k, "繰済|發経済|《経済|学校体育課経済") ~ "経済局",
    str_detect(k, "経済")       ~ "経済局",
    str_detect(k, "厚生")       ~ "厚生局",
    str_detect(k, "市民")       ~ "市民局",
    str_detect(k, "中央卸売")   ~ "中央卸売市場",
    str_detect(k, "港準")       ~ "港湾局",
    str_detect(k, "港湾")       ~ "港湾局",
    str_detect(k, "監査")       ~ "監査局",
    str_detect(k, "産業")       ~ "産業局",
    str_detect(k, "城東病院")   ~ "健民局",
    str_detect(k, "健民")       ~ "健民局",
    str_detect(k, "戰時生活")   ~ "戰時生活局",
    str_detect(k, "経〓")       ~ "経理局",
    str_detect(k, "経理")       ~ "経理局",
    str_detect(k, "労働")       ~ "労働局",
    str_detect(k, "建築|御築")  ~ "建築局",
    str_detect(k, "道路")       ~ "道路局",
    str_detect(k, "復興|伊興事業") ~ "復興事業局",
    str_detect(k, "交通")       ~ "交通局",
    str_detect(k, "計[畫画]")   ~ "計画局",
    str_detect(k, "民局")       ~ "健民局",
    str_detect(k, "後醉院")     ~ "電気局",
    str_detect(k, "築地産院|荒産院") ~ "健民局",
    str_detect(k, "防衛")       ~ "防衛局",
    str_detect(k, "民生")       ~ "民生局",
    str_detect(k, "長官官房")   ~ "長官官房",
    TRUE ~ NA_character_
  )
}

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

df <- df %>%
  mutate(
    norm_kyoku = normalize_kyoku(kyoku),
    pos_rank   = assign_rank(pos_norm, year_num)
  )

df_all <- df_all %>%
  mutate(
    norm_kyoku = normalize_kyoku(kyoku),
    pos_rank   = assign_rank(pos_norm, year_num)
  )

# ============================================================
# 1. IDENTIFY WARTIME NEW HIRES AND THEIR POSTWAR OUTCOMES
# ============================================================

wartime_years <- 1940:1945  # peak drafting period
postwar_years <- 1947:1955

# First year each worker appears
first_year <- df %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num), .groups = "drop")

# Wartime new hires: workers whose first year is during wartime
wartime_hires <- df %>%
  left_join(first_year, by = "staff_id") %>%
  filter(first_year %in% wartime_years) %>%
  # Take their first observation (entry office)
  group_by(staff_id) %>%
  slice_min(year_num, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("Wartime new hires:", n_distinct(wartime_hires$staff_id), "\n")
cat("  Female:", sum(wartime_hires$is_female), "\n")
cat("  Male:", sum(!wartime_hires$is_female), "\n")

# Postwar outcomes for wartime hires
postwar_obs <- df %>%
  filter(year_num %in% postwar_years)

hire_postwar <- wartime_hires %>%
  select(staff_id, entry_office = office_id, entry_ka = ka,
         entry_pos = pos_norm, entry_rank = pos_rank,
         entry_year = year_num, is_female, norm_kyoku) %>%
  left_join(
    postwar_obs %>%
      group_by(staff_id) %>%
      summarise(
        postwar_years_observed = n_distinct(year_num),
        postwar_max_rank = max(pos_rank, na.rm = TRUE),
        postwar_last_year = max(year_num),
        .groups = "drop"
      ),
    by = "staff_id"
  ) %>%
  mutate(
    retained_postwar = as.integer(!is.na(postwar_years_observed) & postwar_years_observed > 0),
    postwar_years_observed = replace_na(postwar_years_observed, 0),
    promoted_postwar = as.integer(!is.na(postwar_max_rank) & postwar_max_rank > entry_rank)
  )

cat("\nPostwar retention among wartime hires:\n")
cat("  Retained:", sum(hire_postwar$retained_postwar), "\n")
cat("  Not retained:", sum(!hire_postwar$retained_postwar), "\n")
cat("  Retention rate:", round(mean(hire_postwar$retained_postwar), 3), "\n")
cat("  Female retention:", round(mean(hire_postwar$retained_postwar[hire_postwar$is_female]), 3), "\n")
cat("  Male retention:", round(mean(hire_postwar$retained_postwar[!hire_postwar$is_female]), 3), "\n")

# ============================================================
# 2. AGGREGATE TO OFFICE LEVEL (ka x entry_year)
# ============================================================

# Draft intensity at office level
office_drafts <- df_all %>%
  filter(year_num %in% wartime_years) %>%
  group_by(office_id, ka, year_num) %>%
  summarise(
    n_drafted_male = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    n_male_total   = sum(!is_female, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate wartime hires and their postwar outcomes by office
office_hires <- hire_postwar %>%
  group_by(entry_office, entry_ka, norm_kyoku, entry_year) %>%
  summarise(
    n_new_hires        = n(),
    n_new_hires_female = sum(is_female),
    n_new_hires_male   = sum(!is_female),
    n_retained_postwar = sum(retained_postwar),
    n_promoted_postwar = sum(promoted_postwar),
    share_retained     = mean(retained_postwar),
    share_promoted     = mean(promoted_postwar),
    # Female-specific
    n_retained_female  = sum(retained_postwar & is_female),
    n_promoted_female  = sum(promoted_postwar & is_female),
    share_retained_f   = ifelse(sum(is_female) > 0,
                                mean(retained_postwar[is_female]), NA_real_),
    share_promoted_f   = ifelse(sum(is_female) > 0,
                                mean(promoted_postwar[is_female]), NA_real_),
    # Male-specific
    n_retained_male    = sum(retained_postwar & !is_female),
    n_promoted_male    = sum(promoted_postwar & !is_female),
    share_retained_m   = ifelse(sum(!is_female) > 0,
                                mean(retained_postwar[!is_female]), NA_real_),
    share_promoted_m   = ifelse(sum(!is_female) > 0,
                                mean(promoted_postwar[!is_female]), NA_real_),
    .groups = "drop"
  ) %>%
  rename(office_id = entry_office, ka = entry_ka, year_num = entry_year)

# Merge draft intensity
office_panel <- office_hires %>%
  left_join(office_drafts, by = c("office_id", "ka", "year_num")) %>%
  mutate(
    n_drafted_male = replace_na(n_drafted_male, 0),
    n_male_total = replace_na(n_male_total, 0),
    ka_id = paste(norm_kyoku, ka, sep = "_"),
    log_n_male = log(n_male_total + 1)
  ) %>%
  filter(!is.na(ka_id), !is.na(norm_kyoku))

cat("\nOffice panel:", nrow(office_panel), "office-years\n")
cat("With any drafts:", sum(office_panel$n_drafted_male > 0), "\n")
cat("Mean new hires:", round(mean(office_panel$n_new_hires), 2), "\n")
cat("Mean share retained:", round(mean(office_panel$share_retained, na.rm = TRUE), 3), "\n")

# ============================================================
# 3. FIRST STAGE: Draft intensity → New hires
# ============================================================

cat("\n===== FIRST STAGE: Drafts → New Hires =====\n")

fs_all <- feols(n_new_hires ~ n_drafted_male + log_n_male |
                  year_num + norm_kyoku,
                data = office_panel, cluster = ~office_id)

fs_female <- feols(n_new_hires_female ~ n_drafted_male + log_n_male |
                     year_num + norm_kyoku,
                   data = office_panel, cluster = ~office_id)

fs_male <- feols(n_new_hires_male ~ n_drafted_male + log_n_male |
                   year_num + norm_kyoku,
                 data = office_panel, cluster = ~office_id)

etable(fs_all, fs_female, fs_male,
       se.below = TRUE, fitstat = ~n + r2 + wald,
       headers = c("All Hires", "Female Hires", "Male Hires"))

# ============================================================
# 4. REDUCED FORM: Draft intensity → Postwar retention/promotion
# ============================================================

cat("\n===== REDUCED FORM: Drafts → Postwar Retention =====\n")

# All workers
rf1 <- feols(share_retained ~ n_drafted_male + log_n_male |
               year_num + norm_kyoku,
             data = office_panel, cluster = ~office_id)

rf2 <- feols(share_promoted ~ n_drafted_male + log_n_male |
               year_num + norm_kyoku,
             data = office_panel, cluster = ~office_id)

rf3 <- feols(n_retained_postwar ~ n_drafted_male + log_n_male |
               year_num + norm_kyoku,
             data = office_panel, cluster = ~office_id)

rf4 <- feols(n_promoted_postwar ~ n_drafted_male + log_n_male |
               year_num + norm_kyoku,
             data = office_panel, cluster = ~office_id)

cat("\n--- All Workers ---\n")
etable(rf1, rf2, rf3, rf4,
       se.below = TRUE, fitstat = ~n + r2,
       headers = c("Share Retained", "Share Promoted", "N Retained", "N Promoted"))

# ============================================================
# 5. IV: Instrumented hiring → Postwar retention
# ============================================================

cat("\n===== IV: New Hires (instrumented) → Postwar Retention =====\n")

# All workers
iv_ret <- feols(n_retained_postwar ~ log_n_male |
                  year_num + norm_kyoku |
                  n_new_hires ~ n_drafted_male,
                data = office_panel, cluster = ~office_id)

iv_prom <- feols(n_promoted_postwar ~ log_n_male |
                   year_num + norm_kyoku |
                   n_new_hires ~ n_drafted_male,
                 data = office_panel, cluster = ~office_id)

cat("\n--- IV: All Workers ---\n")
etable(iv_ret, iv_prom,
       se.below = TRUE, fitstat = ~n + ivf + ivwald,
       headers = c("N Retained (IV)", "N Promoted (IV)"))

# ============================================================
# 6. GENDER-SPECIFIC: MALE HIRES
# ============================================================

cat("\n===== MALE HIRES =====\n")

rf_ret_m <- feols(share_retained_m ~ n_drafted_male + log_n_male |
                    year_num + norm_kyoku,
                  data = office_panel %>% filter(!is.na(share_retained_m)),
                  cluster = ~office_id)

rf_prom_m <- feols(share_promoted_m ~ n_drafted_male + log_n_male |
                     year_num + norm_kyoku,
                   data = office_panel %>% filter(!is.na(share_promoted_m)),
                   cluster = ~office_id)

iv_ret_m <- feols(n_retained_male ~ log_n_male |
                    year_num + norm_kyoku |
                    n_new_hires_male ~ n_drafted_male,
                  data = office_panel, cluster = ~office_id)

iv_prom_m <- feols(n_promoted_male ~ log_n_male |
                     year_num + norm_kyoku |
                     n_new_hires_male ~ n_drafted_male,
                   data = office_panel, cluster = ~office_id)

etable(rf_ret_m, rf_prom_m, iv_ret_m, iv_prom_m,
       se.below = TRUE, fitstat = ~n + r2 + ivf,
       headers = c("Share Ret (RF)", "Share Prom (RF)", "N Ret (IV)", "N Prom (IV)"))

# ============================================================
# 7. GENDER-SPECIFIC: FEMALE HIRES (focus on low-class admin, rank 1-2)
# ============================================================

cat("\n===== FEMALE HIRES (Low-class admin) =====\n")

# For female analysis, restrict to offices that had female hires in low-rank positions
female_hires_lowrank <- hire_postwar %>%
  filter(is_female, entry_rank %in% c(1, 2))

cat("Female low-rank hires:", nrow(female_hires_lowrank), "\n")

office_female_lr <- female_hires_lowrank %>%
  group_by(entry_office, entry_ka, norm_kyoku, entry_year) %>%
  summarise(
    n_female_hires_lr = n(),
    n_retained_f_lr   = sum(retained_postwar),
    n_promoted_f_lr   = sum(promoted_postwar),
    share_retained_f_lr = mean(retained_postwar),
    share_promoted_f_lr = mean(promoted_postwar),
    .groups = "drop"
  ) %>%
  rename(office_id = entry_office, ka = entry_ka, year_num = entry_year)

office_panel_f <- office_panel %>%
  left_join(office_female_lr, by = c("office_id", "ka", "norm_kyoku", "year_num")) %>%
  mutate(
    n_female_hires_lr = replace_na(n_female_hires_lr, 0),
    n_retained_f_lr = replace_na(n_retained_f_lr, 0),
    n_promoted_f_lr = replace_na(n_promoted_f_lr, 0)
  )

# First stage for female low-rank hires
fs_f_lr <- feols(n_female_hires_lr ~ n_drafted_male + log_n_male |
                   year_num + norm_kyoku,
                 data = office_panel_f, cluster = ~office_id)

# Reduced form
rf_ret_f <- feols(share_retained_f_lr ~ n_drafted_male + log_n_male |
                    year_num + norm_kyoku,
                  data = office_panel_f %>% filter(!is.na(share_retained_f_lr)),
                  cluster = ~office_id)

rf_prom_f <- feols(share_promoted_f_lr ~ n_drafted_male + log_n_male |
                     year_num + norm_kyoku,
                   data = office_panel_f %>% filter(!is.na(share_promoted_f_lr)),
                   cluster = ~office_id)

# IV
iv_ret_f <- feols(n_retained_f_lr ~ log_n_male |
                    year_num + norm_kyoku |
                    n_female_hires_lr ~ n_drafted_male,
                  data = office_panel_f, cluster = ~office_id)

iv_prom_f <- feols(n_promoted_f_lr ~ log_n_male |
                     year_num + norm_kyoku |
                     n_female_hires_lr ~ n_drafted_male,
                   data = office_panel_f, cluster = ~office_id)

cat("\nFirst stage (female low-rank):\n")
etable(fs_f_lr, se.below = TRUE, fitstat = ~n + r2 + wald)

cat("\nReduced form + IV (female low-rank):\n")
etable(rf_ret_f, rf_prom_f, iv_ret_f, iv_prom_f,
       se.below = TRUE, fitstat = ~n + r2 + ivf,
       headers = c("Share Ret (RF)", "Share Prom (RF)", "N Ret (IV)", "N Prom (IV)"))

# ============================================================
# 8. EXPORT LaTeX TABLE
# ============================================================

dict <- c(
  n_drafted_male         = "No. males drafted",
  n_new_hires            = "No. new hires",
  n_new_hires_male       = "No. male new hires",
  n_female_hires_lr      = "No. female new hires (low-rank)",
  fit_n_new_hires        = "No. new hires",
  fit_n_new_hires_male   = "No. male new hires",
  fit_n_female_hires_lr  = "No. female new hires (low-rank)"
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
  drop_idx <- grep("Dependent Var", tex_content)
  if (length(drop_idx) > 0) tex_content <- tex_content[-drop_idx]
  tex_content
}

# Panel A: First Stage
tex_fs <- etable(fs_all, fs_male, fs_f_lr,
                 dict = dict,
                 drop = "log",
                 headers = c("All Hires", "Male Hires", "Female (Low-Rank)"),
                 se.below = TRUE, fitstat = ~n + wald,
                 tex = TRUE)
tc_fs <- clean_depvar(extract_tabular(tex_fs))

# Panel B: Reduced Form (All, Male, Female)
tex_rf <- etable(rf1, rf2, rf_ret_m, rf_prom_m, rf_ret_f, rf_prom_f,
                 dict = dict,
                 drop = "log",
                 headers = c("\\makecell{Ret.\\\\All}", "\\makecell{Prom.\\\\All}",
                             "\\makecell{Ret.\\\\Male}", "\\makecell{Prom.\\\\Male}",
                             "\\makecell{Ret.\\\\Female}", "\\makecell{Prom.\\\\Female}"),
                 se.below = TRUE, fitstat = ~n,
                 tex = TRUE)
tc_rf <- clean_depvar(extract_tabular(tex_rf))

# Panel C: IV
tex_iv <- etable(iv_ret, iv_prom, iv_ret_m, iv_prom_m, iv_ret_f, iv_prom_f,
                 dict = dict,
                 drop = "log",
                 headers = c("\\makecell{Ret.\\\\All}", "\\makecell{Prom.\\\\All}",
                             "\\makecell{Ret.\\\\Male}", "\\makecell{Prom.\\\\Male}",
                             "\\makecell{Ret.\\\\Female}", "\\makecell{Prom.\\\\Female}"),
                 se.below = TRUE, fitstat = ~n + ivf,
                 tex = TRUE)
tc_iv <- clean_depvar(extract_tabular(tex_iv))

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Post-War Retention of Wartime Hires: IV Estimates}",
  "\\label{tab:postwar-iv}",
  "\\small",
  "\\begin{threeparttable}",
  "",
  "\\textbf{Panel A: First Stage (Dep.\\ var: No.\\ new hires)}",
  "\\medskip",
  "",
  tc_fs,
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel B: Reduced Form (Dep.\\ var: Share retained/promoted post-war)}",
  "\\medskip",
  "",
  tc_rf,
  "",
  "\\bigskip",
  "",
  "\\textbf{Panel C: IV (Dep.\\ var: No.\\ retained/promoted post-war)}",
  "\\medskip",
  "",
  tc_iv,
  "",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Panel~A reports the first-stage relationship between ",
    "office-level military draft intensity and new hiring (1940--1945). ",
    "Panel~B reports reduced-form estimates of draft intensity on the share of ",
    "wartime hires who are retained or promoted in the post-war period (1947--1955). ",
    "Panel~C reports IV estimates instrumenting new hiring with draft intensity. ",
    "``All'' pools both genders; ``Male'' restricts to male hires; ``Female'' restricts ",
    "to female hires in low-rank administrative positions (rank~1--2). ",
    "All specifications include entry-year and kyoku fixed effects and control for ",
    "log male baseline (not shown). Standard errors clustered at the office level. ",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("PostwarRetentionIV.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "PostwarRetentionIV.tex"))
cat("\nTable exported to PostwarRetentionIV.tex\n")
