################################################################################
# NEW Table 3: Future Outcomes (through 1955)
# C1: Promotion (rank change) ~ draft_share
# C2: Promotion by gender (interaction)
# C3: Destination female share ~ draft_share
# C4: Destination female share by gender (interaction)
#
# Appendix: Retention (tenure_postwar) ~ draft_share, with interaction
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

# Enriched rank
assign_rank_enriched <- function(pos, yr) {
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

df <- df %>% mutate(rank_e = assign_rank_enriched(pos_norm, year_num))

# Z-score by era
df <- df %>%
  mutate(era = if_else(year_num < 1948, "pre", "post")) %>%
  group_by(era) %>%
  mutate(rank_z = (rank_e - mean(rank_e)) / sd(rank_e)) %>%
  ungroup()

# ============================================================
# 1. IDENTIFY 1944 NON-DRAFTED WORKERS
# ============================================================

workers_1944 <- df %>%
  filter(year_num == 1944, drafted != TRUE | is.na(drafted)) %>%
  distinct(staff_id, .keep_all = TRUE) %>%
  select(staff_id, office_id_1944 = office_id, pos_norm_1944 = pos_norm,
         kyoku_1944 = kyoku, ka_1944 = ka, is_female,
         rank_e_1944 = rank_e, rank_z_1944 = rank_z)

# Draft exposure
pos_draft_1944 <- df_all %>%
  filter(year_num == 1944) %>%
  group_by(office_id, pos_norm) %>%
  summarise(
    n_male_1944 = sum(!is_female, na.rm = TRUE),
    n_drafted_1944 = sum(drafted == TRUE & !is_female, na.rm = TRUE),
    draft_share = ifelse(n_male_1944 > 0, n_drafted_1944 / n_male_1944, 0),
    .groups = "drop"
  )

# Cumulative male baseline at office level (pre-1944)
cumul_male_stock <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop")

workers_1944 <- workers_1944 %>%
  left_join(pos_draft_1944, by = c("office_id_1944" = "office_id",
                                    "pos_norm_1944" = "pos_norm")) %>%
  left_join(cumul_male_stock, by = c("office_id_1944" = "office_id")) %>%
  mutate(
    draft_share = replace_na(draft_share, 0),
    cumul_n_male = replace_na(cumul_n_male, 0),
    ka_id_1944 = if_else(!is.na(ka_1944) & !is.na(kyoku_1944),
                          paste(kyoku_1944, ka_1944, sep = "_"), NA_character_)
  )

# ============================================================
# 2. POSTWAR OUTCOMES (extended to 1955)
# ============================================================
postwar_years <- 1946:1955

# C1/C2: Promotion (rank change)
postwar_rank <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(
    avg_rank_e = mean(rank_e), max_rank_e = max(rank_e),
    avg_rank_z = mean(rank_z), max_rank_z = max(rank_z),
    .groups = "drop"
  )

# Retention (years appearing postwar) -- for appendix
postwar_tenure <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(staff_id) %>%
  summarise(tenure_postwar = n_distinct(year_num), .groups = "drop")

# C3/C4: Destination female share
office_female_share <- df %>%
  filter(year_num %in% postwar_years) %>%
  group_by(office_id, pos_norm, year_num) %>%
  summarise(n_total = n(), n_female = sum(is_female),
            female_share = n_female / n_total, .groups = "drop")

worker_postwar <- df %>%
  filter(year_num %in% postwar_years) %>%
  select(staff_id, office_id, pos_norm, year_num) %>%
  left_join(office_female_share, by = c("office_id", "pos_norm", "year_num"))

avg_dest <- worker_postwar %>%
  group_by(staff_id) %>%
  summarise(avg_female_share_dest = mean(female_share, na.rm = TRUE), .groups = "drop")

# ============================================================
# 3. ASSEMBLE
# ============================================================

indiv <- workers_1944 %>%
  left_join(postwar_rank, by = "staff_id") %>%
  left_join(postwar_tenure, by = "staff_id") %>%
  left_join(avg_dest, by = "staff_id") %>%
  mutate(
    tenure_postwar = replace_na(tenure_postwar, 0),
    rank_z_change = avg_rank_z - rank_z_1944,
    rank_e_change = avg_rank_e - rank_e_1944
  )

indiv_ka <- indiv %>% filter(!is.na(ka_id_1944))
indiv_ka_pw <- indiv_ka %>% filter(!is.na(avg_rank_e))  # appeared postwar

cat("Sample: all 1944 non-drafted:", nrow(indiv), "\n")
cat("With ka FE:", nrow(indiv_ka), "\n")
cat("Appearing postwar:", nrow(indiv_ka_pw), "\n")
cat("Female in postwar sample:", sum(indiv_ka_pw$is_female), "\n\n")

# ============================================================
# 4. REGRESSIONS -- MAIN TABLE
# ============================================================

# --- C1: Promotion ---
c1 <- feols(rank_z_change ~ draft_share + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka_pw, cluster = ~office_id_1944)

# --- C2: Promotion by gender ---
c2 <- feols(rank_z_change ~ draft_share * is_female + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka_pw, cluster = ~office_id_1944)

# --- C3: Destination gender balance ---
indiv_ka_dest <- indiv_ka %>% filter(!is.na(avg_female_share_dest))

c3 <- feols(avg_female_share_dest ~ draft_share + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka_dest, cluster = ~office_id_1944)

# --- C4: Destination gender balance by gender ---
c4 <- feols(avg_female_share_dest ~ draft_share * is_female + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka_dest, cluster = ~office_id_1944)

# ============================================================
# 5. REGRESSIONS -- APPENDIX (Retention)
# ============================================================

r1 <- feols(tenure_postwar ~ draft_share + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka, cluster = ~office_id_1944)

r2 <- feols(tenure_postwar ~ draft_share * is_female + log(cumul_n_male + 1) | ka_id_1944 + pos_norm_1944,
            data = indiv_ka, cluster = ~office_id_1944)

# ============================================================
# PRINT
# ============================================================

cat("===== TABLE 3: FUTURE OUTCOMES (Main) =====\n\n")
etable(c1, c2, c3, c4,
       headers = c("Promotion", "Promotion\nx Gender",
                    "Dest F.Share", "Dest F.Share\nx Gender"),
       drop = "log",
       se.below = TRUE, fitstat = ~n + r2)

cat("\n===== APPENDIX: RETENTION =====\n\n")
etable(r1, r2,
       headers = c("Retention", "Retention\nx Gender"),
       drop = "log",
       se.below = TRUE, fitstat = ~n + r2)

# ============================================================
# EXPORT LaTeX -- MAIN TABLE
# ============================================================

dict <- c(
  draft_share = "Draft share",
  is_femaleTRUE = "Female (=1)",
  "draft_share:is_femaleTRUE" = "Draft share $\\times$ Female"
)

tex <- etable(c1, c2, c3, c4,
              dict = dict,
              drop = "log",
              headers = c("Promotion", "Promotion", "Dest. F.Share", "Dest. F.Share"),
              se.below = TRUE, fitstat = ~n + r2,
              tex = TRUE)

tex_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Postwar Outcomes of 1944 Colleagues (1946--1955)}",
  "\\label{tab:future}",
  "\\small",
  "\\begin{threeparttable}",
  tex,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} OLS regressions. Sample: non-drafted workers observed in 1944. Columns (1)--(2): dependent variable is z-scored rank change (postwar avg.\\ minus 1944 rank, standardized within era). Column (2) includes gender interaction. Columns (3)--(4): dependent variable is average female share of the worker's office-position across postwar years. Column (4) includes gender interaction. All specifications include ka and position fixed effects from 1944 and control for log cumulative male stock. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_out, here("NewTable3_FutureOutcomes.tex"))
writeLines(tex_out, here("..", "Tokyo_Project", "Tables_Figures", "NewTable3_FutureOutcomes.tex"))
cat("\nMain table exported to NewTable3_FutureOutcomes.tex\n")

# ============================================================
# EXPORT LaTeX -- APPENDIX (Retention)
# ============================================================

tex_ret <- etable(r1, r2,
                  dict = dict,
                  drop = "log",
                  headers = c("Retention", "Retention"),
                  se.below = TRUE, fitstat = ~n + r2,
                  tex = TRUE)

tex_ret_out <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Postwar Retention of 1944 Colleagues (1946--1955)}",
  "\\label{tab:retention_appendix}",
  "\\small",
  "\\begin{threeparttable}",
  tex_ret,
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Notes:} OLS regressions. Sample: non-drafted workers observed in 1944. Dependent variable is count of postwar years (1946--1955) appearing in civil service. Column (2) includes gender interaction. All specifications include ka and position fixed effects from 1944 and control for log cumulative male stock. Standard errors clustered at the office level. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_ret_out, here("..", "Tokyo_Project", "Appendix", "AppendixTable_Retention.tex"))
cat("Appendix retention table exported to AppendixTable_Retention.tex\n")
