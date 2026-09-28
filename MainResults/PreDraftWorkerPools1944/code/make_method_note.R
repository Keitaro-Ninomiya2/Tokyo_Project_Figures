suppressPackageStartupMessages({
  library(tidyverse)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
base <- file.path(root, "MainResults", "PreDraftWorkerPools1944")
tab <- file.path(base, "tables")

diag <- read_csv(file.path(tab, "diagnostics.csv"), show_col_types = FALSE) %>%
  deframe()
stats <- read_csv(file.path(tab, "summary_statistics.csv"), show_col_types = FALSE) %>%
  deframe()
mapping <- read_csv(file.path(tab, "position_rank_track_map_1944.csv"), show_col_types = FALSE)

reg_files <- file.path(tab, c("regression_lateral.csv", "regression_promotion.csv", "regression_new_hires.csv"))
regs <- map_dfr(reg_files, ~read_csv(.x, show_col_types = FALSE))
ppml <- regs %>%
  filter(specification == "robust_b_ppml") %>%
  group_by(outcome) %>%
  summarise(
    observations = first(observations),
    status = if_else(any(term == "<model failed>"), "failed", "estimated"),
    .groups = "drop"
  )

fmt <- function(x, digits = 3) formatC(as.numeric(x), digits = digits, format = "f", big.mark = ",")
pct <- function(x) paste0(fmt(100 * as.numeric(x), 1), "%")

main <- regs %>% filter(specification == "main_exact_match")
get_row <- function(target_outcome, target_term) {
  main %>% filter(.data$outcome == target_outcome, .data$term == target_term) %>% slice(1)
}
r1 <- get_row("lateral_other_kyoku", "Draft:log_lat_same_ka")
r2 <- get_row("lateral_other_kyoku", "Draft:logStaff")
r3 <- get_row("promotion_same_kyoku", "Draft:log_pro_same_kyoku")
r4 <- get_row("promotion_other_kyoku", "Draft:log_pro_own_kakari")
r5 <- get_row("new_hires", "Draft:log_pro_own_kakari")
summary_line <- function(label, row) {
  sign <- if_else(row$estimate >= 0, "positive", "negative")
  paste0(label, " is ", sign, ": ", fmt(row$estimate, 3),
         " (clustered p=", fmt(row$p_value, 3),
         ", wild p=", fmt(row$wild_p_value, 3), ").")
}

lines <- c(
  "# Method note: pre-draft worker pools and 1944 replacements",
  "",
  "## Data and variables",
  "",
  "I use the linked `Tokyo_Personnel_Master_All_Years.csv` worker-year panel. I use `staff_id` for person, `year` for year, `kyoku`, `ka`, and `kakari` for hierarchy, `office_id` for office, whitespace-normalized `position` for occupation, `gender_modern` for gender, and the Boolean `drafted` for military service. I rename the original `rank` field to `court_rank` and never use it as employment rank. I retain one row per `staff_id`-year after filtering to named rows.",
  "",
  "I use the pre-1948 Table 1 position-rank rule in `Regressions/NewTable1_Reallocation.R` (the `assign_rank` function near lines 124-131): exact `主事` and `技師` are rank 3; exact `雇` and `囑託` are rank 1; all other titles are rank 2. This rule assigns every 1944 worker, so the unassigned-rank worker count is 0. I classify titles containing `技` as technical. I classify `書記`, `主事`, and administrative/supervisory titles containing `事務`, `係長`, `課長`, `所長`, `局長`, `次長`, `場長`, `監視`, `巡視`, `監吏`, or `監督` as clerical. For rank-1 titles, I classify a title as technical only when it contains `技`; otherwise I set track to unknown. Unknown-track rank-1 workers enter both technical and clerical promotion pools. The no-track robustness ignores track for every worker.",
  "",
  paste0("There are ", diag[["unique_occupations_1944"]], " normalized positions in 1944; all appear in the title table below, and no workers fall outside a top-50 list. The track rule leaves ", format(diag[["track_unknown_workers_1944"]], big.mark = ","), " workers with unknown track. The table lists every unknown title explicitly."),
  "",
  "The gravity-model files contain `occ_similarity`, but that measure is cosine similarity between office-level occupation-composition vectors, not a pairwise `Sim(o,o')` between occupations. I therefore use exact normalized-position matches for lateral pools. Robustness (a) repeats this exact-match definition and is numerically identical by construction.",
  "",
  "## Sample and construction",
  "",
  paste0("The source has ", format(diag[["raw_named_rows_1942"]], big.mark = ","), " named 1942 rows and ", format(diag[["raw_named_rows_1944"]], big.mark = ","), " named 1944 rows. Deduplication leaves ", format(diag[["unique_workers_1942"]], big.mark = ","), " workers in 1942 and ", format(diag[["unique_workers_1944"]], big.mark = ","), " in 1944; it drops ", format(diag[["duplicate_worker_year_rows_dropped_1942"]], big.mark = ","), " and ", format(diag[["duplicate_worker_year_rows_dropped_1944"]], big.mark = ","), " repeated worker-year rows, respectively. The male-only 1944 sample contains ", format(stats[["male_workers_1944"]], big.mark = ","), " workers."),
  "",
  paste0("The nested unit is the literal `kyoku × ka × kakari` tuple. I retain missing labels as the explicit level `<missing>`. This produces 383 nested kakari, matching Table 1. Only 66 tuples have all three raw labels observed; dropping incomplete tuples explains a gap of 317. The male-only occupation-cell panel contains ", format(stats[["cells"]], big.mark = ","), " cells across 378 nested kakari; five nested kakari contain no men and therefore leave the analysis sample."),
  "",
  paste0("The panel contains no draft annotations in 1942 (0 men and 0 women). In 1944, it contains ", format(diag[["drafted_men_1944"]], big.mark = ","), " drafted men and ", format(diag[["drafted_women_1944"]], big.mark = ","), " drafted women. I exclude the women from `Draft_ko`; `tables/drafted_women_1944_audit.csv` lists their names and positions. Under the nested definition, ", format(diag[["nested_kakari_with_male_draftee"]], big.mark = ","), " of 383 kakari (", pct(diag[["share_nested_kakari_with_male_draftee"]]), ") contain at least one drafted man."),
  "",
  paste0(format(diag[["linked_workers_1944"]], big.mark = ","), " of ", format(diag[["unique_workers_1944"]], big.mark = ","), " 1944 workers link to 1942, a share of ", pct(diag[["share_workers_1944_linked_1942"]]), ". The incumbent-pool union contains ", format(stats[["incumbent_pool_workers"]], big.mark = ","), " men: linked 1942-1944 men plus unlinked drafted men observed in 1944. I use the 1942 assignment for linked incumbents and the 1944 directory assignment for unlinked draftees. I exclude the destination kakari-occupation cell itself from every pool."),
  "",
  "`Draft_ko` counts drafted men in the 1944 cell. `Staff_ko` counts men in that cell who link to 1942, plus drafted men. Lateral pools contain same-rank incumbents with an exact occupation match at same-ka, same-kyoku/other-ka, or other-kyoku distance. Promotion pools contain rank `r-1` incumbents in the same track at own-kakari, same-ka, same-kyoku/other-ka, or other-kyoku distance. All pools and staff enter as `log(x+1)`. Outcomes count male 1944 arrivals relative to their 1942 cell, split into equal-rank lateral moves, rank increases, and first appearances in 1944.",
  "",
  "## Estimation and diagnostics",
  "",
  "For every outcome I estimate the requested OLS equation with kyoku and occupation fixed effects and kyoku-clustered standard errors. I compute Rademacher wild-cluster bootstrap p-values for every `Draft × log(pool)` and `Draft × log(Staff)` interaction using 999 draws and restricted-null residuals. Only eight kyoku clusters are available, so both conventional clustered and wild-bootstrap inference are imprecise.",
  "",
  paste0("I ran: (1) main OLS with exact-match lateral pools; (2) exact-occupation robustness, identical to (1); (3) PPML with the same regressors and fixed effects; (4) OLS after requiring `Staff_ko >= 3`; and (5) promotion-pool OLS that ignores track. The staff restriction drops ", format(diag[["cells_staff_below_3"]], big.mark = ","), " of ", format(stats[["cells"]], big.mark = ","), " cells and retains ", format(stats[["cells"]] - diag[["cells_staff_below_3"]], big.mark = ","), "."),
  "",
  "PPML estimates lateral same-ka on 334 observations, lateral same-kyoku on 562, lateral other-kyoku on 1,227, promotion same-kyoku on 461, promotion other-kyoku on 1,117, and new hires on 1,265 after removing zero-outcome or singleton fixed-effect groups. PPML promotion same-ka fails because the clustered covariance calculation contains infinite or missing values. Several estimated PPML covariance matrices require a non-positive-semidefinite correction. These are separation/sparsity problems, not silently dropped specifications.",
  "",
  "The data contain many sparse cells: 888 of 1,288 have `Staff_ko < 3`. The original court-rank field is 99.2% missing in 1944 and contains values such as `正八`, so I do not substitute it. The exact Table 1 position rule has no missing employment ranks. Pairwise occupation similarity is unavailable. Hierarchy labels are incomplete for 317 of 383 nested kakari. These limitations and the eight-cluster count apply to all reported estimates.",
  "",
  "## Five-line coefficient summary",
  "",
  paste0("1. ", summary_line("For other-kyoku lateral arrivals, `Draft × log(same-ka lateral pool)`", r1)),
  paste0("2. ", summary_line("For other-kyoku lateral arrivals, `Draft × log(Staff)`", r2)),
  paste0("3. ", summary_line("For same-kyoku promotion arrivals, `Draft × log(same-kyoku promotion pool)`", r3)),
  paste0("4. ", summary_line("For other-kyoku promotion arrivals, `Draft × log(own-kakari promotion pool)`", r4)),
  paste0("5. ", summary_line("For new hires, `Draft × log(own-kakari promotion pool)`", r5)),
  "",
  "## Position assignments (all 42 normalized titles)",
  "",
  "| Position | Rank | Track | Workers in 1944 |",
  "|---|---:|---|---:|"
)

map_lines <- mapping %>%
  arrange(desc(workers_1944), occupation) %>%
  transmute(line = paste0("| ", occupation, " | ", position_rank, " | ", track,
                          if_else(track == "unknown", " (unclassified)", ""),
                          " | ", format(workers_1944, big.mark = ","), " |")) %>%
  pull(line)

writeLines(c(lines, map_lines), file.path(base, "method_note.md"), useBytes = TRUE)
