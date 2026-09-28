suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

data_path <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

raw <- read_csv(data_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

cat("Columns:\n", paste(names(raw), collapse = ", "), "\n\n", sep = "")
cat("gov_level values in 1942/1944 named rows:\n")
print(raw %>% filter(year %in% c(1942, 1944), is_name) %>% count(year, gov_level))

tokyo <- raw %>%
  filter(
    (year == 1942 & gov_level == "TokyoShi") |
      (year == 1944 & gov_level == "TokyoTo"),
    is_name, !is.na(staff_id)
  ) %>%
  distinct(staff_id, year, .keep_all = TRUE) %>%
  mutate(
    occupation = str_remove_all(coalesce(position, ""), "\\s+"),
    gender = coalesce(gender_modern, "missing"),
    rank_num = suppressWarnings(as.integer(rank))
  )

cat("\nWorker counts:\n")
print(tokyo %>% count(year, name = "workers"))
cat("\nDrafted counts by year and gender:\n")
print(tokyo %>% count(year, gender, drafted, name = "workers") %>% filter(drafted %in% TRUE))
cat("\nRank distribution (including missing/invalid):\n")
print(tokyo %>% mutate(rank_report = if_else(rank_num %in% 1:3, as.character(rank_num), "missing_or_invalid")) %>% count(year, rank_report) %>% group_by(year) %>% mutate(share = n / sum(n)) %>% ungroup())
cat("\nRaw rank values:\n")
print(tokyo %>% mutate(rank_raw = if_else(is.na(rank) | rank == "", "<missing>", as.character(rank))) %>% count(year, rank_raw, sort = TRUE), n = Inf)
cat("\nRaw grade values (diagnostic only; not substituted):\n")
print(tokyo %>% mutate(grade_raw = if_else(is.na(grade) | grade == "", "<missing>", as.character(grade))) %>% count(year, grade_raw, sort = TRUE), n = Inf)

occ44 <- tokyo %>% filter(year == 1944) %>% count(occupation, sort = TRUE)
cat("\nUnique normalized positions in 1944:", nrow(occ44), "\n")
cat("Top 50 normalized positions:\n")
print(head(occ44, 50), n = 50)
cat("Workers outside top 50:", sum(tail(occ44$n, -50)), "\n")

kakari44 <- tokyo %>% filter(year == 1944, !is.na(kakari), kakari != "")
cat("\n1944 hierarchy completeness and distinct counts:\n")
print(tokyo %>% filter(year == 1944) %>% summarise(
  workers = n(),
  missing_kyoku = sum(is.na(kyoku) | kyoku == ""),
  missing_ka = sum(is.na(ka) | ka == ""),
  missing_kakari = sum(is.na(kakari) | kakari == ""),
  nested_kakari_complete = n_distinct(paste(kyoku, ka, kakari, sep = " || ")[!(is.na(kyoku) | kyoku == "" | is.na(ka) | ka == "" | is.na(kakari) | kakari == "")]),
  nested_kakari_including_missing = n_distinct(paste(coalesce(kyoku, "<NA>"), coalesce(ka, "<NA>"), coalesce(kakari, "<NA>"), sep = " || ")),
  office_kakari_complete = n_distinct(paste(office_id, kakari, sep = " || ")[!(is.na(office_id) | office_id == "" | is.na(kakari) | kakari == "")])
), width = Inf)
cat("\nShare of 1944 kakari with at least one draftee (all genders):\n")
print(kakari44 %>% group_by(office_id, kyoku, ka, kakari) %>% summarise(any_drafted = any(drafted %in% TRUE), .groups = "drop") %>% summarise(kakari = n(), exposed = sum(any_drafted), share = mean(any_drafted)))
cat("\nShare of 1944 kakari with at least one male draftee (treatment definition):\n")
print(kakari44 %>% group_by(office_id, kyoku, ka, kakari) %>% summarise(any_drafted = any(drafted %in% TRUE & gender == "male"), .groups = "drop") %>% summarise(kakari = n(), exposed = sum(any_drafted), share = mean(any_drafted)))
