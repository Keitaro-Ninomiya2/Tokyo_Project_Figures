################################################################################
# Check whether salary parsing failure is due to file encoding.
################################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

parse_salary_utf8_style <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c("ã€‡" = "0", "ä¸€" = "1", "äºŒ" = "2", "ä¸‰" = "3", "å››" = "4",
             "äº”" = "5", "å…­" = "6", "ä¸ƒ" = "7", "å…«" = "8", "ä¹" = "9")
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }
  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    is_annual <- str_detect(x, "^å¹´")
    cleaned <- str_remove(x, "^[æœˆå¹´]")
    if (nchar(cleaned) == 0) return(NA_real_)
    chars <- strsplit(cleaned, "")[[1]]
    digits <- sapply(chars, kanji_to_digit)
    if (any(is.na(digits))) return(NA_real_)
    val <- as.numeric(paste(digits, collapse = ""))
    if (is_annual) val <- val / 12
    val
  }
  sapply(s, parse_one, USE.NAMES = FALSE)
}

parse_salary_jp <- function(s) {
  kanji_to_digit <- function(ch) {
    map <- c("〇" = "0", "一" = "1", "二" = "2", "三" = "3", "四" = "4",
             "五" = "5", "六" = "6", "七" = "7", "八" = "8", "九" = "9")
    ifelse(ch %in% names(map), map[ch], NA_character_)
  }
  parse_one <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    is_annual <- str_detect(x, "^年")
    cleaned <- str_remove(x, "^[月年]")
    if (nchar(cleaned) == 0) return(NA_real_)
    chars <- strsplit(cleaned, "")[[1]]
    digits <- sapply(chars, kanji_to_digit)
    if (any(is.na(digits))) return(NA_real_)
    val <- as.numeric(paste(digits, collapse = ""))
    if (is_annual) val <- val / 12
    val
  }
  sapply(s, parse_one, USE.NAMES = FALSE)
}

check_one <- function(enc, parser, parser_label) {
  df <- read_csv(DATA_PATH, locale = locale(encoding = enc), show_col_types = FALSE) %>%
    filter(is_name == TRUE, year == 1944, !is.na(salary), salary != "") %>%
    mutate(salary_num = parser(salary))
  tibble(
    encoding = enc,
    parser = parser_label,
    n_rows = nrow(df),
    share_parsed_positive = mean(!is.na(df$salary_num) & df$salary_num > 0),
    sample_salary_1 = df$salary[[1]],
    sample_salary_2 = df$salary[[2]],
    sample_salary_num_1 = df$salary_num[[1]],
    sample_salary_num_2 = df$salary_num[[2]]
  )
}

results <- bind_rows(
  check_one("UTF-8", parse_salary_utf8_style, "utf8_style"),
  check_one("UTF-8", parse_salary_jp, "jp_style"),
  check_one("CP932", parse_salary_utf8_style, "utf8_style"),
  check_one("CP932", parse_salary_jp, "jp_style")
)

write_csv(results, file.path(getwd(), "MainResults", "SlackDistribution", "results", "worker_salary_encoding_check.csv"))
print(results, n = Inf)
