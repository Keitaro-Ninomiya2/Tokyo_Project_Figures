#!/usr/bin/env Rscript
# Local development runner for TimeSeriesChihouKomuin.R
# This script sets up the environment to use local sample data instead of Box paths.

rm(list=ls())

library(dplyr)
library(stringr)
library(tidyverse)
library(scales)
library(ggplot2)

# Use local data path instead of Box Sync paths
DATA_PATH <- file.path(dirname(dirname(normalizePath("."))), "time_series", "data", "merged_data_cleaned.csv")
if (!file.exists(DATA_PATH)) {
  DATA_PATH <- file.path("time_series", "data", "merged_data_cleaned.csv")
}
if (!file.exists(DATA_PATH)) {
  DATA_PATH <- file.path("..", "data", "merged_data_cleaned.csv")
}

cat("Loading data from:", DATA_PATH, "\n")

df <- read_csv(
  DATA_PATH,
  locale = locale(encoding = "UTF-8"),
  col_types = cols(
    text           = col_character(),
    Office         = col_character(),
    Name           = col_character(),
    Department     = col_character(),
    Division       = col_character(),
    Final_Position = col_character()
  ),
  show_col_types = FALSE
)

names(df) <- tolower(names(df))

# ==============================================================================
# 2. Define Strict Taxonomy
# ==============================================================================

base_national_pat <- paste(
  "主事",
  "技師",
  "長$",
  sep="|"
)

exclude_pat <- paste(
  "看護",
  "保母",
  "保健婦",
  "婦長",
  "巡視",
  "雇",
  sep="|"
)

# ==============================================================================
# 3. Classification Logic
# ==============================================================================
df_for_plot <- df %>%
  mutate(
    position_clean   = str_squish(final_position),
    position_clean   = str_remove(position_clean, "^[○●◇■]+"),
    position_nospace = str_replace_all(position_clean, "\\s+", ""),
    matches_national_base = str_detect(position_nospace, base_national_pat),
    matches_exclusion     = str_detect(position_nospace, exclude_pat),
    job_group = if_else(
      matches_national_base & !matches_exclusion, 
      "官吏", 
      "公吏"
    )
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year), year < 1960)

# ==============================================================================
# 4. Aggregate Data
# ==============================================================================
ts_gender <- df_for_plot %>%
  filter(!is.na(gender), !is.na(job_group)) %>%
  count(year, gender, job_group, name = "n") %>%
  mutate(gender = factor(gender, levels = c("female", "male")))

# Plot Parameters
ww2_start <- 1937
ww2_end   <- 1945
major_by  <- 5
minor_by  <- 1
x_min     <- min(ts_gender$year)
x_max     <- max(ts_gender$year)

binary_colors <- c(
  "官吏" = "#d73027",
  "公吏" = "#1a9850"
)

# ==============================================================================
# 5. Generate Plots
# ==============================================================================

p_female <- ggplot(filter(ts_gender, gender == "female"),
                   aes(x = year, y = n, color = job_group, group = job_group)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = binary_colors) +
  scale_x_continuous(breaks = seq(x_min, x_max, by = major_by),
                     minor_breaks = seq(x_min, x_max, by = minor_by)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Female Headcount: 官吏 (Ranked/Chiefs) vs 公吏 (Others)",
    subtitle = "Taxonomy: 官吏 = 主事, 技師, *長 (Excluding Nurses). 公吏 = All others.",
    x = "Year", y = "Count", color = "Rank"
  )

p_male <- ggplot(filter(ts_gender, gender == "male"),
                 aes(x = year, y = n, color = job_group, group = job_group)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = binary_colors) +
  scale_x_continuous(breaks = seq(x_min, x_max, by = major_by),
                     minor_breaks = seq(x_min, x_max, by = minor_by)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Male Headcount: 官吏 (Ranked/Chiefs) vs 公吏 (Others)",
    subtitle = "Taxonomy: 官吏 = 主事, 技師, *長. 公吏 = All others.",
    x = "Year", y = "Count", color = "Rank"
  )

# Save plots
output_dir <- file.path("time_series", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "figure_female_binary_strict.png"), p_female, width = 8, height = 5, dpi = 300)
ggsave(file.path(output_dir, "figure_male_binary_strict.png"),   p_male,   width = 8, height = 5, dpi = 300)

cat("\nPlots saved successfully to:", output_dir, "\n")
cat("- figure_female_binary_strict.png\n")
cat("- figure_male_binary_strict.png\n")
cat("\nData summary:\n")
print(ts_gender)
