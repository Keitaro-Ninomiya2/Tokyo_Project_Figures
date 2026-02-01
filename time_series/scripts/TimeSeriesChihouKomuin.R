rm(list=ls())

library(dplyr)
library(stringr)
library(tidyverse)
library(scales)
library(ggplot2)

# 1. Load the data
data_paths <- c(
  "Keitaro Ninomiya" = "C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Tokyo_Jobs/Processed_Data",
  "yoko1" = "C:/Users/yoko1/Box/Processed_Data"
)
data_dir <- data_paths[Sys.info()["user"]]
DATA_PATH<- file.path(data_dir, "merged_data_cleaned.csv")

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

names(df) <- tolower(names(df))

# ==============================================================================
# 2. Define Strict Taxonomy
# ==============================================================================

# --- A. 官吏 (National / Elite) Definition ---
# User Rule: Only 主事, 技師, and Chief roles (*長).
base_national_pat <- paste(
  "主事",  # Shuji
  "技師",  # Gishi
  "長$",   # Any role ending in "Chief/Head" (Cho)
  sep="|"
)

# --- B. Exclusions (Safety Filter) ---
# We must exclude "Chiefs" in female-dominated field roles to prevent the
# "Head Nurse" (看護婦長) or "Head Nanny" (保母長) artifact from returning.
exclude_pat <- paste(
  "看護",   # Nurse
  "保母",   # Nanny
  "保健婦", # Public Health Nurse
  "婦長",   # Head Nurse (specific term)
  "巡視",   # Patrol
  "雇",     # Employment/Temporary (just in case "雇長" exists)
  sep="|"
)

# ==============================================================================
# 3. Classification Logic
# ==============================================================================
df_for_plot <- df %>%
  mutate(
    # Clean text
    position_clean   = str_squish(final_position),
    position_clean   = str_remove(position_clean, "^[○●◇■]+"),
    position_nospace = str_replace_all(position_clean, "\\s+", ""),
    
    # Check flags
    matches_national_base = str_detect(position_nospace, base_national_pat),
    matches_exclusion     = str_detect(position_nospace, exclude_pat),
    
    # --- BINARY TAXONOMY ---
    # Rule: If it matches (主事|技師|*長) AND is not (Nurse/Nanny) -> 官吏
    #       Everything else -> 公吏
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

# Colors: Red for Elite (National), Green for Local (Everything else)
binary_colors <- c(
  "官吏" = "#d73027",  # Red
  "公吏" = "#1a9850"   # Green
)

# ==============================================================================
# 5. Generate Plots
# ==============================================================================

# --- Female Plot ---
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

# --- Male Plot ---
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

# Print and Save
print(p_female)
print(p_male)

ggsave("figure_female_binary_strict.png", p_female, width = 8, height = 5, dpi = 300)
ggsave("figure_male_binary_strict.png",   p_male,   width = 8, height = 5, dpi = 300)

