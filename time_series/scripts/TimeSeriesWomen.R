################################################################################
# Time Series: Female Employment in Tokyo Civil Service (1928-1955)
#
# Distinguishes between 官吏 (kanri, elite) and 公吏 (kouri, non-elite)
# Log scale, dashed lines for gaps
# Similar format to TimeSeriesChihouKomuin.R
################################################################################

library(tidyverse)
library(scales)
library(ggplot2)

# ============================================================
# 1. Load Data (use FULL dataset for 1928+ coverage)
# ============================================================

user_path <- Sys.getenv("USERPROFILE")

DATA_ROOT <- file.path(user_path, "Box/Research Notes (keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data")
FILE_PATH <- file.path(DATA_ROOT, "Tokyo_Personnel_Master_All_Years.csv")

df <- read_csv(FILE_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE)
names(df) <- tolower(names(df))

# ============================================================
# 2. Classification: 官吏 vs 公吏
# ============================================================

# Same classification as TimeSeriesChihouKomuin.R
prewar_elite_pat <- "^主事$|^技師$|^視学$|^理事$"
cutoff_year <- 1947

df_clean <- df %>%
  filter(!is.na(year)) %>%
  mutate(
    pos_norm = str_replace_all(position, "\\s+", ""),
    year_num = as.numeric(year),
    gender   = gender_modern,

    is_elite_title = case_when(
      year_num <= cutoff_year ~ str_detect(pos_norm, prewar_elite_pat),
      year_num > cutoff_year  ~ str_detect(pos_norm, "長"),
      TRUE ~ FALSE
    ),

    rank_category = factor(
      if_else(is_elite_title, "elite", "non_elite"),
      levels = c("elite", "non_elite")
    )
  )

# ============================================================
# 3. Aggregate: female count by year x rank category
# ============================================================

ts_data <- df_clean %>%
  filter(gender == "female") %>%
  group_by(year_num, rank_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(year_num = unique(df_clean$year_num), rank_category, fill = list(n = 0)) %>%
  filter(year_num >= 1928, year_num <= 1955) %>%
  filter(year_num != 1945) %>%  # No 1945 data

  arrange(year_num)

# Labels
ts_plot <- ts_data %>%
  mutate(
    n_plot = if_else(n == 0L, 0.5, as.double(n)),
    category_label = case_when(
      year_num <= cutoff_year & rank_category == "elite"     ~ "Kanri",
      year_num <= cutoff_year & rank_category == "non_elite" ~ "Kouri",
      year_num > cutoff_year  & rank_category == "elite"     ~ "Manager",
      year_num > cutoff_year  & rank_category == "non_elite" ~ "Non-manager",
      TRUE ~ NA_character_
    ),
    category_label = factor(category_label,
                            levels = c("Kanri", "Kouri", "Manager", "Non-manager"))
  )

# ============================================================
# 4. Handle gaps: connect missing years with dashed lines
# ============================================================

# Years present in the data
available_years <- sort(unique(ts_plot$year_num))
cat("Available years:", paste(available_years, collapse = ", "), "\n")

# For solid lines: only connect consecutive available years
# For dashed lines: connect across gaps
# Strategy: solid lines for data, dashed segments bridging gaps

# Identify gap pairs for each category
# Build gap segments directly (no group_modify to avoid column stripping)
gap_segments <- ts_plot %>%
  arrange(category_label, year_num) %>%
  group_by(category_label) %>%
  mutate(
    next_year = lead(year_num),
    n_plot_end = lead(n_plot),
    gap = next_year - year_num
  ) %>%
  ungroup() %>%
  filter(!is.na(gap), gap > 1) %>%
  select(year_num, n_plot, next_year, n_plot_end, category_label)

# For solid lines: set n_plot to NA where there's a gap (year not in data)
# Instead, we'll use geom_line which automatically connects available points,
# and add gap segments manually

# Drop 1954 data quality issue (same as original script)
ts_plot_solid <- ts_plot %>%
  mutate(n_plot = if_else(year_num == 1954, NA_real_, n_plot))

ts_plot_points <- ts_plot %>% filter(year_num != 1954)

# Dashed line for 1953-1955 bridge
ts_plot_dashed_1954 <- ts_plot %>% filter(year_num %in% c(1953, 1955))

# Additional dashed segments for other gaps (1945 removed, 1947/1949 missing)
# These are handled by gap_segments computed above

# ============================================================
# 5. Generate Plot
# ============================================================

cols <- c(
  "Kanri" = "#b2182b",
  "Kouri" = "#1a9850",
  "Manager"       = "#2166ac",
  "Non-manager"   = "#f4a582"
)

p <- ggplot() +

  # --- Background regions ---
  geom_rect(aes(xmin = -Inf, xmax = 1945.5, ymin = 0.3, ymax = Inf),
            fill = "#ffe0e0", alpha = 0.6) +

  geom_rect(aes(xmin = 1945.5, xmax = 1946.5, ymin = 0.3, ymax = Inf),
            fill = "#f5f5f5", alpha = 0.8) +

  geom_rect(aes(xmin = 1946.5, xmax = Inf, ymin = 0.3, ymax = Inf),
            fill = "#e0f0ff", alpha = 0.6) +

  # --- Era labels ---
  annotate("text", x = 1936, y = 1.5,
           label = "War Era\n(through 1945)", size = 4, fontface = "bold", color = "#8b0000") +

  annotate("text", x = 1946, y = 1.5,
           label = "Transition", size = 3.5, fontface = "bold", color = "#444444") +

  annotate("text", x = 1952, y = 1.5,
           label = "Post-Reform\n(1947+)", size = 4, fontface = "bold", color = "#00008b") +

  # --- Solid lines (data points, NA gaps break the line) ---
  geom_line(data = ts_plot_solid, aes(x = year_num, y = n_plot, color = category_label),
            linewidth = 1.2) +

  # --- Dashed line segments across gaps ---
  geom_line(data = ts_plot_dashed_1954,
            aes(x = year_num, y = n_plot, color = category_label),
            linewidth = 1.0, linetype = "dashed") +

  # Additional dashed segments for other year gaps
  geom_segment(data = gap_segments,
               aes(x = year_num, xend = next_year, y = n_plot, yend = n_plot_end,
                   color = category_label),
               linewidth = 1.0, linetype = "dashed") +

  # --- Points ---
  geom_point(data = ts_plot_points,
             aes(x = year_num, y = n_plot, color = category_label), size = 2.5) +

  # --- Scales & Theme ---
  scale_color_manual(values = cols) +

  scale_x_continuous(
    breaks = seq(1928, 1955, by = 2),
    expand = expansion(mult = 0.03)
  ) +

  scale_y_log10(labels = comma) +

  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +

  labs(
    title = "Female Employment in Tokyo Civil Service (1928-1955)",
    subtitle = "Kanri vs. Kouri, log scale",
    x = "Year",
    y = "Female Headcount (Log Scale)",
    color = "Category"
  )

print(p)

# ============================================================
# 6. Export
# ============================================================

output_dir_candidates <- c(
  file.path(Sys.getenv("USERPROFILE"), "Documents/GitHub/Tokyo_Project (Draft)/Results"),
  file.path(Sys.getenv("USERPROFILE"), "Documents/GitHub/Tokyo_Project/Results")
)
output_dirs <- output_dir_candidates[dir.exists(output_dir_candidates)]
if (length(output_dirs) == 0) {
  warning("No valid output directory found under Documents/GitHub. Saving locally.")
  output_dirs <- here("time_series")
}

output_filename <- "TimeSeriesWomen"

for (output_dir in output_dirs) {
  ggsave(
    filename = file.path(output_dir, paste0(output_filename, ".pdf")),
    plot = p,
    device = "pdf",
    width = 10,
    height = 5.5,
    units = "in"
  )

  ggsave(
    filename = file.path(output_dir, paste0(output_filename, ".png")),
    plot = p,
    width = 10,
    height = 5.5,
    units = "in",
    dpi = 300
  )

  message(paste("Figure saved to:", file.path(output_dir, paste0(output_filename, ".pdf"))))
}
