library(dplyr)
library(stringr)
library(tidyverse)
library(scales)
library(ggplot2)

# ==============================================================================
# 1. Load Data
# ==============================================================================
# Use tilde (~) to represent C:/Users/Keitaro Ninomiya/
# 1. Define the User Path dynamically
user_path <- Sys.getenv("USERPROFILE") # Gets "C:/Users/Keitaro Ninomiya"

# 2. Build the path to the Box data
DATA_ROOT <- file.path(user_path, "Box/Research Notes (keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data")
FILE_PATH <- file.path(DATA_ROOT, "Tokyo_Personnel_Master_All_Years_v2.csv")

# 3. Load the data
df <- read_csv(FILE_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  filter(is_name == TRUE)
names(df) <- tolower(names(df))
colnames(df)
# ==============================================================================
# 2. Classification Logic
# ==============================================================================
prewar_elite_pat  <- "^\u4e3b\u4e8b$|^\u6280\u5e2b$|^\u8996\u5b66$|^\u7406\u4e8b$"

df_clean <- df %>%
  filter(!is.na(year)) %>%
  mutate(
    name_clean  = str_trim(name),
    pos_norm    = str_replace_all(position, "\\s+", ""),
    year_num    = as.numeric(year),
    
    gender = gender_modern,
    
    # --- Dynamic Rank Logic ---
    is_elite_title = case_when(
      year_num <= 1945 ~ str_detect(pos_norm, prewar_elite_pat),
      year_num > 1945  ~ str_detect(pos_norm, "\u9577"),
      TRUE ~ FALSE
    ),
    
    rank_category = if_else(is_elite_title, "官吏 (Elite)", "公吏 (Clerk/Local)")
  )


# ==============================================================================
# 3. Aggregate Data
# ==============================================================================
ts_combined <- df_clean %>%
  filter(gender == "female") %>%
  group_by(year_num, rank_category) %>%
  summarise(n = n(), .groups = 'drop') %>%
  complete(year_num = unique(df_clean$year_num), rank_category, fill = list(n = 0)) %>%
  filter(year_num != 1945) %>%
  arrange(year_num)

# Replace zero with 0.5 so points are visible at the bottom of the log scale
ts_plot <- ts_combined %>%
  mutate(n_plot = if_else(n == 0L, 0.5, as.double(n)))


# ==============================================================================
# 4. Generate Plot (Corrected Label Position)
# ==============================================================================
cols <- c("官吏 (Elite)" = "#d73027", "公吏 (Clerk/Local)" = "#1a9850")

p_log_strong <- ggplot() +
  
  # --- 1. Background Rectangles ---
  geom_rect(aes(xmin = -Inf, xmax = 1945.5, ymin = 0.5, ymax = Inf),
            fill = "#ffcccc", alpha = 0.6) + 
  
  geom_rect(aes(xmin = 1949.5, xmax = Inf, ymin = 0.5, ymax = Inf),
            fill = "#cceeff", alpha = 0.6) + 
  
  # --- 2. Era Text Labels (MOVED LEFT) ---
  annotate("text", x = 1941, y = 3, 
           label = "War Era\n(Pre-1945)", size = 4.5, fontface = "bold", color = "#8b0000") +
  
  # CHANGED X FROM 1955 -> 1953 to prevent hiding
  annotate("text", x = 1953, y = 3, 
           label = "Post-Reform\n(1950+)", size = 4.5, fontface = "bold", color = "#00008b") +
  
  # --- 3. Data Lines ---
  geom_line(data = ts_plot, aes(x = year_num, y = n_plot, color = rank_category), size = 1.2) + 
  geom_point(data = ts_plot, aes(x = year_num, y = n_plot, color = rank_category), size = 3) +
  
  # --- 4. Scales & Theme ---
  scale_color_manual(values = cols) +
  
  # Added a small expansion (padding) to the X-axis so labels don't hit the edge
  scale_x_continuous(breaks = unique(ts_combined$year_num), expand = expansion(mult = 0.05)) + 
  
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
    title = "Female Employment in Tokyo (Log Scale)",
    subtitle = "Gap represents the occupation/transition period (1946-1949)",
    x = "Year",
    y = "Total Headcount (Log Scale)",
    color = "Rank Category"
  )

print(p_log_strong)

# ==============================================================================
# 5. Export for LaTeX (Save to Tokyo_Project/Results)
# ==============================================================================

# 1. Define the output path dynamically (Windows)
output_dir_candidates <- c(
  file.path(Sys.getenv("USERPROFILE"), "Documents/GitHub/Tokyo_Project (Draft)/Results"),
  file.path(Sys.getenv("USERPROFILE"), "Documents/GitHub/Tokyo_Project/Results")
)
output_dirs <- output_dir_candidates[dir.exists(output_dir_candidates)]
if (length(output_dirs) == 0) {
  stop("No valid output directory found under Documents/GitHub.")
}

# 2. Define the filename (PDF is best for LaTeX)
output_filename <- "TimeSeriesChihouKomuin.pdf"
# 3. Save the plot to all detected project outputs
# width=8, height=5 is optimal for slides (makes text appear relatively larger)
for (output_dir in output_dirs) {
  full_save_path <- file.path(output_dir, output_filename)
  ggsave(
    filename = full_save_path,
    plot = p_log_strong,
    device = "pdf",
    width = 8,
    height = 5,
    units = "in"
  )

  # Optional: Save a PNG version as well (for quick previewing outside LaTeX)
  ggsave(
    filename = file.path(output_dir, "TimeSeriesChihouKomuin.png"),
    plot = p_log_strong,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )

  message(paste("Figure saved to:", full_save_path))
}
