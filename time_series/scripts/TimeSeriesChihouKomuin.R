library(dplyr)
library(stringr)
library(tidyverse)
library(scales)
library(ggplot2)

# ==============================================================================
# 1. Load Data
# ==============================================================================
DATA_PATH <- "C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data/Tokyo_Personnel_Master_1937_1960.csv"

df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
names(df) <- tolower(names(df))

# ==============================================================================
# 2. Classification Logic
# ==============================================================================
female_kanji_pat  <- "[子枝江代紀美恵美貴]$|婦$|^[小]?[佐]?[美]"
surname_blocklist <- "金子|増子|尼子|砂子|白子|呼子|舞子|神子" 
prewar_elite_pat  <- "^主事$|^技師$|^視学$|^理事$"

df_clean <- df %>%
  filter(!is.na(year)) %>%
  mutate(
    name_clean  = str_trim(name),
    pos_norm    = str_replace_all(position, "\\s+", ""),
    year_num    = as.numeric(year),
    
    # --- Gender Logic ---
    gender = case_when(
      str_detect(name_clean, female_kanji_pat) & !str_detect(name_clean, surname_blocklist) ~ "female",
      TRUE ~ "male"
    ),
    
    # --- Dynamic Rank Logic ---
    is_elite_title = case_when(
      year_num <= 1945 ~ str_detect(pos_norm, prewar_elite_pat),
      year_num > 1945  ~ str_detect(pos_norm, "長"),
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

# ==============================================================================
# 4. Generate Plot (Corrected Label Position)
# ==============================================================================
cols <- c("官吏 (Elite)" = "#d73027", "公吏 (Clerk/Local)" = "#1a9850")

p_log_strong <- ggplot() +
  
  # --- 1. Background Rectangles ---
  geom_rect(aes(xmin = -Inf, xmax = 1945.5, ymin = 0, ymax = Inf),
            fill = "#ffcccc", alpha = 0.6) + 
  
  geom_rect(aes(xmin = 1949.5, xmax = Inf, ymin = 0, ymax = Inf),
            fill = "#cceeff", alpha = 0.6) + 
  
  # --- 2. Era Text Labels (MOVED LEFT) ---
  annotate("text", x = 1941, y = 3, 
           label = "War Era\n(Pre-1945)", size = 4.5, fontface = "bold", color = "#8b0000") +
  
  # CHANGED X FROM 1955 -> 1953 to prevent hiding
  annotate("text", x = 1953, y = 3, 
           label = "Post-Reform\n(1950+)", size = 4.5, fontface = "bold", color = "#00008b") +
  
  # --- 3. Data Lines ---
  geom_line(data = ts_combined, aes(x = year_num, y = n, color = rank_category), linewidth = 1.2) + 
  geom_point(data = ts_combined, aes(x = year_num, y = n, color = rank_category), size = 3) +
  
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