rm(list=ls())

library(dplyr)
library(stringr)
library(tidyverse)
library(scales)
library(ggplot2)

# 1. Load the data
DATA_PATH <- "C:/Users/yoko1/Box/Processed_Data/merged_data_cleaned.csv"
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

# ---------------------------
# 2. Title patterns
# ---------------------------

# 「地方（自治体）」っぽい職名
civil_pat2 <- paste(
  "係長|課長|局長|所長|區長|区長",
  "書記|主事|事務官|地方事務官|労働事務官|勞働事務官",
  "吏員|事務員|属|屬",
  "技師|技手",
  "教員|教諭|助教諭|視學|視学",
  sep="|"
)

civil_pat3 <- paste(
  civil_pat2,
  "監督|監視|巡視",
  "醫員|医員|看護|保健婦|藥劑員|薬剤|助産",
  "清掃|運輸|機關手|機関手|船長",
  "授業員|講師|體力指導員|体力指導員",
  "掛長",
  sep="|"
)

# Reflect Nino-kun's comments that 官吏（国家公務員）っぽいもの
# - 「◯◯長」はほとんど官吏
# - 主事・技師は国家公務員として入庁した人が多い
national_pat <- paste(
  "係長|課長|局長|所長|區長|区長|部長|長$",
  "主事|技師",
  sep="|"
)

# 雇（雇員系）は非公務員側へ（baseline）
yatoi_pat <- "^雇|雇$|雇"  

# ---------------------------
# 3. Build groups (3-series)
# ---------------------------
df_for_plot <- df %>%
  mutate(
    position_clean   = str_squish(final_position),
    position_clean   = str_remove(position_clean, "^[○●◇■]+"),
    position_nospace = str_replace_all(position_clean, "\\s+", ""),
    
    is_national = str_detect(position_nospace, national_pat),
    is_yatoi    = str_detect(position_nospace, yatoi_pat),
    is_local    = str_detect(position_nospace, civil_pat3),
    
    # ★3分類（優先順位：国家 → 地方 → 非公務員）
    # - 国家に当たれば国家（placebo）
    # - それ以外で地方辞書に当たれば地方（treated）
    # - 残り（雇＋その他）は非公務員（baseline）
    job_group3 = case_when(
      is_national ~ "国家公務員（placebo）",
      is_local    ~ "地方公務員（treated）",
      TRUE        ~ "非公務員（baseline）"
    )
  ) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year), year < 1960)   # 1960は落とす

# ---------------------------
# 4. Time series counts
# ---------------------------
ts_gender <- df_for_plot %>%
  filter(!is.na(gender), !is.na(job_group3)) %>%
  count(year, gender, job_group3, name = "n") %>%
  mutate(gender = factor(gender, levels = c("female", "male")))

# Plot params
ww2_start <- 1939
ww2_end   <- 1945
major_by  <- 5
minor_by  <- 1

x_min <- min(ts_gender$year)
x_max <- max(ts_gender$year)

# ---------------------------
# 5. Plots (separate by gender)
# ---------------------------
p_female <- ggplot(filter(ts_gender, gender == "female"),
                   aes(x = year, y = n, color = job_group3, group = job_group3)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = major_by),
    minor_breaks = seq(x_min, x_max, by = minor_by)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "Female headcount: Treated (Local) vs Placebo (National) vs Baseline (Non-public)",
    x = "Year", y = "Count", color = "Job group"
  )

p_male <- ggplot(filter(ts_gender, gender == "male"),
                 aes(x = year, y = n, color = job_group3, group = job_group3)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = major_by),
    minor_breaks = seq(x_min, x_max, by = minor_by)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "Male headcount: Treated (Local) vs Placebo (National) vs Baseline (Non-public)",
    x = "Year", y = "Count", color = "Job group"
  )

p_female
p_male


# if you want to save these figures, here it is 
ggsave("figure_female_headcount.png", p_female, width = 7, height = 4.5, dpi = 300)
ggsave("figure_male_headcount.png",   p_male,   width = 7, height = 4.5, dpi = 300)




# here is the 2nd step that we have 公務員（国家＋地方） vs 非公務員

ts_gender_public <- ts_gender %>%
  mutate(
    job_group2 = if_else(
      job_group3 %in% c("国家公務員（placebo）", "地方公務員（treated）"),
      "公務員（国家＋地方）",
      "非公務員"
    )
  ) %>%
  group_by(year, gender, job_group2) %>%
  summarise(n = sum(n), .groups = "drop")

p_female2 <- ggplot(filter(ts_gender_public, gender == "female"),
                    aes(x = year, y = n, color = job_group2, group = job_group2)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = major_by),
    minor_breaks = seq(x_min, x_max, by = minor_by)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "Female headcount: Public (National+Local) vs Non-public",
    x = "Year", y = "Count", color = "Job group"
  )

p_male2 <- ggplot(filter(ts_gender_public, gender == "male"),
                    aes(x = year, y = n, color = job_group2, group = job_group2)) +
  annotate("rect", xmin = ww2_start, xmax = ww2_end, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "grey50") +
  geom_vline(xintercept = 1950, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = major_by),
    minor_breaks = seq(x_min, x_max, by = minor_by)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "Male headcount: Public (National+Local) vs Non-public",
    x = "Year", y = "Count", color = "Job group"
  )


p_female2
p_male2


