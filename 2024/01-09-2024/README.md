# Week 2 (Jan 9, 2024) - Canadian NHL Birth Month Advantage

**📊 Project Overview**

**Date:** January 9, 2024 (Week 2)
**Dataset:** Canadian NHL Player Birth Months vs General Population
**Theme:** Malcolm Gladwell's "Outliers" Theory
**Creator:** Louie Christopher Wee

**🎯 Objective**

Investigate whether Canadian NHL players show a birth month advantage, particularly in Q1 (January-March), compared to the general Canadian population, validating Malcolm Gladwell's famous theory from "Outliers."

**📈 Key Insights**

**🏂 The Birth Month Effect**

- **Q1 Dominance:** Canadian NHL players are dramatically over-represented in early birth months
- **January Peak:** Highest concentration of NHL players born in January
- **Gradual Decline:** Player representation decreases through the year
- **Q4 Disadvantage:** Significantly fewer players born in October-December

**🔍 Why This Matters**

- **Youth Sports Cutoffs:** Age cutoffs in youth hockey favor earlier-born children
- **Compound Advantages:** Physical and developmental advantages accumulate over time
- **System Bias:** Reveals how arbitrary deadlines can shape elite athletic careers
- **Social Science Validation:** Confirms Gladwell's observations with empirical data

**🛠️ Technical Implementation**

**📊 Visualization Features**

- Side-by-side comparison of population vs NHL player birth distributions
- Color-coded subtitle with red text for NHL players, blue for general population
- Q1 highlighting with subtle red background shade
- Clean, publication-ready design following modern data visualization principles

**📊 Data Sources**

- **Statistics Canada:** Canadian birth records 1991-2022
- **NHL API:** Professional hockey player birth information
- **TidyTuesday:** Data Science Learning Community curated dataset

```r
# TidyTuesday 2024 Week 2: Canadian NHL Birth Month Advantage
# Created by: Louie Wee
# Date: July 8, 2025
# Data: Statistics Canada, NHL API
# Language: R | Libraries: tidyverse, ggplot2, ggtext

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(scales)
library(lubridate)

# Try to load showtext for fonts
font_family <- "sans"
tryCatch({
  library(showtext)
  font_add_google("Inter", "inter")
  showtext_auto()
  showtext_opts(dpi = 300)
  font_family <- "inter"
}, error = function(e) {})

# Load data  
canada_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv', show_col_types = FALSE)
nhl_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv', show_col_types = FALSE)

# Process Canadian population data
canada_monthly <- canada_births %>%
  group_by(month) %>%
  summarise(total_births = sum(births), .groups = "drop") %>%
  mutate(pct_population = (total_births / sum(total_births)) * 100)

# Process NHL player data
nhl_canadian <- nhl_births %>%
  filter(birth_country == "Canada")

nhl_monthly <- nhl_canadian %>%
  mutate(birth_month = month(birth_date)) %>%
  count(birth_month, name = "nhl_players") %>%
  mutate(pct_nhl = (nhl_players / sum(nhl_players)) * 100)

# Combine data
comparison_data <- canada_monthly %>%
  left_join(nhl_monthly, by = c("month" = "birth_month")) %>%
  mutate(month_name = factor(month.abb[month], levels = month.abb)) %>%
  replace_na(list(pct_nhl = 0))

# Create color palette
colors <- list(
  nhl = "#C8102E",
  population = "#003f7f", 
  background = "#F8F9FA",
  text_dark = "#1A1A1A",
  text_light = "#6B7280"
)

# Main visualization
p_main <- comparison_data %>%
  select(month_name, pct_population, pct_nhl) %>%
  pivot_longer(cols = c(pct_population, pct_nhl), 
               names_to = "group", values_to = "percentage") %>%
  mutate(
    group = case_when(
      group == "pct_population" ~ "Canadian Population",
      group == "pct_nhl" ~ "Canadian NHL Players"
    ),
    group = factor(group, levels = c("Canadian Population", "Canadian NHL Players"))
  ) %>%
  ggplot(aes(x = month_name, y = percentage, fill = group)) +
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = colors$nhl) +
  geom_col(position = "dodge", alpha = 0.9, width = 0.8) +
  scale_fill_manual(
    values = c("Canadian Population" = colors$population, 
               "Canadian NHL Players" = colors$nhl),
    name = ""
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(comparison_data$pct_nhl, comparison_data$pct_population) * 1.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "The Birth Month Advantage in Hockey",
    subtitle = "<span style='color:#C8102E'>**Canadian NHL players**</span> are dramatically over-represented in early birth months,<br>especially January-March, compared to the <span style='color:#003f7f'>**Canadian population**</span>.",
    x = "Birth Month",
    y = "Percentage of births",
    caption = "**Data:** Statistics Canada, NHL API | **R:** tidyverse, ggplot2 | **Graphic:** Louie Christopher Wee"
  ) +
  theme_minimal(base_family = font_family) +
  theme(
    plot.background = element_rect(fill = colors$background, color = NA),
    panel.background = element_rect(fill = colors$background, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "white", size = 0.5),
    
    plot.title = element_text(
      size = 22, face = "bold", color = colors$text_dark,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      size = 12, color = colors$text_light, lineheight = 1.4,
      margin = margin(b = 5)
    ),
    plot.caption = element_textbox_simple(
      size = 9, color = colors$text_light, hjust = 0, halign = 1,
      margin = margin(t = 5, b = 5)
    ),
    
    axis.title = element_text(size = 11, color = colors$text_dark),
    axis.text = element_text(size = 10, color = colors$text_dark),
    
    legend.position = "none"
  )

# Display
print(p_main)
```

**🎓 Learning Outcomes**

- **Data Validation:** Confirming social science theories with empirical data
- **Comparative Analysis:** Population vs subset comparison techniques
- **Advanced ggplot2:** Color coding in text, custom theming, annotations
- **Typography:** Professional font integration with showtext

**📚 References**

- Malcolm Gladwell. "Outliers: The Story of Success"
- TidyTuesday 2024 Week 2 Dataset
- Statistics Canada Birth Records
- NHL Player Database
