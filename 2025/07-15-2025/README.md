# Week 28: British Library Funding Analysis - Government Dependency Trends

📊 TidyTuesday 2025 Week 28 | 📅 July 15, 2025 | 👨‍💻 Louie Wee

Dataset: British Library Funding (1998-2023) | Focus: Government dependency analysis and strategic funding trends

🔗 Dataset Source

- GitHub: https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv
- Source: British Library Annual Reports 1998-2023
- Data Type: Financial time series with funding breakdown by source

🎯 Key Insights & Findings

- Government Grant-in-Aid consistently provides 65-85% of total funding (25-year average: 74.2%)
- 2006 anomaly: Lowest government dependency (64.5%) due to exceptional £32M voluntary donations
- Funding strategy demonstrates optimal balance: stable government support + strategic philanthropy
- 2020 data confirms resilience: £125.9M total funding with 86% government dependency during COVID

📈 Data Analysis Approach

- Focus: Government dependency trends over 25 years (1998-2023)
- Visualization: Line plot with highlighted anomalies and strategic annotations
- Statistical insight: Identified 2006 as outlier due to exceptional voluntary income

🛠️ Technical Implementation

- Libraries: tidyverse, ggplot2, ggtext, showtext, scales
- Typography: Google Fonts (Roboto family) for professional appearance
- Color Palette: Professional blue (#2E4057) with dark red accent (#C73E1D) for anomalies
- Output: High-resolution PNG (300 DPI, 14x9 inches) suitable for publication

📊 Historical Context & Research Insights

- 2006 coincided with Business & IP Centre opening and British Library Sounds launch
- Financial crisis (2008-2012) showed funding resilience with minimal government cuts
- Post-2015 stabilization reflects mature funding strategy balancing public/private sources

🔢 Data Summary & Methodology

- Time Period: 25 years (1998-2023) - complete dataset, no missing years
- Government Dependency Range: 64.5% (2006) to 86.3% (2020)
- 2006 Breakdown: £159.2M total (£102.6M gov + £31.9M voluntary + £22.8M services)
- Design Philosophy: Clean, minimal annotation highlighting single key insight

🎆 GitHub Repository Description

**TidyTuesday Week 28: British Library Funding Stability Analysis**

Exploring 25 years of British Library funding data reveals remarkable institutional resilience. This analysis focuses on government dependency trends, uncovering how strategic philanthropy enhances rather than replaces stable public investment.

**Key Discovery:** The 2006 'funding dip' represents a success story - exceptional £32M in voluntary donations temporarily reduced government dependency while enabling major digital initiatives.

**Technical Approach:** Clean R visualization using ggplot2 with strategic color coding to highlight anomalies. Professional typography and minimal design philosophy prioritize data story over decoration.

**Files:** R script, high-res PNG output

Code

```r
# TidyTuesday 2025 Week 28: British Library Funding - Government Dependency Analysis
# Author: Louie Wee
# Date: July 15, 2025

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Load fonts
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# Load data
bl_funding <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')

# Data exploration - check for missing years
cat("Years in dataset:", sort(bl_funding$year), "\n")
cat("2020 data exists:", 2020 %in% bl_funding$year, "\n")

# Verify 2020 data
year_2020_data <- bl_funding %>% filter(year == 2020)
cat("2020 funding: Total £", year_2020_data$nominal_gbp_millions, "M, GIA £", year_2020_data$gia_gbp_millions, "M\n")

# Calculate government dependency over time
df_dependency <- bl_funding %>%
  select(year, gia_gbp_millions, nominal_gbp_millions, voluntary_gbp_millions, gia_as_percent_of_peak_gia) %>%
  mutate(
    govt_percentage = (gia_gbp_millions / nominal_gbp_millions) * 100,
    peak_gia_percentage = gia_as_percent_of_peak_gia * 100
  ) %>%
  filter(!is.na(govt_percentage)) %>%
  arrange(year)

# Find key data points
lowest_govt_dep <- df_dependency %>% slice_min(govt_percentage)
highest_govt_dep <- df_dependency %>% slice_max(govt_percentage)
year_2006_data <- df_dependency %>% filter(year == 2006)

cat("Lowest government dependency:", lowest_govt_dep$year, "at", round(lowest_govt_dep$govt_percentage, 1), "%\n")
cat("2006 voluntary income: £", year_2006_data$voluntary_gbp_millions, "M (unusually high)\n")

# Create focused government dependency visualization
p <- df_dependency %>%
  ggplot(aes(x = year, y = govt_percentage)) +
  geom_line(color = "#2E4057", size = 1.3, alpha = 0.9) +
  geom_point(color = "#2E4057", size = 3, alpha = 0.8) +
  # Highlight 2006 anomaly
  geom_point(data = filter(df_dependency, year == 2006), 
             aes(y = govt_percentage), color = "#C73E1D", size = 4) +
  scale_x_continuous(
    breaks = seq(1998, 2023, by = 5),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    limits = c(60, 88),
    breaks = seq(60, 85, by = 5)
  ) +
  labs(
    title = "British Library's growing funding: 25 years of government support",
    subtitle = "Government Grant-in-Aid consistently provides 65-85% of total funding, demonstrating remarkable stability in public investment despite economic turbulence and the digital transformation of library services.",
    x = NULL,
    y = "Government funding as % of total",
    caption = "**Data:** British Library Annual Reports | **R:** tidyverse, ggplot2 | **Graphic:** Louie Christopher Wee"
  ) +
  theme_minimal(base_family = "roboto", base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5", size = 0.3),
    plot.title = element_textbox_simple(
      family = "roboto_condensed",
      size = 20,
      face = "bold",
      color = "#2E4057",
      margin = margin(t = 20, b = 15)
    ),
    plot.subtitle = element_textbox_simple(
      family = "roboto",
      size = 13,
      color = "#5A5A5A",
      lineheight = 1.3,
      margin = margin(t = 5, b = 25),
      width = unit(8, "in")
    ),
    plot.caption = element_textbox_simple(
      family = "roboto",
      size = 9,
      color = "#7A7A7A",
      margin = margin(t = 20),
      width = unit(6, "in"),
      hjust = 1
    ),
    axis.title.y = element_text(
      family = "roboto",
      size = 12,
      color = "#5A5A5A",
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      family = "roboto",
      size = 11,
      color = "#5A5A5A"
    ),
    axis.ticks = element_blank(),
    plot.margin = margin(25, 25, 25, 25)
  )

# Add 2006 annotation only
p_annotated <- p +
  # 2006 anomaly annotation
  annotate(
    "text",
    x = 2009,
    y = 67,
    label = "2006: Lowest government dependency (64.5%)\ndue to exceptional £32M in voluntary donations\n(3x the typical £10M annual average)",
    family = "roboto",
    size = 3.2,
    color = "#C73E1D",
    hjust = 0,
    vjust = 0.5,
    lineheight = 1.1
  ) +
  annotate(
    "segment",
    x = 2008.5,
    xend = 2006.3,
    y = 67,
    yend = 64.8,
    color = "#C73E1D",
    arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
    size = 0.5
  )

# Display the plot
print(p_annotated)

# Create supplementary data table for 2006 context
year_2006_detail <- bl_funding %>% filter(year == 2006)

# Historical context years around 2006
cat("\n=== 2004-2023 VOLUNTARY INCOME CONTEXT ===\n")
bl_funding %>% 
  filter(year >= 2004 & year <= 2023) %>%
  select(year, voluntary_gbp_millions) %>%
  arrange(year) %>%
  mutate(context = case_when(
    year == 2006 ~ "*** EXCEPTIONAL YEAR ***",
    TRUE ~ ""
  )) %>%
  print()
```
