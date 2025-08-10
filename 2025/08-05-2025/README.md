# 📊 TidyTuesday Week 32 (August 5, 2025): OECD Income Inequality & Redistribution

## 🎯 Overview

**The 2020 Redistribution Champions vs Laggards:** A slope graph visualization comparing the effectiveness of tax and transfer systems in reducing income inequality across OECD countries.

## 📁 Data Source

- **Dataset:** OECD Income Distribution Database
- **URL:** [TidyTuesday 2025-08-05](https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-08-05/readme.md)
- **Year Focus:** 2020 (for consistent cross-country comparison)
- **Metrics:** Gini coefficients (pre-tax and post-tax)

## 🔍 Key Findings

### Top 3 Redistributors (2020)

1. **Belgium:** 50.1% reduction
2. **Austria:** 45.3% reduction
3. **Ireland:** 44.7% reduction

### Bottom 3 Redistributors (2020)

1. **Bulgaria:** 24.8% reduction
2. **USA:** 27.8% reduction
3. **Japan:** 27.9% reduction

## 📈 Visualization Details

### Chart Type

**Slope Graph** - Shows the journey from pre-tax to post-tax inequality

### Design Choices

- **Color Scheme:**
    - Dark Blue: Top performers
    - Dark Orange: Bottom performers
    - Background: `#FAFAF8` (warm white)
- **Typography:**
    - Headers: Outfit (Google Font)
    - Body: Source Sans Pro
- **Visual Elements:**
    - Steeper slopes indicate more effective redistribution
    - Percentage labels on the right show reduction achieved
    - Clean minimal design with no gridlines

## 📖 The Narrative

**"Champions vs Laggards in Inequality Reduction"**

The visualization reveals a stark divide in how effectively countries use their tax and transfer systems to reduce inequality. Belgium cuts its inequality in half (50.1% reduction), transforming a Gini coefficient of 0.50 to 0.25. In contrast, the United States and Bulgaria achieve only about 25-28% reduction, leaving significant inequality intact after redistribution.

The slope graph format makes this policy effectiveness immediately visible - the steeper the downward slope, the more successful the country's redistribution system. The gap between dark blue and dark orange lines represents different political choices about the role of government in addressing inequality.

## 📊 Data Processing Notes

- Filtered for countries with complete 2020 data
- Calculated redistribution percentage: `(gini_pretax - gini_posttax) / gini_pretax * 100`
- Removed countries with missing values to ensure valid comparisons

## Code

```r
# Load libraries ----
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Typography setup ----
font_add_google("Outfit", "outfit")
font_add_google("Source Sans Pro", "source")
showtext_auto()

# Color palette ----
bg_color <- "#FAFAF8"
text_color <- "#2D3436"
top_color <- "darkblue"    # Dark Blue for top performers
bottom_color <- "darkorange"  # Dark Orange for bottom performers
grid_color <- "#E0E0E0"

# === DATA IMPORT ==============================================================

# Read TidyTuesday data ----
income_inequality_processed <- readr::read_csv(
'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_processed.csv'
)

# === DATA WRANGLING ===========================================================

# Clean and prepare data ----
inequality_data <- income_inequality_processed %>%
rename(
country = Entity,
code = Code,
year = Year,
gini_pretax = gini_mi_eq,
gini_posttax = gini_dhi_eq
) %>%
mutate(
redistribution_effect = gini_pretax - gini_posttax,
redistribution_pct = (redistribution_effect / gini_pretax) * 100
) %>%
filter(
![is.na](http://is.na/)(gini_pretax),
![is.na](http://is.na/)(gini_posttax)
)

# Get 2020 data for all countries ----
# Using 2020 for consistent comparison across all countries at the same point in time
data_2020 <- inequality_data %>%
filter(year == 2020) %>%
filter(![is.na](http://is.na/)(redistribution_pct))  # Ensure we have valid redistribution data

# Identify top 3 and bottom 3 redistributors ----
top_3 <- data_2020 %>%
arrange(desc(redistribution_pct)) %>%
slice_head(n = 3) %>%
pull(country)

bottom_3 <- data_2020 %>%
arrange(redistribution_pct) %>%
slice_head(n = 3) %>%
pull(country)

# Prepare slope graph data - only top and bottom performers ----
slope_data <- data_2020 %>%
filter(country %in% c(top_3, bottom_3)) %>%
mutate(
country_label = case_when(
country == "United States" ~ " USA",
country == "United Kingdom" ~ "UK",
TRUE ~ country
),
performance = case_when(
country %in% top_3 ~ "top",
country %in% bottom_3 ~ "bottom"
)
)

# Reshape for slope graph ----
slope_long <- slope_data %>%
select(country, country_label, gini_pretax, gini_posttax, redistribution_pct, performance) %>%
pivot_longer(
cols = c(gini_pretax, gini_posttax),
names_to = "type",
values_to = "gini"
) %>%
mutate(
x = ifelse(type == "gini_pretax", 1, 2)
)

# === SLOPE GRAPH VISUALIZATION ================================================

slope_plot <- ggplot(slope_long, aes(x = x, y = gini, group = country)) +

# Background grid ----
geom_vline(xintercept = c(1, 2), color = grid_color, size = 0.5) +

# Slope lines ----
geom_line(
aes(color = performance),
size = 1,
alpha = 0.9
) +

# Points ----
geom_point(
aes(color = performance),
size = 2
) +

# Country labels (left side) ----

# Country labels (right side) ----
geom_text(
data = filter(slope_long, x == 2),
aes(label = country_label,
color = performance),
hjust = -0.1,
size = 4,
family = "source",
fontface = "bold"
) +

# Percentage improvement labels ----
geom_text(
data = filter(slope_long, x == 2),
aes(label = paste0("-", round(redistribution_pct, 1), "%"),
color = performance),
hjust = -1.8,
size = 4,
family = "outfit",
fontface = "bold"
) +

# Scale adjustments ----
scale_x_continuous(
limits = c(0.5, 2.8),
breaks = c(1, 2),
labels = c("Pre-tax\ninequality", "Post-tax\ninequality"),
position = "top"
) +

scale_y_continuous(
limits = c(min(slope_long$gini) - 0.02, max(slope_long$gini) + 0.02),
breaks = seq(0.20, 0.55, 0.05),
labels = number_format(accuracy = 0.01),
expand = c(0, 0)
) +

scale_color_manual(
values = c(
"top" = top_color,
"bottom" = bottom_color
),
guide = "none"
) +

# Labels ----
labs(
title = "**The 2020 Redistribution Champions vs Laggards:** \n Best and worst at reducing inequality",
subtitle = paste0(
"Gini coefficient ranges from 0 to 1 with higher values indicating higher inequality.",
" Comparing the <span style='color:", top_color, "'>**top 3**</span> and <span style='color:", bottom_color, "'>**bottom 3**</span> OECD countries in reducing inequality through taxes and transfers.<br>"

),
y = "Gini coefficient",
x = NULL,
caption = "**Data:** OECD Income Distribution Database (2020) | **R:** tidyverse, ggplot2 | **Graphic:** Louie Christopher Wee"
) +

# Theme ----
theme_minimal(base_family = "outfit", base_size = 12) +
theme(
# Canvas
plot.background = element_rect(fill = bg_color, color = NA),
panel.background = element_rect(fill = bg_color, color = NA),

# Grid
panel.grid = element_blank(),

# Typography
plot.title = element_textbox_simple(
size = 18,
face = "bold",
color = text_color,
margin = margin(b = 10),
lineheight = 1.2
),
plot.subtitle = element_textbox_simple(
size = 11,
color = text_color,
margin = margin(t = 10, b = 20),
lineheight = 1.3
),
plot.caption = element_textbox_simple(
size = 9,
color = "#666666",
margin = margin(t = 20),
hjust = 0, halign = 1
),

# Axes
axis.title.y = element_text(
size = 11,
color = text_color,
face = "bold",
margin = margin(r = 10)
),
axis.text.y = element_text(
size = 10,
color = text_color
),
axis.text.x = element_text(
size = 12,
color = text_color,
face = "bold"
),
axis.ticks = element_blank(),

# Margins
plot.margin = margin(20, 40, 20, 40)
)

# Display plot ----
slope_plot
```
