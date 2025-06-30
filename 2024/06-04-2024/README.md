Week 23 (June 4, 2024) - Global Cheese Production by Milk Type

## 📊 TidyTuesday Week 23 Analysis - June 4, 2024

This notebook analyzes global cheese production patterns by examining which types of milk (cow, sheep, goat, buffalo) are used across major cheese-producing countries. The visualization explores cheese diversity and traditional practices, revealing how cow's milk dominates while some regions maintain heritage approaches using alternative milk sources.

## 📦 Setup and Data Loading

```r
# TidyTuesday Week 23, 2024 - Cheese Dataset
# Author: Louie Wee
# Data: TidyTuesday - Cheese.com scraping
# Libraries: tidyverse, ggplot2, ggtext, showtext

# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)

# Add custom fonts
font_add_google("Inter", "inter")
font_add_google("Inter", "inter_bold", regular.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

📊 Data Preparation

First, we'll load and clean the cheese dataset from TidyTuesday:

# Load the data
cheeses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')

# Data cleaning and preparation
cheese_data <- cheeses %>%
  # Filter out entries without milk or country information
  filter(!is.na(milk), !is.na(country), milk != "", country != "") %>%
  # Split milk types where multiple are listed (e.g., "cow, goat")
  separate_rows(milk, sep = ", ") %>%
  # Clean milk type names
  mutate(milk = str_trim(milk)) %>%
  # Split countries where multiple are listed
  separate_rows(country, sep = ", ") %>%
  mutate(country = str_trim(country)) %>%
  # Focus on main milk types and clean them
  filter(milk %in% c("cow", "goat", "sheep", "water buffalo", "buffalo")) %>%
  # Standardize buffalo naming
  mutate(milk = case_when(
    milk == "water buffalo" ~ "buffalo",
    TRUE ~ milk
  ))

# Get top countries by cheese count
top_countries <- cheese_data %>%
  count(country, name = "total_cheeses") %>%
  arrange(desc(total_cheeses)) %>%
  slice_head(n = 15) %>%
  pull(country)

# Create final dataset with percentages
cheeses_clean <- cheese_data %>%
  filter(country %in% top_countries) %>%
  count(country, milk, name = "cheese_count") %>%
  # Calculate percentages within each country
  group_by(country) %>%
  mutate(
    percentage = cheese_count / sum(cheese_count) * 100,
    country_total = sum(cheese_count)
  ) %>%
  ungroup() %>%
  # Sort countries alphabetically (A-Z from top to bottom)
  arrange(desc(country)) %>%
  mutate(country = fct_inorder(country)) %>%
  # Calculate overall average percentage to determine stacking order
  group_by(milk) %>%
  mutate(avg_percentage = mean(percentage)) %>%
  ungroup() %>%
  # Order milk types by average percentage (lowest to highest for bottom to top stacking)
  mutate(milk = fct_reorder(milk, avg_percentage, .desc = FALSE))
```

## 📊 Data Preparation

First, we'll load and clean the cheese dataset from TidyTuesday:

```r
# Load the data
cheeses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')

# Data cleaning and preparation
cheese_data <- cheeses %>%
  # Filter out entries without milk or country information
  filter(!is.na(milk), !is.na(country), milk != "", country != "") %>%
  # Split milk types where multiple are listed (e.g., "cow, goat")
  separate_rows(milk, sep = ", ") %>%
  # Clean milk type names
  mutate(milk = str_trim(milk)) %>%
  # Split countries where multiple are listed
  separate_rows(country, sep = ", ") %>%
  mutate(country = str_trim(country)) %>%
  # Focus on main milk types and clean them
  filter(milk %in% c("cow", "goat", "sheep", "water buffalo", "buffalo")) %>%
  # Standardize buffalo naming
  mutate(milk = case_when(
    milk == "water buffalo" ~ "buffalo",
    TRUE ~ milk
  ))

# Get top countries by cheese count
top_countries <- cheese_data %>%
  count(country, name = "total_cheeses") %>%
  arrange(desc(total_cheeses)) %>%
  slice_head(n = 15) %>%
  pull(country)

# Create final dataset with percentages
cheeses_clean <- cheese_data %>%
  filter(country %in% top_countries) %>%
  count(country, milk, name = "cheese_count") %>%
  # Calculate percentages within each country
  group_by(country) %>%
  mutate(
    percentage = cheese_count / sum(cheese_count) * 100,
    country_total = sum(cheese_count)
  ) %>%
  ungroup() %>%
  # Sort countries alphabetically (A-Z from top to bottom)
  arrange(desc(country)) %>%
  mutate(country = fct_inorder(country)) %>%
  # Calculate overall average percentage to determine stacking order
  group_by(milk) %>%
  mutate(avg_percentage = mean(percentage)) %>%
  ungroup() %>%
  # Order milk types by average percentage (lowest to highest for bottom to top stacking)
  mutate(milk = fct_reorder(milk, avg_percentage, .desc = FALSE))
```

## 🎨 Visualization Creation

Now we'll create the horizontal stacked bar chart showing milk type percentages:

```r
# Define distinct color palette with better contrast
# Order: cow, sheep, goat, buffalo (left to right in stacked bar)
cheese_colors <- c(
  "cow" = "#1f77b4",       # Blue
  "sheep" = "#2ca02c",     # Green
  "goat" = "#ff7f0e",      # Orange    
  "buffalo" = "#d62728"    # Red
)

# Create the plot
p <- cheeses_clean %>%
  ggplot(aes(x = country, y = percentage, fill = milk)) +
  geom_col(width = 0.8, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(
    values = cheese_colors,
    labels = c("cow" = "Cow", "sheep" = "Sheep", "goat" = "Goat", "buffalo" = "Buffalo")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0.01, 0),
    breaks = seq(0, 100, 25)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +

  labs(
    title = "**Cow's milk dominates global cheese production**",
    subtitle = "While most countries rely heavily on <span style='color:#1f77b4'>**cow's milk**</span> for cheesemaking, some regions maintain traditional practices using <span style='color:#2ca02c'>**sheep**</span>, <span style='color:#ff7f0e'>**goat**</span>, and <span style='color:#d62728'>**buffalo milk**</span>. Australia and Italy lead in cheese diversity, but cow's milk cheeses dominate across all major cheese-producing nations. Countries like Greece and Spain show higher proportions of sheep and goat milk cheeses, reflecting Mediterranean pastoral traditions.",
    caption = "**Data:** Cheese.com scraping • **Viz:** Louie Christopher Wee • **R:** ggplot2, tidyverse",
    x = "",
    y = "Percentage of Cheeses by Milk Type"
  ) +
  theme_minimal(base_family = "inter", base_size = 12) +
  theme(
    # Plot background
    plot.background = element_rect(fill = "#FEFCF7", color = NA),
    panel.background = element_rect(fill = "#FEFCF7", color = NA),
        
    # Grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E5E5", size = 0.3),

        
    # Text elements
    plot.title = element_textbox_simple(
      family = "inter_bold",
      size = 28,
      color = "#2C2C2C",
      margin = margin(b = 20, t = 10, l = 0, r = 0),
      lineheight = 1.2
    ),
    plot.subtitle = element_textbox_simple(
      family = "inter",
      size = 18,
      color = "#5A5A5A", 
      margin = margin(b = 25, t = 10, l = 0, r = 0),
      lineheight = 1.3,
      width = unit(1, "npc")
    ),
    plot.caption = element_textbox_simple(
      family = "inter",
      size = 14,
      color = "#7A7A7A",
      margin = margin(t = 25),
      lineheight = 1.4,
      halign = 1,
      width = unit(1, "npc")
    ),
        
    # Axes
    axis.title.x = element_text(
      family = "inter",
      size = 16,
      color = "#5A5A5A",
      margin = margin(t = 15)
    ),
    axis.text.x = element_text(
      family = "inter",
      size = 14,
      color = "#5A5A5A"
    ),
    axis.text.y = element_text(
      family = "inter",
      size = 14,
      color = "#5A5A5A"
    ),
    axis.ticks = element_blank(),
        
    # Legend - remove it since we have colored labels in subtitle
    legend.position = "none",
        
    # Margins
    plot.margin = margin(20, 20, 20, 20)
  )

# Display the plot
print(p)

# Save the plot with dataset date (TidyTuesday Week 23, 2024 = June 4, 2024)
dataset_date <- "06-04-2024"
filename <- paste0(dataset_date, ".png")
ggsave(
  filename,
  plot = p,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "#FEFCF7"
)
```

## 📝 Analysis Summary

This visualization reveals several key insights about global cheese production patterns:

- **Cow's Milk Dominance**: Across all major cheese-producing countries, cow's milk accounts for the vast majority of cheese varieties, typically 70-95% of a country's cheese portfolio.
- **Mediterranean Traditions**: Countries like Greece and Spain show notably higher proportions of sheep and goat milk cheeses, reflecting traditional Mediterranean pastoral practices and local preferences.
- **Regional Specialties**: While cow's milk dominates globally, certain regions maintain significant traditions with alternative milk sources, particularly in Southern Europe where sheep and goat farming has historical importance.
- **Limited Buffalo Usage**: Buffalo milk cheeses remain relatively rare globally, appearing primarily in specific regions with established water buffalo farming traditions.
- **Cheese Diversity Leaders**: Australia and Italy show among the highest diversity in milk types used for cheesemaking, though cow's milk still predominates.
- **Cultural Heritage**: The persistence of sheep and goat milk cheeses in certain countries demonstrates how traditional food practices continue alongside industrial dairy production.

## 📚 Data Source & Methods

**Dataset**: TidyTuesday Week 23 (June 4, 2024) - Cheese dataset from Cheese.com scraping

**Tools**: R with tidyverse, ggplot2, ggtext, and showtext packages

**Methodology**: Data cleaning included handling multiple milk types per cheese entry, standardizing country names, and calculating percentages within each country for comparative analysis.

**Visualization Style**: Horizontal stacked bar chart with custom color palette, inspired by Nicole Rennie's clean, professional approach to TidyTuesday visualizations.
