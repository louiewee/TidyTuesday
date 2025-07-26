# Week 30 (July 22 2025) - MTA Art Collection

## 📊 Project Overview

**Dataset:** MTA Arts & Design Collection

**Week:** 30, 2025 (July 22, 2025)

**Visualization Type:** Waterfall Chart with Material Breakdown

**Theme:** Building NYC's Underground Gallery

**Author:** Louie Christopher Wee

**Date Created:** July 25, 2025

---

## 🎯 Key Insights

**Main Finding:** The MTA's art collection has grown steadily since the 1980s, with 381 total artworks transforming subway stations into one of the world's largest public art exhibitions.

**Notable Patterns:**

- Peak growth in the 2010s with +136 artworks added
- Consistent decade-over-decade expansion showing sustained investment
- Material composition dominated by durable installations (metal, mosaics, ceramics)
- Strategic use of materials that withstand subway environment conditions

---

## 📈 Visualization

**Design Choices:**

- Waterfall chart shows cumulative growth over decades
- Stacked total bar reveals material composition (top 3 + other)
- Colored keywords in subtitle eliminate need for legend
- Dark color palette (red, blue, green, gray) for professional appearance
- Material counts displayed inside each segment for clarity

---

## 💾 Dataset Information

**Source:** MTA Arts & Design

---

## 🔍 Data Exploration

**Data Quality:**

- 381 total artworks in main dataset
- Installation dates range from 1980s-2020s
- Material descriptions require cleaning and categorization
- No missing station names or essential fields

**Material Categories (Top 3):**

1. **Metal/Bronze** - Durable outdoor installations
2. **Mosaic/Tile** - Traditional subway art medium
3. **Ceramic/Porcelain** - Detailed artistic works
4. **Other** - Glass, stone, murals, mixed media

---

## 💻 R Code

### Setup and Data Loading

```r
# TidyTuesday 2025-07-22: MTA Art Collection Waterfall Chart
# Author: Louie Wee

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Load fonts
font_add_google("Inter", "Inter")
font_add_google("Merriweather", "Merriweather")
showtext_auto()

# Load data
mta_art <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/mta_art.csv')

```

### Data Preparation

```r
# Prepare waterfall data
waterfall_data <- mta_art %>%
  filter(!is.na(art_date)) %>%
  mutate(decade = floor(art_date / 10) * 10) %>%
  count(decade, name = "added") %>%
  arrange(decade) %>%
  mutate(
    cumulative = cumsum(added),
    start = lag(cumulative, default = 0),
    end = cumulative,
    decade_label = paste0(decade, "s"),
    bar_label = paste0("+", added),
    total_label = comma_format()(cumulative)
  )

# Prepare material breakdown
material_breakdown <- mta_art %>%
  filter(!is.na(art_date), !is.na(art_material)) %>%
  mutate(
    material_clean = case_when(
      str_detect(tolower(art_material), "mosaic|tile|terrazzo") ~ "Mosaic/Tile",
      str_detect(tolower(art_material), "bronze|metal|steel|aluminum") ~ "Metal/Bronze",
      str_detect(tolower(art_material), "porcelain|ceramic|enamel") ~ "Ceramic/Porcelain",
      str_detect(tolower(art_material), "glass|stained") ~ "Glass Art",
      str_detect(tolower(art_material), "mural|painting|acrylic") ~ "Mural/Painting",
      str_detect(tolower(art_material), "stone|marble|granite") ~ "Stone",
      TRUE ~ "Other"
    )
  ) %>%
  count(material_clean, sort = TRUE) %>%
  mutate(
    material_final = case_when(
      row_number() <= 3 ~ material_clean,
      TRUE ~ "Other"
    )
  ) %>%
  group_by(material_final) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  mutate(
    cumulative_material = cumsum(n),
    start_material = lag(cumulative_material, default = 0),
    end_material = cumulative_material,
    total_artworks = sum(n)
  )

```

### Calculate Positions and Colors

```r
# Calculate positions
waterfall_main <- waterfall_data %>%
  mutate(x_pos = row_number())

total_x_pos <- nrow(waterfall_main) + 1.5

material_breakdown <- material_breakdown %>%
  mutate(x_pos = total_x_pos)

# Define colors and subtitle
material_colors <- c(
  "Metal/Bronze" = "#8B0000",
  "Mosaic/Tile" = "#003366",
  "Ceramic/Porcelain" = "#006400",
  "Glass Art" = "#006400",
  "Other" = "#808080"
)

colored_subtitle <- paste0(
  "The MTA's art collection has grown steadily since the 1980s, with each decade adding dozens of new works. ",
  "The total collection showcases diverse materials, from durable <span style='color:#8B0000'>**metal**</span> installations to ",
  "intricate <span style='color:#003366'>**mosaics**</span> and delicate <span style='color:#006400'>**ceramics**</span>, ",
  "creating one of the world's largest public art exhibitions."
)

```

### Create Visualization

```r
# Create plot
waterfall_plot <- ggplot() +
  geom_segment(
    data = waterfall_main,
    aes(x = x_pos + 0.35, xend = x_pos + 0.65, y = end, yend = end),
    color = "#A7A9AC", linetype = "dashed", size = 0.5, na.rm = TRUE
  ) +
  geom_rect(
    data = waterfall_main,
    aes(xmin = x_pos - 0.35, xmax = x_pos + 0.35, ymin = start, ymax = end),
    fill = "#FF6319", alpha = 0.9
  ) +
  geom_rect(
    data = material_breakdown,
    aes(xmin = x_pos - 0.35, xmax = x_pos + 0.35,
        ymin = start_material, ymax = end_material, fill = material_final),
    alpha = 0.9
  ) +
  geom_text(
    data = waterfall_main,
    aes(x = x_pos, y = (start + end) / 2, label = bar_label),
    color = "white", fontface = "bold", size = 3, family = "Inter"
  ) +
  geom_text(
    data = material_breakdown,
    aes(x = x_pos, y = (start_material + end_material) / 2, label = n),
    color = "white", fontface = "bold", size = 3, family = "Inter"
  ) +
  geom_text(
    data = waterfall_main,
    aes(x = x_pos, y = end + 20),
    label = waterfall_main$total_label,
    color = "#2c2c2c", size = 3, family = "Inter", fontface = "bold", vjust = 0
  ) +
  annotate(
    "text", x = total_x_pos, y = max(material_breakdown$total_artworks) + 20,
    label = paste0(max(material_breakdown$total_artworks), "\nArtworks"),
    color = "#2c2c2c", size = 3, family = "Inter", fontface = "bold", vjust = 0
  ) +
  scale_fill_manual(values = material_colors, guide = "none") +
  scale_x_continuous(
    breaks = c(waterfall_main$x_pos, total_x_pos),
    labels = c(waterfall_main$decade_label, "Total"),
    limits = c(0.5, total_x_pos + 0.5), expand = c(0, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.15)), labels = comma_format()) +
  labs(
    title = "Building NYC's Underground Gallery",
    subtitle = colored_subtitle,
    x = "", y = "",
    caption = "Data: MTA Arts & Design | TidyTuesday 2025 Week 30 | Graphic: Louie Wee | R: tidyverse, ggplot2"
  ) +
  theme_minimal(base_family = "Inter") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#f0f0f0", size = 0.3),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 10, color = "#2c2c2c"),
    plot.title = element_text(family = "Merriweather", size = 24, color = "#1a1a1a",
                             face = "bold", margin = margin(b = 10), hjust = 0),
    plot.subtitle = element_markdown(family = "Inter", size = 13, color = "#404040",
                                    lineheight = 1.3, margin = margin(b = 30), hjust = 0),
    legend.position = "none",
    plot.caption = element_text(family = "Inter", size = 10, color = "#666666",
                               hjust = 0, margin = margin(t = 25)),
    plot.margin = margin(40, 50, 40, 50)
  )
  
# Display and save
print(waterfall_plot)

```

---

## 🔗 Resources

**Data Source:** [TidyTuesday GitHub](https://github.com/rfordatascience/tidytuesday)

**Fonts:** Google Fonts (Inter, Merriweather)
