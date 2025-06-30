# Week 13 (April 1, 2025) - Pokemon Power Evolution

Data Source: TidyTuesday Pokemon Dataset | Libraries: tidyverse, ggplot2, ggtext, showtext

## 🎯 The Story

Pokemon power evolution reveals an interesting trend: while legendary Pokemon drove most of the power growth from Generation 1 to Generation 4, regular Pokemon have been steadily catching up. From Gen 4 to Gen 7, overall power levels remained stable even as legendary power decreased, suggesting that regular Pokemon are closing the gap.

## 📝 Code

```r
# Pokemon Power Evolution: #TidyTuesday 2025 Week 13
# Created by: Louie Christopher Wee
# Data: Pokemon Dataset from TidyTuesday
# Libraries used: tidyverse, ggplot2, ggtext, showtext

# Load required libraries
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
pokemon_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

# Data preparation
pokemon_clean <- pokemon_df %>%
  # Remove alternate forms and megas to focus on base Pokemon
  filter(id <= 802) %>%
  # Create total base stat
  mutate(
    total_stats = hp + attack + defense + special_attack + special_defense + speed,
    is_legendary = base_experience >= 270 # Rough cutoff for legendaries
  )

# Create generation summary with better labels - only Gen 1, 4, 7
gen_summary <- pokemon_clean %>%
  filter(generation_id %in% c(1, 4, 7)) %>%
  group_by(generation_id) %>%
  summarise(
    avg_total_all = mean(total_stats, na.rm = TRUE),
    avg_total_regular = mean(total_stats[!is_legendary], na.rm = TRUE),
    avg_total_legendary = mean(total_stats[is_legendary], na.rm = TRUE),
    count_total = n(),
    count_legendary = sum(is_legendary, na.rm = TRUE),
    pct_legendary = round(100 * count_legendary / count_total, 1),
    .groups = "drop"
  ) %>%
  mutate(
    generation = paste0("Gen ", generation_id),
    release_year = case_when(
      generation_id == 1 ~ 1996,
      generation_id == 4 ~ 2007,
      generation_id == 7 ~ 2016
    )
  )

# Define clean color palette
colors <- c(
  "Regular Pokemon" = "#1A237E",    # Dark blue
  "Legendary Pokemon" = "#B71C1C",  # Dark red
  "Overall Average" = "#9C88D4"     # Light purple
)

# Create the main plot
p1 <- ggplot(data = gen_summary) +
  # Background area for context
  geom_ribbon(aes(x = generation_id, ymin = 300, ymax = avg_total_all),
              fill = "#ECF0F1", alpha = 0.3) +
  # Lines connecting the three generations
  geom_line(aes(x = generation_id, y = avg_total_regular),
            color = "#1A237E", size = 2, alpha = 0.8) +
  geom_line(aes(x = generation_id, y = avg_total_legendary),
            color = "#B71C1C", size = 2, alpha = 0.8) +
  geom_line(aes(x = generation_id, y = avg_total_all),
            color = "#9C88D4", size = 3, alpha = 0.9) +
  # Points for emphasis
  geom_point(aes(x = generation_id, y = avg_total_all),
             color = "#9C88D4", size = 4, alpha = 0.9) +
  geom_point(aes(x = generation_id, y = avg_total_regular),
             color = "#1A237E", size = 3, alpha = 0.8) +
  geom_point(aes(x = generation_id, y = avg_total_legendary),
             color = "#B71C1C", size = 3, alpha = 0.8)

  # Scales and aesthetics
  scale_x_continuous(
    breaks = c(1, 4, 7),
    labels = c("Gen 1\n(1996)", "Gen 4\n(2007)", "Gen 7\n(2016)")
  ) +
  scale_y_continuous(
    limits = c(300, 700),
    breaks = seq(300, 700, 100),
    labels = scales::number_format()
  ) +
  # Labels and theme
  labs(
    title = "Regular Pokémon are catching up!",
    subtitle = "Power growth from Gen 1 to Gen 4 was driven mostly by <span style='color:#B71C1C;'>**Legendary Pokémon**</span>, while power from <span style='color:#9C88D4;'>**All Pokémon**</span> maintained from Gen 4 to Gen 7 even when legendary power dropped, suggesting <span style='color:#1A237E;'>**Regular Pokémon**</span> are catching up.",
    x = NULL,
    y = "Average Total Base Stats",
    caption = "**Data**: Pokemon Dataset  | **R**: tidyverse, ggplot2  | **Graphic:** Louie Christopher Wee"
  ) +

  theme_minimal(base_family = 'roboto') +
  theme(
    plot.title = element_textbox_simple(size = 20, face = 'bold', color = '#2C3E50',
                             margin = margin(b = 12), family = 'roboto_condensed'),
    plot.subtitle = element_textbox_simple(size = 12, color = "#34495E",
                                margin = margin(b = 25), lineheight = 1.4,
                                family = "roboto"),
    plot.caption = element_textbox_simple(size = 9, color = "#7F8C8D",
                               margin = margin(t = 20), family = "roboto", halign = 1),
    axis.title.y = element_text(size = 11, color = "#2C3E50",
                               margin = margin(r = 10), family = "roboto"),
    axis.text = element_text(size = 10, color = "#2C3E50", family = "roboto"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ECF0F1", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 35, 20, 25)
  )

# Print and save plot
print(p1)
```

## 💡 Key Insights

- Generation 1 (1996) had the lowest overall power levels with regular Pokemon averaging around 400 base stats
- Generation 4 (2007) saw the biggest power jump, driven primarily by legendary Pokemon reaching ~600+ base stats
- Generation 7 (2016) shows power convergence - legendary power decreased while regular Pokemon power increased
- The gap between regular and legendary Pokemon is narrowing, suggesting better game balance over time
