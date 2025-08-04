# Week 30 (July 29, 2025) - Netflix Engagement Report

🎬 **Data Source:** Netflix Engagement Report 2025

📅 **Week:** 2025-07-29 (Week 30)

---

**🎯 Key Insight**

**"Netflix Movies Outperform TV Shows in Peak Viewership"**

Despite the binge-watching era, movies consistently achieve higher individual view counts than even the most popular TV show episodes, with recent releases dominating both categories.

**📊 Dataset Overview**

- **Source:** Netflix Engagement Report 2025
- **Content:** Two datasets - movies.csv and shows.csv
- **Metric:** Views
- **Scope:** Netflix content with peak episode/movie viewership
- **Time Range:** Focused on 2015-2025 releases

**🔍 Analysis Approach**

- **Data Cleaning:** Combined movies and TV shows datasets
- **Peak Performance:** Used only the highest-viewed episode per TV show
- **Comparison:** Each title represented by its best performance
- **Temporal Focus:** Filtered to 2015+ for cleaner visualization

**💡 Key Findings**

- Movies consistently achieve higher peak viewership than TV episodes
- Recent content (2020+) significantly outperforms older releases
- The "streaming era" narrative may overstate TV show dominance
- Individual movie viewership remains strong despite binge culture

**🛠 Technical Stack**

- **Language:** R
- **Core Libraries:** tidyverse, ggplot2, ggtext, showtext, scales
- **Typography:** Inter, JetBrains Mono (via showtext)
- **Styling:** ggtext for rich text formatting
- **Data Processing:** dplyr for grouping and filtering

**📝 R Code Implementation**

```r
# TidyTuesday 2025 Week 30: Netflix Engagement Report
# Created by: Louie Wee
# Date: August 2, 2025
# Data: Netflix Engagement Report 2025

# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Load Google Fonts
font_add_google("Inter", "Inter")
font_add_google("JetBrains Mono", "JetBrains Mono")
showtext_auto()

# Load the Netflix data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')

# Prepare data for line chart by release date - using max views per title
netflix_content <- bind_rows(
movies %>% mutate(content_type = "Movie"),
shows %>% mutate(content_type = "TV Show")
) %>%
# Clean and prepare data
filter(!is.na(views), !is.na(release_date)) %>%
mutate(
views_millions = views / 1e6,
release_year = year(release_date),
content_type = factor(content_type, levels = c("Movie", "TV Show"))
) %>%
# Get only the episode/entry with max views for each title
group_by(title, content_type) %>%
slice_max(views_millions, n = 1, with_ties = FALSE) %>%
ungroup() %>%
# Focus on content from 2015 onwards for cleaner visualization
filter(release_year >= 2015)

# Create the main visualization
main_plot <- netflix_content %>%
ggplot(aes(x = release_date, y = views_millions, color = content_type)) +
geom_point(alpha = 0.6, size = 2) +
geom_smooth(method = "loess", se = TRUE, alpha = 0.2, size = 1.2) +
# Annotate highest movie
geom_point(data = netflix_content %>% filter(content_type == "Movie") %>% slice_max(views_millions, n = 1),
size = 4, alpha = 0.9) +
geom_text(data = netflix_content %>% filter(content_type == "Movie") %>% slice_max(views_millions, n = 1),
aes(label = paste0(str_trunc(title, 20))),
hjust = -0.1, vjust = 0.5, size = 3.5, color = "#E50914", fontface = "bold") +
# Annotate highest TV show
geom_point(data = netflix_content %>% filter(content_type == "TV Show") %>% slice_max(views_millions, n = 1),
size = 4, alpha = 0.9) +
geom_text(data = netflix_content %>% filter(content_type == "TV Show") %>% slice_max(views_millions, n = 1),
aes(label = paste0(str_sub(title, 1, 11))),
hjust = -0.1, vjust = 0.5, size = 3.5, color = "#2C3E50", fontface = "bold") +
scale_color_manual(
values = c("Movie" = "#E50914", "TV Show" = "#221F1F"),
guide = "none"  # Remove legend
) +
scale_x_date(
date_labels = "%Y",
date_breaks = "1 year",
expand = expansion(mult = c(0.02, 0.15))  # More space for annotations
) +
scale_y_continuous(
labels = function(x) paste0(x, "M"),
expand = expansion(mult = c(0, 0.05))
) +
labs(
title = "Netflix Movies Outperform TV Shows in Peak Viewership",
subtitle = "Despite the binge-watching era, <span style='color:#E50914'>movies</span> consistently achieve higher individual view counts than even the most popular <span style='color:#221F1F'>TV show episodes</span>, with recent releases dominating both categories.",
x = "",
y = "Views (millions)",
caption = "Data: Netflix Engagement Report 2025 | R: ggplot2, tidyverse | Graphic: Louie Christopher Wee"
) +
theme_minimal(base_family = "Inter") +
theme(
plot.background = element_rect(fill = "#FAFAFA", color = NA),
panel.background = element_rect(fill = "#FAFAFA", color = NA),
panel.grid.major.x = element_line(color = "#E8E8E8", size = 0.4),
panel.grid.major.y = element_blank(),
panel.grid.minor = element_blank(),

plot.title = element_textbox_simple(family = "Inter", size = 16, face = "bold",
color = "#2C3E50", margin = margin(b = 15)),
plot.subtitle = element_textbox_simple(family = "Inter", size = 12,
color = "#34495E", lineheight = 1.4,
margin = margin(b = 25)),
plot.caption = element_textbox_simple(family = "JetBrains Mono", size = 9,
color = "#7F8C8D", hjust = 0,
margin = margin(t = 20)),

axis.title.x = element_text(size = 12, color = "#2C3E50", margin = margin(t = 15)),
axis.text.x = element_text(size = 10, color = "#34495E"),
axis.text.y = element_text(size = 10, color = "#34495E"),

plot.margin = margin(10, 10, 10, 10)
)

# Display the plot
print(main_plot)
```
