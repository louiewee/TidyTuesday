# 📊 TidyTuesday Week 10 Analysis - March 11, 2025

This notebook analyzes Pixar films' critical reception over 25 years using Rotten Tomatoes scores. The visualization explores the question: "Has Pixar lost its magic touch?" through an engaging timeline showing the studio's journey from 1995 to 2020.

## 📦 Setup and Data Loading

```r
# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)

# Add Google Fonts
font_add_google("Inter", "inter")
font_add_google("Poppins", "poppins")
showtext_auto()
```

## 📊 Data Preparation

Next, we'll create our Pixar films dataset with 23 films from 1995-2020:

```r
# Pixar films data
pixar_data <- data.frame(
  film = c('Toy Story', "A Bug's Life", 'Toy Story 2', 'Monsters, Inc.', 'Finding Nemo', 
           'The Incredibles', 'Cars', 'Ratatouille', 'WALL-E', 'Up', 'Toy Story 3', 
           'Cars 2', 'Brave', 'Monsters University', 'Inside Out', 'The Good Dinosaur', 
           'Finding Dory', 'Cars 3', 'Coco', 'Incredibles 2', 'Toy Story 4', 'Onward', 'Soul'),
  year = c(1995, 1998, 1999, 2001, 2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 
           2012, 2013, 2015, 2015, 2016, 2017, 2017, 2018, 2019, 2020, 2020),
  rotten_tomatoes = c(100, 92, 100, 96, 99, 97, 74, 96, 95, 98, 98, 40, 78, 80, 
                      98, 76, 94, 69, 97, 93, 97, 88, 96)
)

# Create categories for storytelling
pixar_data <- pixar_data %>%
  arrange(year) %>%
  mutate(
    performance = case_when(
      rotten_tomatoes >= 95 ~ "Exceptional",
      rotten_tomatoes >= 80 ~ "Strong", 
      TRUE ~ "Struggled"
    )
  )
```

## 🎨 Visualization Creation

Now we'll create the timeline visualization showing Pixar's critical reception journey:

```r

# Define colors
colors <- c(
  "Exceptional" = "#27ae60",
  "Strong" = "#3498db", 
  "Struggled" = "#e74c3c"
 
# Create the main timeline plot
p <- ggplot(pixar_data, aes(x = year, y = rotten_tomatoes)) +
  
  # Add decade background bands
  annotate("rect", xmin = 1994, xmax = 2000, ymin = 25, ymax = 105, 
           fill = "#fff8e1", alpha = 0.3) +
  annotate("rect", xmin = 2000, xmax = 2010, ymin = 25, ymax = 105, 
           fill = "#e3f2fd", alpha = 0.3) +
  annotate("rect", xmin = 2010, xmax = 2021, ymin = 25, ymax = 105, 
           fill = "#e8f5e8", alpha = 0.3) +

  # Add connecting line and points
  geom_line(color = "#95a5a6", size = 1, alpha = 0.7) +
  geom_point(aes(color = performance), size = 3, alpha = 0.9) +

  # Add annotations for key films
  annotate("text", x = 1992, y = 107, label = "Toy Story\n100%", 
           size = 3, family = "inter", fontface = "bold", 
           color = "black", hjust = 0.5) +
  annotate("segment", x = 1993.5, y = 105, xend = 1994.5, yend = 101,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           color = "black", size = 0.5) +
  
  annotate("text", x = 2008, y = 32, label = "Cars 2\n40%", 
           size = 3, family = "inter", fontface = "bold", 
           color = "black", hjust = 0.5) +
  annotate("segment", x = 2009.5, y = 34, xend = 2010.5, yend = 38,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           color = "black", size = 0.5) +

  # Add scales and labels
  scale_color_manual(values = colors, name = "") +
  scale_x_continuous(limits = c(1994, 2021), breaks = seq(1995, 2020, 5)) +
  scale_y_continuous(limits = c(25, 110), breaks = seq(30, 100, 20)) +
  
  labs(
    x = "Release Year",
    y = "Rotten Tomatoes Score (%)",
    title = "**Has Pixar lost its magic touch?**",
    subtitle = "After a perfect start with <span style='color:#27ae60;'>**exceptional films above 95%**</span> in the 1990s, Pixar's critical reception shows more variation. While most films still achieve <span style='color:#3498db;'>**strong ratings above 80%**</span>, the 2010s introduced <span style='color:#e74c3c;'>**notable struggles**</span> alongside continued <span style='color:#27ae60;'>**masterpieces**</span>.",
    caption = "**Data:** {pixarfilms} | **R**: tidyverse, ggplot2 | **Graphic:** Louie Christopher Wee"
  )

# Apply theme and display
print(p)
```

## 📝 Analysis Summary

This visualization reveals several key insights about Pixar's 25-year journey:

- **Perfect Start**: Pixar's 1990s films (Toy Story trilogy) achieved near-perfect critical scores
- **Consistent Excellence**: Most films (19 out of 23) score above 80% on Rotten Tomatoes
- **Notable Struggles**: Cars franchise consistently underperforms (Cars 2 at 40% being the lowest)
- **Continued Innovation**: Recent films like Inside Out (98%) show Pixar can still achieve critical masterpieces
- **Overall Assessment**: While showing more variation, Pixar maintains remarkable consistency over 25 years

## 📚 Data Source & Methods

**Dataset**: TidyTuesday Week 10 (March 11, 2025) - Pixar Films from {pixarfilms} R package

**Tools**: R with tidyverse, ggplot2, ggtext, and showtext packages
