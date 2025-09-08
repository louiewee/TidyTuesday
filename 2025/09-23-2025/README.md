# TidyTuesday Week 39, 2025: FIDE Chess Elite Shakeup

## 🎯 Overview

**Chess's Elite Shakeup:** A slope graph visualization showing the dramatic September 2025 FIDE ranking changes, featuring Vincent Keymer's historic breakthrough as the first German in the top 10 since Robert Hübner, alongside unexpected falls from established elite players.

## 📁 Data Source

- **Dataset:** FIDE Chess Ratings (August & September 2025)
- **URL:** [TidyTuesday 2025-09-23](https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-09-23/readme.md)
- **Files:** `fide_ratings_august.csv` and `fide_ratings_september.csv`
- **Focus:** Top 12 players showing movements in and out of elite top 10
- **Key Metrics:** FIDE ratings, world rankings, rating point changes

## 🔍 Key Findings

- **Vincent Keymer (GER):** Jumped from rank 21 to #10 - first German in top 10 since Robert Hübner
- **Achievement:** Dominated Chennai Grand Masters with 5 wins, 0 losses (+21 rating points)
- **Historical significance:** Ended Germany's decades-long absence from chess elite
- **Wesley So (USA):** Climbed back into top 10 after winning Sinquefield Cup playoff
- **Impact:** Tournament victories can instantly reshape elite landscape
- **Abdusattorov (UZB):** Crashed from #7 to #11 after disastrous Sinquefield Cup
    - Finished last place (2.5/9 points)
    - Lost 23 rating points in single event
- **Giri (NED):** Fell out despite finishing 2nd at Chennai Grand Masters
    - Shows how competitive the top 10 has become
- **Ultra-tight margins:** Only 10 rating points separate ranks 7-12 in September 2025
- **Volatility:** One bad tournament can derail months of progress
- **Opportunity:** Strong performances can create historic breakthroughs

## 📈 Visualization Details

### Chart Type

**Slope Graph** - Perfect for showing individual player rank changes between two time periods

### Design Choices

- **Color Scheme:**
    - Deep Green: Breakthrough players (new top 10 entries)
    - Deep Red: Players who fell out of top 10
    - Gray: Players who maintained top 10 status
    - Warm Ivory background: Chess board inspired
- **Typography:**
    - Headers: Playfair Display (elegant serif for chess sophistication)
    - Body: Source Sans Pro (clean readability)
- **Visual Elements:**
    - Rank numbers only on August side (left)
    - Player names with FIDE ratings on September side

## 📖 The Narrative

**"When Excellence Meets Disaster: September 2025's Elite Chess Earthquake"**

September 2025 will be remembered as one of the most dramatic months in modern chess rankings. The story centers on Vincent Keymer's historic achievement - becoming the first German to crack the world's top 10 since Robert Hübner decades ago. At just 20 years old, Keymer's undefeated domination of the Chennai Grand Masters (5 wins, 0 losses) represents not just personal triumph but a national milestone for German chess.

The flip side reveals elite chess's unforgiving nature. Nodirbek Abdusattorov, comfortably seated at #7 in August, experienced a catastrophic collapse at the Sinquefield Cup, finishing dead last with 2.5/9 points and losing 23 precious rating points. His fall from #7 to #11 demonstrates how quickly fortunes can change at the highest level.

Perhaps most surprising was Anish Giri's exit from the top 10 despite finishing second at the same Chennai tournament where Keymer triumphed. This paradox - strong performance still resulting in a top 10 fall - illustrates the fierce competition where only excellence guarantees elite status.

The visualization's most striking insight: just 10 rating points separate ranks 7-12 in September 2025, showing how razor-thin the margins are among the world's best players.

## 🔧 Data Processing Notes

### Key Steps

- Combined August and September top 15 players to capture all movements
- Calculated age from birth year (bday) column
- Identified movement categories: Breakthrough, Fell Out, Maintained, Outside Top 10
- Created rating change calculations for both rank and points
- Focused on players ranked 12 or better in either month for relevance

### Technical Challenges

- Used `fed` (federation) instead of expected `country` column
- Handled missing rankings by assigning rank 16 to players outside top 15
- Implemented case-insensitive name matching for display labels

## 📊 Code

```r
# TidyTuesday 2025-09-23: FIDE Chess Ratings - Top 10 Shakeup
# Author: Louie Wee
# Theme: Breaking barriers and unexpected falls in chess's elite rankings

# Load required libraries ----
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Typography setup ----
font_add_google("Playfair Display", "playfair")  # Elegant serif for chess theme
font_add_google("Source Sans Pro", "source")     # Clean sans-serif for body text
showtext_auto()
showtext_opts(dpi = 300)  # CRITICAL: Always use dpi = 300

# Color palette - Chess-inspired ----
bg_color <- "#FAF9F6"        # Warm ivory (like chess board)
text_color <- "#2C2C2C"      # Dark charcoal
breakthrough_color <- "#1B5E20"  # Deep green for success stories
decline_color <- "#D32F2F"   # Deep red for falls
stable_color <- "#808080"    # Blue-grey for minimal changes
grid_color <- "#E8E6E1"      # Subtle grid

# Read TidyTuesday data ----
fide_ratings_august <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

# === DATA WRANGLING ===========================================================

# Prepare August data (top 15 to capture those who fall out)
august_top15 <- fide_ratings_august %>%
arrange(desc(rating)) %>%
slice_head(n = 15) %>%
mutate(
rank_august = row_number(),
period = "August",
age = ifelse(![is.na](http://is.na/)(bday) & bday > 1900, 2025 - bday, NA)
) %>%
select(name, rating_august = rating, rank_august, period, fed, age)

# Prepare September data (top 15)
september_top15 <- fide_ratings_september %>%
arrange(desc(rating)) %>%
slice_head(n = 15) %>%
mutate(
rank_september = row_number(),
period = "September",
age = ifelse(![is.na](http://is.na/)(bday) & bday > 1900, 2025 - bday, NA)
) %>%
select(name, rating_september = rating, rank_september, period, fed, age)

# Combine the datasets to track changes
ratings_comparison <- august_top15 %>%
full_join(september_top15, by = c("name", "fed", "age")) %>%
mutate(
# Handle missing rankings (players who weren't in top 15)
rank_august = ifelse([is.na](http://is.na/)(rank_august), 16, rank_august),
rank_september = ifelse([is.na](http://is.na/)(rank_september), 16, rank_september),

# Calculate rank change
rank_change = rank_august - rank_september,
rating_change = rating_september - rating_august,

# Focus on top 10 movements
in_top10_august = rank_august <= 10,
in_top10_september = rank_september <= 10,

movement_category = case_when(
!in_top10_august & in_top10_september ~ "Breakthrough",
in_top10_august & !in_top10_september ~ "Fell Out",
in_top10_august & in_top10_september ~ "Maintained",
TRUE ~ "Outside Top 10"
)
) %>%
filter(rank_august <= 12 | rank_september <= 12) %>% # Focus on relevant players
arrange(rank_september)

# Key storylines
breakthrough_players <- ratings_comparison %>%
filter(movement_category == "Breakthrough") %>%
arrange(rank_september)

fell_out_players <- ratings_comparison %>%
filter(movement_category == "Fell Out") %>%
arrange(rank_august)

# === VISUALIZATION ============================================================

# Create slope chart showing top 12 movements
slope_data <- ratings_comparison %>%
filter(rank_august <= 12 | rank_september <= 12) %>%
pivot_longer(
cols = c(rank_august, rank_september),
names_to = "period",
values_to = "rank"
) %>%
mutate(
period = case_when(
period == "rank_august" ~ "August",
period == "rank_september" ~ "September"
),
period_num = ifelse(period == "August", 1, 2),

# Add rating for display
rating_display = case_when(
period == "August" ~ rating_august,
period == "September" ~ rating_september,
TRUE ~ NA_real_
),

# Create display name (shorter for visualization)
display_name = case_when(
str_detect(tolower(name), "carlsen") ~ "Carlsen",
str_detect(tolower(name), "nakamura") ~ "Nakamura",
str_detect(tolower(name), "caruana") ~ "Caruana",
str_detect(tolower(name), "praggnanandhaa") ~ "Praggnanandhaa",
str_detect(tolower(name), "gukesh") ~ "Gukesh",
str_detect(tolower(name), "erigaisi") ~ "Erigaisi",
str_detect(tolower(name), "firouzja") ~ "Firouzja",
str_detect(tolower(name), "keymer") ~ "Keymer",
str_detect(tolower(name), "wesley|so") ~ "So",
str_detect(tolower(name), "abdusattorov") ~ "Abdusattorov",
str_detect(tolower(name), "giri") ~ "Giri",
str_detect(tolower(name), "wei") ~ "Wei Yi",
str_detect(tolower(name), "aronian") ~ "Aronian",
TRUE ~ str_extract(name, "^[^,]+")  # Take everything before first comma
)
)

# Create the main visualization
chess_plot <- ggplot(slope_data, aes(x = period_num, y = -rank, group = name)) +

# Background vertical lines only
geom_vline(xintercept = c(1, 2), color = grid_color, size = 0.5, alpha = 0.7) +

# Slope lines - colored by movement type
geom_line(
aes(color = movement_category),
size = 1.2,
alpha = 0.8
) +

# Points at each end
geom_point(
aes(color = movement_category),
size = 2.5,
alpha = 0.9
) +

# Rank numbers on August side (left)
geom_text(
data = filter(slope_data, period == "August", rank <= 12),
aes(label = rank),
hjust = 1.8,
size = 3.5,
family = "source",
color = text_color,
fontface = "bold"
) +

# Player names with FIDE points on September side (right)
geom_text(
data = filter(slope_data, period == "September", rank <= 12),
aes(label = paste0(display_name, " (", rating_display, ")"), color = movement_category),
hjust = -0.1,
size = 3.2,
family = "source",
fontface = "bold"
) +

# Color scheme
scale_color_manual(
values = c(
"Breakthrough" = breakthrough_color,
"Fell Out" = decline_color,
"Maintained" = stable_color,
"Outside Top 10" = stable_color
),
guide = "none"
) +

# Scales
scale_x_continuous(
limits = c(0.5, 3.5),
breaks = c(1, 2),
labels = c("AUGUST 2025", "SEPTEMBER 2025"),
position = "top"
) +

scale_y_continuous(
limits = c(-13, -0.5),
expand = c(0.02, 0.02)
) +

# Labels and title
labs(
title = "Chess's Elite Shakeup: Historic Breakthrough Meets Unexpected Falls",
subtitle = paste0(
"• <span style='color:", breakthrough_color, "'>**Vincent Keymer**</span>: Jumped from rank 21 to #10 after dominating Chennai Grand Masters — first German in top 10 since Robert Hübner<br>",
"• <span style='color:", breakthrough_color, "'>**Wesley So**</span>: Returned to top 10 after winning the Sinquefield Cup playoff<br>",
"• <span style='color:", decline_color, "'>**Abdusattorov**</span>: Crashed from #7 to #11 after finishing last at Sinquefield Cup<br>",
"• <span style='color:", decline_color, "'>**Giri**</span>: Fell out of top 10 despite finishing 2nd in Chennai Grand Masters<br>",
"• **Elite competition**: Just 10 rating points separate ranks 7-12 in September 2025"
),
x = NULL,
y = NULL,
caption = "**Data**: FIDE Chess Ratings (August-September 2025) | **R**: tidyverse, ggplot2 | **Graphic**: Louie Christopher Wee"
) +

# Theme
theme_minimal(base_family = "source", base_size = 12) +
theme(
# Canvas
plot.background = element_rect(fill = bg_color, color = NA),
panel.background = element_rect(fill = bg_color, color = NA),

# Remove all grid lines and axis elements
panel.grid = element_blank(),
axis.line = element_blank(),
axis.ticks = element_blank(),

# Typography
plot.title = element_textbox_simple(
size = 18,
face = "bold",
color = text_color,
family = "playfair",
margin = margin(b = 15),
lineheight = 1.2
),
plot.subtitle = element_textbox_simple(
size = 12,
color = text_color,
family = "source",
margin = margin(t = 5, b = 5),
lineheight = 1.4
),
plot.caption = element_textbox_simple(
size = 9,
color = "#666666",
family = "source",
margin = margin(t = 20),
hjust = 0,
halign = 1
),

# Remove y-axis completely
axis.text.y = element_blank(),
axis.title.y = element_blank(),

# Keep x-axis labels
axis.text.x = element_text(
size = 11,
color = text_color,
family = "source",
face = "bold"
),

# Margins
plot.margin = margin(5, 5, 5, 5)
)

# Display plot
print(chess_plot)
```
