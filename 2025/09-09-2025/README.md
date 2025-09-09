# 📊 TidyTuesday Week 37 Analysis - September 9, 2025

**The Rise and Fall of Travel Freedom: Visa-Free Access Changes 2010-2025**

This analysis explores dramatic shifts in global travel freedom through visa-free access data, revealing how **peace and stability drive diplomatic gains** while **conflict and insecurity cause restrictions**. The visualization shows clear regional patterns in passport power evolution.

## 🎯 Key Findings

**🚀 Top 3 Improvements (2010-2025):**

- **🇦🇪 United Arab Emirates**: +120 destinations, ↑57 ranks (Gulf States)
- **🇺🇦 Ukraine**: +83 destinations, ↑37 ranks (Eastern Europe)
- **🇨🇴 Colombia**: +78 destinations, ↑38 ranks (South America)

**📉 Top 3 Declines (2010-2025):**

- **🇳🇬 Nigeria**: -6 destinations, ↓11 ranks (Africa)
- **🇾🇪 Yemen**: -6 destinations, ↓7 ranks (Middle East)
- **🇸🇾 Syria**: -12 destinations, ↓10 ranks (Middle East)

## 🌍 Regional Patterns

**Improvements span diverse regions** - Gulf States, Eastern Europe, and South America - showing that diplomatic gains come from various geopolitical strategies and circumstances.

**Restrictions center in Africa and Middle Eastern regions** - areas affected by conflict, terrorism, and political instability face concentrated travel barriers.

## 💡 Why These Changes Occurred

### 🚀 **Success Stories:**

**🇦🇪 UAE** - Aggressive soft diplomacy, economic diversification beyond oil, hosting global events (Expo 2020), strategic business hub positioning

**🇺🇦 Ukraine** - EU integration efforts, 2017 visa liberalization with EU, Western alignment policies, democratic reforms (pre-2022 conflict)

**🇨🇴 Colombia** - FARC peace process, improved security situation, Pacific Alliance membership, economic growth, diplomatic normalization

### 📉 **Decline Drivers:**

**🇳🇬 Nigeria** - Boko Haram terrorism, security concerns, governance challenges, economic instability, migration fears

**🇾🇪 Yemen** - Civil war since 2014, humanitarian crisis, state collapse, security risks, diplomatic isolation

**🇸🇾 Syria** - Civil war since 2011, refugee crisis, international sanctions, security concerns, diplomatic isolation

## 💻 Complete R Code

```r
# TidyTuesday 2025-09-09: Visa-Free Access Champions
# Author: Louie Wee
# Dataset: Passport Index - Visa-Free Access Rankings
# Theme: Countries Breaking Down Travel Barriers

# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(showtext)
library(scales)

# Load Google Fonts
font_add_google("Inter", "Inter")
font_add_google("Merriweather", "Merriweather")
showtext_auto()
showtext_opts(dpi = 300)

# Load the data
country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')
rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')

# Data processing: Calculate visa-free access changes from 2010 onwards
visa_changes <- rank_by_year %>%
  # Filter for 2010 onwards and clean data
  filter(year >= 2010, ![is.na](http://is.na)(year), ![is.na](http://is.na)(country), ![is.na](http://is.na)(visa_free_count)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  filter(n() >= 5) %>%  # Only countries with at least 5 years of data
  summarise(
    first_year = min(year),
    last_year = max(year),
    first_score = first(visa_free_count),
    last_score = last(visa_free_count),
    first_rank = first(rank),
    last_rank = last(rank),
    change = last_score - first_score,
    rank_change = first_rank - last_rank, # Positive means improved ranking
    pct_change = (change / first_score) * 100,
    years_tracked = n(),
    .groups = "drop"
  ) %>%
  filter(years_tracked >= 5, first_score > 30) %>%  # Focus on substantial tracking period
  arrange(desc(change))

# Get top 3 improvements and top 3 drops
top_3_improvements <- visa_changes %>%
  slice_head(n = 3) %>%
  mutate(category = "improvement")

top_3_drops <- visa_changes %>%
  slice_tail(n = 3) %>%
  mutate(category = "drop")

# Combine for highlighting
highlighted_countries <- bind_rows(top_3_improvements, top_3_drops)
highlighted_country_names <- highlighted_countries$country

# Get timeline data for line chart (ONLY top 3 and bottom 3)
timeline_data <- rank_by_year %>%
  filter(year >= 2010, country %in% highlighted_country_names) %>%
  filter(![is.na](http://is.na)(year), ![is.na](http://is.na)(visa_free_count)) %>%
  arrange(country, year) %>%
  mutate(
    country_category = case_when(
      country %in% top_3_improvements$country ~ "improvement",
      country %in% top_3_drops$country ~ "drop",
      TRUE ~ "other"
    )
  )

# Define country codes mapping
country_codes <- c(
  "United Arab Emirates" = "UAE",
  "Ukraine" = "UKR", 
  "Colombia" = "COL",
  "Nigeria" = "NGA",
  "Yemen" = "YEM",
  "Syria" = "SYR"
)

# Add country codes to timeline data
timeline_data <- timeline_data %>%
  mutate(country_code = country_codes[country])

# Define country-specific flag colors
country_colors <- c(
  "United Arab Emirates" = "#007A3D",  # UAE green
  "Ukraine" = "#0057B7",               # Ukraine blue  
  "Colombia" = "#FDE047",              # Colombia yellow
  "Nigeria" = "#22C55E",               # Nigeria green
  "Yemen" = "#EF4444",                 # Yemen red
  "Syria" = "#DC2626"                  # Syria red
)

# Get the year range for subtitle
year_range <- paste0("2010–", max(rank_by_year$year, na.rm = TRUE))

# Create info for subtitle with rank changes
improvement_info <- top_3_improvements %>%
  mutate(
    rank_text = case_when(
      rank_change > 0 ~ paste0("↑", rank_change, " ranks"),
      rank_change < 0 ~ paste0("↓", abs(rank_change), " ranks"),
      TRUE ~ "no rank change"
    )
  )

drop_info <- top_3_drops %>%
  mutate(
    rank_text = case_when(
      rank_change > 0 ~ paste0("↑", rank_change, " ranks"),
      rank_change < 0 ~ paste0("↓", abs(rank_change), " ranks"),
      TRUE ~ "no rank change"
    )
  )

# Create the line chart visualization (ONLY 6 countries)
main_plot <- timeline_data %>%
  ggplot(aes(x = year, y = visa_free_count, color = country)) +
  
  # Lines for the 6 countries
  geom_line(
    linewidth = 1.2,
    show.legend = FALSE
  ) +
  
  # Points for the 6 countries
  geom_point(
    size = 2.0,
    show.legend = FALSE
  ) +
  
  # Country codes as labels at the end of lines
  geom_text(
    data = timeline_data %>% 
      group_by(country) %>% 
      filter(year == max(year)),
    aes(
      x = year + 0.2, 
      y = visa_free_count, 
      label = country_code
    ),
    hjust = 0, 
    family = "Inter", 
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = country_colors) +
  
  scale_x_continuous(
    expand = c(0.02, 0.80),
    breaks = seq(2010, 2025, by = 2)
  ) +
  
  scale_y_continuous(
    expand = c(0.05, 0.05),
    labels = function(x) paste0(x)
  ) +
  
  labs(
    title = "**The Rise and Fall of Travel Freedom: Visa-Free Access 2010-2025**",
    subtitle = paste0("**Improvements driven by peace and stability** span diverse regions such as Gulf States, Eastern Europe, and South America, while **restrictions caused by conflict and insecurity** center in Africa and Middle Eastern regions. <br><br>**Biggest improvements:**<br>• <span style='color:", country_colors[improvement_info$country[1]], "'>", 
                     improvement_info$country[1], " (+", improvement_info$change[1], ", ", improvement_info$rank_text[1], ")</span><br>• <span style='color:", country_colors[improvement_info$country[2]], "'>", 
                     improvement_info$country[2], " (+", improvement_info$change[2], ", ", improvement_info$rank_text[2], ")</span><br>• <span style='color:", country_colors[improvement_info$country[3]], "'>", 
                     improvement_info$country[3], " (+", improvement_info$change[3], ", ", improvement_info$rank_text[3], ")</span><br><br>**Biggest drops:**<br>• <span style='color:", country_colors[drop_info$country[1]], "'>", 
                     drop_info$country[1], " (", drop_info$change[1], ", ", drop_info$rank_text[1], ")</span><br>• <span style='color:", country_colors[drop_info$country[2]], "'>", 
                     drop_info$country[2], " (", drop_info$change[2], ", ", drop_info$rank_text[2], ")</span><br>• <span style='color:", country_colors[drop_info$country[3]], "'>", 
                     drop_info$country[3], " (", drop_info$change[3], ", ", drop_info$rank_text[3], ")</span><br>"),
    x = "Year",
    y = "Visa-free destinations",
    caption = "**Data:** Henley Passport Index • **Graphic:** Louie Christopher Wee • **R:** ggplot2, tidyverse"
  ) +
  
  theme_minimal(base_family = "Inter") +
  theme(
    plot.background = element_rect(fill = "#FEFCF7", color = NA),
    panel.background = element_rect(fill = "#FEFCF7", color = NA),
    panel.grid.major = element_line(color = "#F1F5F9", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    plot.title = element_textbox_simple(
      size = 24, 
      family = "Merriweather", 
      face = "bold",
      color = "#1F2937",
      margin = margin(b = 5, t = 15)
    ),
    
    plot.subtitle = element_textbox_simple(
      size = 10, 
      family = "Inter",
      color = "#374151",
      lineheight = 1.2,
      margin = margin(t = 5)
    ),
    
    plot.caption = element_textbox_simple(
      size = 8, 
      family = "Inter",
      color = "#6B7280",
      halign = 1,
      margin = margin(t = 25)
    ),
    
    axis.title = element_text(
      size = 10, 
      family = "Inter", 
      color = "#374151",
    ),
    
    axis.text = element_text(
      size = 10, 
      family = "Inter", 
      color = "#6B7280"
    ),
    
    axis.ticks = element_blank(),
    legend.position = "none",
    
    plot.margin = margin(t=10, b=5, l=20, r=20)
  )

# Display the plot
print(main_plot)
```

## 📈 Data Analysis Process

### **Data Filtering Strategy**

- **2010+ only**: Excluded 2007 and 2009 due to missing data points
- **Minimum 5 years tracking**: Ensured reliable trend analysis
- **Substantial baseline**: Countries with >30 initial visa-free access for meaningful changes

### **Metric Selection**

- **Absolute changes**: Raw increase/decrease in visa-free destinations
- **Ranking shifts**: Global competitive position changes
- **Timeline focus**: 15-year period shows sustained diplomatic trends

### **Visual Encoding**

- **Line chart**: Reveals timing and progression of changes
- **Country codes**: Clean labeling without crowding
- **Flag colors**: Authentic national representation
- **Dual highlighting**: Green for gains, red for losses

## 🌟 Key Insights

### **Geopolitical Patterns**

1. **Peace dividend**: Countries resolving conflicts (Colombia FARC) see immediate diplomatic gains
2. **EU integration effect**: Ukraine's pre-war alignment strategy paid dividends
3. **Soft power strategy**: UAE's business diplomacy transforms passport strength
4. **Conflict penalty**: War and instability (Syria, Yemen) create immediate travel restrictions
5. **Security concerns**: Terrorism fears (Nigeria Boko Haram) impact international trust

### **Regional Dynamics**

- **Gulf States**: Strategic diversification beyond oil creates diplomatic openness
- **Eastern Europe**: EU integration processes drive systematic improvements
- **South America**: Regional stability and trade agreements boost access
- **Middle East/Africa**: Conflict zones face concentrated restrictions

## 📊 Technical Achievements

- **Clean data processing** with robust filtering criteria
- **Dual-metric storytelling** combining absolute and relative changes
- **Authentic visual design** using flag-inspired country colors
- **Scalable layout** accommodating country code labels
- **Professional typography** hierarchy for publication readiness

## 🔗 Data Source & Attribution

**Dataset**: TidyTuesday Week 37 (2025-09-09) - Passport Index Rankings

**Original Source**: Henley Passport Index via TidyTuesday

**GitHub**: https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-09-09

**Tools**: R (ggplot2, tidyverse, ggtext, showtext)

**Analysis Period**: 2010-2025 (15 years)

---

*This visualization reveals how diplomatic success and failure manifest in citizens' travel freedom, showing clear patterns between peace/stability and passport power.*
