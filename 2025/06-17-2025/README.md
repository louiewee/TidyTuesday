**📊 Key Narrative**

**"What APIs Power Our Digital Economy?"**

Cloud computing absolutely dominates the API landscape with nearly 1,000 services (955), creating a massive 3x gap over the second-place media category (340). This overwhelming dominance reveals how our digital infrastructure has fundamentally shifted to cloud-first architecture, where everything from storage and compute to AI services runs in the cloud.

**🔍 Key Insights**

- **Cloud Computing Leads:** 955 APIs - nearly triple the next category
- **Media & Open Data Follow:** 340 and 318 APIs respectively
- **Long Tail Distribution:** Most categories cluster between 50-170 APIs
- **Infrastructure Focus:** Cloud, analytics, and developer tools dominate the top categories

**🎨 Visualization & Design**

**Design Decisions:**

- **Horizontal bar chart** for easy category comparison
- **Gradient color scheme** from dark blue (high values) to gray (low values)
- **Clean, minimal styling** following Nicole Rennie's approach
- **Value labels on bars** to eliminate need for x-axis labels
- **Professional typography** using Inter font family

**💡 What This Tells Us**

The dominance of cloud computing APIs reflects our economy's fundamental digital transformation. These aren't just technical tools - they're the invisible infrastructure powering every app, website, and digital service we use daily. From AWS storage to Google Cloud AI to Azure compute services, cloud APIs have become the foundation layer of the modern internet.

The significant gap between cloud computing and other categories demonstrates how cloud providers have successfully created comprehensive ecosystems of interconnected services, making it easier for developers to build cloud-native applications.

**🔧 Technical Details**

**Libraries Used:**

- `tidyverse` - Data manipulation and visualization
- `ggplot2` - Core plotting functionality
- `ggtext` - Enhanced text formatting
- `showtext` - Custom font integration
- `scales` - Number formatting

**📋 Code**

`# TidyTuesday Week 24 2025: Web APIs
# Data: APIs.guru API Directory
# Visualization by: Louie Wee

# Load required libraries
library(tidyverse)
library(ggtext)
library(showtext)
library(scales)
library(patchwork)

# Load fonts
font_add_google("Inter", "Inter")
font_add_google("Inter", "Inter Bold")
showtext_auto()

# Load the data
api_categories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
api_info <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
api_logos <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
api_origins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
apisguru_apis <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')`

`# Data preparation
top_categories <- api_categories %>%
  count(apisguru_category, sort = TRUE) %>%
  slice_head(n = 12) %>%
  rename(category = apisguru_category) %>%
  mutate(
    category = case_when(
      category == "cloud" ~ "Cloud Computing",
      category == "open_data" ~ "Open Data",
      category == "developer_tools" ~ "Developer Tools",
      category == "ecommerce" ~ "E-Commerce", 
      category == "financial" ~ "Financial Services",
      category == "media" ~ "Media",
      category == "analytics" ~ "Analytics",
      category == "messaging" ~ "Messaging",
      category == "entertainment" ~ "Entertainment",
      category == "telecom" ~ "Telecom",
      category == "text" ~ "Text Processing",
      category == "location" ~ "Location Services",
      TRUE ~ str_to_title(str_trim(category))
    )
  )`

`# Create the main visualization
main_plot <- top_categories %>%
  ggplot(aes(x = reorder(category, n), y = n)) +
  geom_col(aes(fill = n), 
           width = 0.8, 
           show.legend = FALSE) +
  scale_fill_gradient(low = "#7f8c8d", high = "#1f3a93") +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip()+`

  `labs(
    title = "What APIs Power Our Digital Economy?",
    subtitle = "With nearly 1,000 APIs, cloud computing leads by a massive margin, followed by media and open data platforms. This reflects our shift toward cloud-first infrastructure where everything from storage to AI runs in the cloud.",
    x = NULL,
    y = "Number of API Services",
    caption = "**Data**: APIs.guru | **R:** ggplot2, tidyverse | **Graphic:** Louie Christopher Wee "
  ) +
  theme_minimal(base_family = "Inter", base_size = 14) +
  theme(
    plot.title = element_textbox_simple(
      size = 22, face = "bold", margin = margin(b = 10), color = "#2c3e50"
    ),
    plot.subtitle = element_textbox_simple(
      size = 12, color = "#34495e", margin = margin(t = 15, b = 25), lineheight = 1.2
    ),
    axis.text.y = element_text(size = 10, color = "#2c3e50"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_textbox_simple(
      size = 10, color = "#7f8c8d", margin = margin(t = 20), halign = 1, hjust = 0
    ),
    plot.background = element_rect(fill = "#fefefe", color = NA),
    panel.background = element_rect(fill = "#fefefe", color = NA),
    plot.margin = margin(30, 30, 20, 30)
  )

# Add value labels and display
main_plot_with_labels <- main_plot +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 4, 
            color = "#2c3e50", family = "Inter", fontface = "bold")

print(main_plot_with_labels)

**📈 Data Summary**

**Top 5 API Categories:**

- Cloud Computing: 955 APIs
- Media: 340 APIs
- Open Data: 318 APIs
- Analytics: 284 APIs
- Developer Tools: 168 APIs

**🔗 Resources**

- **Dataset:** APIs.guru GitHub Repository
- **TidyTuesday:** Week 24, 2025
