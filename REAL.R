### Analysis Part

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Define a consistent color palette
region_colors <- c("EU_Sales" = "#66C2A5", "JP_Sales" = "#FC8D62", "NA_Sales" = "#8DA0CB", "Other_Sales" = "#E78AC3")

# Read the data
data <- read.csv('vgsales.csv')

# Filter the data for the years 2010 to 2016 and filter NA
data_filtered <- data %>%
  filter(!is.na(Year)) %>%
  filter(Year >= 2010 & Year <= 2016)

# Game Revenue by Country with pie chart
total_sales <- data_filtered %>%
  summarise(
    NA_Sales = sum(NA_Sales),
    EU_Sales = sum(EU_Sales),
    JP_Sales = sum(JP_Sales),
    Other_Sales = sum(Other_Sales)
  )

total_sales_long <- total_sales %>%
  gather(key = "Region", value = "Sales") %>%
  mutate(Percentage = Sales / sum(Sales) * 100)

ggplot(total_sales_long, aes(x = "", y = Sales, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  ggtitle("Market Share of Game Revenue\nby Country") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = region_colors) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))

# Game Purchases by Platform and Country
platform_sales <- data_filtered %>%
  group_by(Platform) %>%
  summarise(
    NA_Sales = sum(NA_Sales),
    EU_Sales = sum(EU_Sales),
    JP_Sales = sum(JP_Sales),
    Other_Sales = sum(Other_Sales)
  ) %>%
  gather(key = "Region", value = "Sales", -Platform)

ggplot(platform_sales, aes(x = Platform, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Game Purchases\nby Platform and Country") +
  xlab("Platform") +
  ylab("Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = region_colors)

# Game Sales by Genre
genre_sales <- data_filtered %>%
  group_by(Genre) %>%
  summarise(
    NA_Sales = sum(NA_Sales),
    EU_Sales = sum(EU_Sales),
    JP_Sales = sum(JP_Sales),
    Other_Sales = sum(Other_Sales)
  ) %>%
  gather(key = "Region", value = "Sales", -Genre)

ggplot(genre_sales, aes(x = Genre, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Game Sales\nby Genre") +
  xlab("Genre") +
  ylab("Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  scale_fill_manual(values = region_colors)

# Revenue by top 10 company
company_revenue <- data_filtered %>%
  group_by(Publisher) %>%
  summarise(
    NA_Sales = sum(NA_Sales),
    EU_Sales = sum(EU_Sales),
    JP_Sales = sum(JP_Sales),
    Other_Sales = sum(Other_Sales),
    Total_Sales = sum(NA_Sales + EU_Sales + JP_Sales + Other_Sales)
  ) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

company_revenue_long <- company_revenue %>%
  gather(key = "Region", value = "Sales", -Publisher, -Total_Sales)

ggplot(company_revenue_long, aes(y = reorder(Publisher, Total_Sales), x = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Revenue from\nTop 10 Companies") +
  ylab("Company") +
  xlab("Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = region_colors)

## ONLY JP

# Game Purchases by Platform in Japan
platform_sales_jp <- data_filtered %>%
  group_by(Platform) %>%
  summarise(JP_Sales = sum(JP_Sales)) %>%
  gather(key = "Region", value = "Sales", -Platform)

ggplot(platform_sales_jp, aes(x = Platform, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Game Purchases\nby Platform in Japan") +
  xlab("Platform") +
  ylab("Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_fill_manual(values = c("#66C2A5"))

# Game Sales by Genre in Japan
genre_sales_jp <- data_filtered %>%
  group_by(Genre) %>%
  summarise(JP_Sales = sum(JP_Sales)) %>%
  gather(key = "Region", value = "Sales", -Genre)

ggplot(genre_sales_jp, aes(x = Genre, y = Sales, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Game Sales\nby Genre in Japan") +
  xlab("Genre") +
  ylab("Sales (in millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_fill_manual(values = c("#66C2A5")) +
  coord_flip()


### ML part

