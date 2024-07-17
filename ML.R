# all use lib
library(ggplot2)
library(dplyr)
library(caret)

# import data
data <- read.csv('vgsales.csv')

# Filter
data_filtered <- data %>%
  filter(!is.na(Year)) %>%
  filter(Year >= 2010 & Year <= 2016)

# Select data
data_filtered <- data_filtered %>%
  select(Platform, Genre, Publisher, NA_Sales, 
         EU_Sales, JP_Sales, Other_Sales, Global_Sales) %>%
  na.omit()

# Split for training and testing (only training)
trainIndex <- createDataPartition(data_filtered$JP_Sales, p = 1, list = FALSE)
trainData <- data_filtered[trainIndex, ]

# Train linear regression models
model_NA <- train(NA_Sales ~ Platform + Genre + Publisher, data = trainData, method = "lm")
model_EU <- train(EU_Sales ~ Platform + Genre + Publisher, data = trainData, method = "lm")
model_JP <- train(JP_Sales ~ Platform + Genre + Publisher, data = trainData, method = "lm")
model_Other <- train(Other_Sales ~ Platform + Genre + Publisher, data = trainData, method = "lm")
model_Global <- train(Global_Sales ~ Platform + Genre + Publisher, data = trainData, method = "lm")

# Function
predict_sales <- function(input_data) {
  predicted_NA <- predict(model_NA, newdata = input_data)
  predicted_EU <- predict(model_EU, newdata = input_data)
  predicted_JP <- predict(model_JP, newdata = input_data)
  predicted_Other <- predict(model_Other, newdata = input_data)
  predicted_Global <- predict(model_Global, newdata = input_data)
  
  result <- data.frame(
    NA_Sales = predicted_NA,
    EU_Sales = predicted_EU,
    JP_Sales = predicted_JP,
    Other_Sales = predicted_Other,
    Global_Sales = predicted_Global
  )
  return(result)
}

# Use case
input <- data.frame(Platform = "Wii", Genre = "Sports", Publisher = "Nintendo")
predicted_sales <- predict_sales(input)
print(predicted_sales, row.names = FALSE)



