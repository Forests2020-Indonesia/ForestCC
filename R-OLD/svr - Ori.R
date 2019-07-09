# developed by Felix Tampinongkol

library(readxl)
library(dplyr)
library(e1071)
library(caret)
library(openxlsx)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

## Load Data_frame
setwd("D:/Bang Wim/SET_SAMPLES/")
load_dataframe_train <- read_xlsx("Data_Bg.WIM_train_25042019.xlsx")
head(load_dataframe_train)
load_dataframe_test <- read_xlsx("Data_Bg.WIM_test_25042019.xlsx")
head(load_dataframe_test)

## Pembulatan data
round_data_train <- as.data.frame(lapply(load_dataframe_train, function(x) round(x, 3)))
head(round_data_train)
round_data_test <- as.data.frame(lapply(load_dataframe_test, function(x) round(x, 3)))
head(round_data_test)


## """Divide data for testing and training data"""
set.seed(3033)
# intrain <- createDataPartition(y = round_data$FRCI, p = 0.7, list = FALSE)
# training <- round_data[intrain,]
# testing <- round_data[-intrain,]

training <- round_data_train
testing <- round_data_test

dim(training)
dim(testing)
anyNA(round_data)

model_svr <- svm(FRCI ~ . , training)
predictedY <- predict(model_svr, testing)

error <- testing$FRCI - predictedY
svrPredictionRMSE <- rmse(error)

tuneResult <- tune(svm, FRCI ~ .,  data = training,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, testing)
error <- testing$FRCI - tunedModelY
tunedModelRMSE <- rmse(error)

# 1. 'Actual' and 'Predicted' data
df <- data.frame(testing$FRCI, tunedModelY)
# 2. R2 Score components
# 2.1. Average of actual data
avr_y_actual <- mean(df$testing.FRCI)
# 2.2. Total sum of squares
ss_total <- sum((df$testing.FRCI - avr_y_actual)^2)
# 2.3. Regression sum of squares
ss_regression <- sum((df$tunedModelY - avr_y_actual)^2)
# 2.4. Residual sum of squares
ss_residuals <- sum((df$testing.FRCI - df$tunedModelY)^2)
# 3. R2 Score
r2 <- 1 - ss_residuals / ss_total
