# Load required libraries
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)

# Import Dataset #
ds<-enrollment_data_final
ts_data <- ds$total_enrollees

# Factor the semester
ds$semester <- as.factor(ds$semester)

# Create a lagged version of the enrollment data
ds <- ds %>%
  arrange(year) %>%
  mutate(total_enrollees_lag1 = lag(total_enrollees, 3))
ds<- ds[complete.cases(ds), ]
print(ds)

# Split the data into training and testing sets (e.g., 80% training, 20% testing)
train_size <- 0.8
train_index <- round(nrow(ds) * train_size)
train_data <- ds[1:train_index, ]
test_data <- ds[(train_index + 1):nrow(ds), ]

# Train the Random Forest model
rf_model <- randomForest(total_enrollees ~ total_enrollees_lag1, data = test_data[c("year", "semester", "total_enrollees", 
                                                                 "total_enrollees_lag1")], proximity=TRUE, ntree = 100)
# Make predictions on the testing set
rf_predict <- predict(rf_model, test_data)
plot(rf_predict)

# Predict future enrollments until 2033 starting from 2026
future_years <-  c(max(test_data$year), rep(seq(max(test_data$year) + 1, 2026), each = 3))
future_semesters <- rep(test_data$semester, length.out = length(future_years))

future_data <- NULL
future_data <- data.frame(year = future_years, semester = future_semesters)
future_data$year <- as.numeric(future_data$year)
future_data$semester <- as.factor(future_data$semester)
future_data$total_enrollees <- head(test_data$total_enrollees, -2)
future_data$total_enrollees_lag1 <- head(test_data$total_enrollees_lag1, -2)
future_data<-data.frame(future_data)
future_data
# Make predictions for future enrollments
future_predictions <- predict(rf_model, newdata = future_data)
print(future_predictions)
# Combine future years, semesters, and predictions into a single dataframe
future_results <- data.frame(year = future_data$year,
                             semester = future_data$semester,
                             total_enrollees = future_predictions)

# Print or visualize future predictions
print(future_results)
plot(future_results$total_enrollees)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((future_data$total_enrollees - future_predictions)^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((future_data$total_enrollees - future_predictions) / future_data$total_enrollees)) * 100

# Mean Absolute Error (MAE)
mae <- mean(abs(future_data$total_enrollees - future_predictions))

print(rmse)
print(mape)
print(mae)
