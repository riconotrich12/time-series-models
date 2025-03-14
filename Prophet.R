# Load required packages
library(randomForest)
library(xts)

# Import Dataset #
ds<-enrollment_data_final
ts_data <- ds$total_enrollees

# Create a month variable based on the semester
ds$month <- ifelse(ds$semester == 1, 1,  # September for the first semester
                                      ifelse(ds$semester == 2, 2,  # January for the second semester
                                             ifelse(ds$semester == 3, 3, NA)))  # June for summer

# Create a date variable
ds$date <- as.Date(paste(ds$year, ds$month, "01", sep = "-"))

# Convert to time series object
ts_xts <- xts(ts_data, order.by = ds$date)
# Create lag features for time series data
lags <- 1:3  # Number of lags to consider
lagged_data <- lag(ts_xts, k = lags)  # Create lagged data

# Combine the lagged features into one data frame
lagged_df <- data.frame(lagged_data)
colnames(lagged_df) <- paste0("lag_", lags)  # Rename columns with lag prefixes

# Merge the lagged features with the original data
final_data <- cbind(ds, lagged_df)  # Combine data frames

# Remove rows with NAs created by lagging
final_data <- final_data[complete.cases(final_data), ]

# Split the data into training and testing sets
train_percentage <- 0.8
train_size <- floor(train_percentage * nrow(final_data))
train_data <- final_data[1:train_size, ]
test_data <- final_data[(train_size + 1):nrow(final_data), ]
max_year <- as.numeric(format(test_data$date, "%Y")) + 4
test_data$date <- as.Date(paste(max_year, test_data$month, "01", sep = "-"))
test_data$date 
# Fit a Random Forest model
rf_model <- randomForest(total_enrollees ~ ., data = train_data, ntree = 200)
# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model using RMSE
rmse <- sqrt(mean((test_data$total_enrollees - predictions)^2))
cat("RMSE:", rmse, "\n")

ds$total_enrollees

future_dates <- seq(as.Date("2034-01-01"), by = "month", length.out = 3)  # Adjust the length.out as needed
future_dates
# Plot the original time series and the forecast
plot(ds$date, ds$total_enrollees, type = "l", col = "blue", lwd = 2, ylim = c(0, max(ds$total_enrollees, predictions)),
     xlab = "Date", ylab = "Total Enrollees", main = "Actual vs Predicted Enrollment", xlim=c(0, max(test_data$year, predictions)))

lines(test_data$date, predictions, col = "red", lwd = 2)

legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1, lwd = 2)

