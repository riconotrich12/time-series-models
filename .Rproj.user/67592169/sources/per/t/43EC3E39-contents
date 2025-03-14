########################################
## ETS Model ##
########################################

# Import Libraries #
library(ggplot2)
library(forecast)
library(caret)

# Import Dataset #
ds<-enrollment_data_final
ts_data<-ds$total_enrollees

# Create a month variable based on the semester
ds$month <- ifelse(ds$semester == 1, 1,  # September for the first semester
                   ifelse(ds$semester == 2, 2,  # January for the second semester
                          ifelse(ds$semester == 3, 3, NA)))  # June for summer

ds$date <- as.Date(paste(ds$year, ds$month, "01", sep = "-"))

# Sort the data frame by Date in ascending order
ds <- ds[order(ds$date), ]
print(ds)

# Create a time series object
enrollment_ts <- ts(ds$total_enrollees, 
                    start = c(min(ds$year), min(ds$month)),
                    frequency = 3)
plot(enrollment_ts, main="Original Time Series", ylab="Enrollment", xlab="Time")
time_series_df <- data.frame(Date = time(time(enrollment_ts)), Enrollment = as.vector(enrollment_ts))
print(time_series_df)

# Apply Multiplicative Decomposition 
decomposed_ts <- decompose(enrollment_ts, type = "multiplicative")
plot(decomposed_ts)
seasonal_comp<-decomposed_ts$seasonal
trend_comp<-decomposed_ts$trend
decomposed_ts
# Fit the NNETAR model to the time series data
ets_model <- ets(enrollment_ts, damped=TRUE)
ets_model
# Forecast future values using the fitted model
ets_forecast <- forecast(ets_model, h=30, level=c(80,95))
print(ets_forecast)
plot(ets_forecast, ylab="Total Enrollment", xlim=c(2003.0,2033.0))
lines(ets_forecast$fitted, lty=2, col="red")

#Evaluate model's residual correlation and accuracy
acf(ets_forecast$residuals, lag.max=20, na.action=na.pass)
Box.test(ets_forecast$residuals, lag=20, type="Ljung-Box")
hist(ets_forecast$residuals)
accuracy_res<-accuracy(ets_forecast$fitted, enrollment_ts, d=2, D=3)
accuracy_res

# Perform linear regression
data <- data.frame(fit = ets_forecast$fitted, seasonal = seasonal_comp, trend = trend_comp)
lm_result <- lm(fit ~ seasonal + trend, data = data)

# Summary of regression results
summary(lm_result)

# Create a cross-validation control object
ctrl <- trainControl(method = "cv", number = 10)
data <- na.omit(data)

# Perform cross-validation
model <- train(fit ~ seasonal + trend, data = data, method = "lm", trControl = ctrl)

# Access cross-validation results
print(model$results)
