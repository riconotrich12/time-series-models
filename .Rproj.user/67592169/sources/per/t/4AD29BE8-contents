########################################
## TBATS Model ##
########################################

# Import Libraries #
library(ggplot2)
library(forecast)
library(fUnitRoots)
library(lmtest)
library(tseries)
library(caTools)
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

# Fit TBATS model
tbats_model<-tbats(enrollment_ts, seasonal.periods=3)
summary(tbats_model)

# Create prediction and forecast

tbats_forecast<-forecast(tbats_model, h=31)
plot(tbats_forecast)
print(tbats_forecast)

# Validate accuracy
acf(tbats_forecast$residuals, lag.max=20, na.action=na.pass)
pacf(tbats_forecast$residuals, lag.max=20, na.action=na.pass)
Box.test(tbats_forecast$residuals, lag=20, type="Ljung-Box")
hist(tbats_forecast$residuals)
accuracy_res<-accuracy(tbats_forecast$fitted, enrollment_ts, d=2, D=3)
accuracy_res

# Perform linear regression
data <- data.frame(fit = tbats_forecast$fitted, seasonal = seasonal_comp, trend = trend_comp)
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
