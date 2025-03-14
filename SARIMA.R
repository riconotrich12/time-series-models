########################################
## ARIMA Model ##
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
plot(enrollment_ts)
plot(enrollment_ts, main="Original Time Series", ylab="Enrollment", xlab="Time")
time_series_df <- data.frame(Date = time(time(enrollment_ts)), Enrollment = as.vector(enrollment_ts))
print(time_series_df)

# Apply Multiplicative Decomposition 
decomposed_ts <- decompose(enrollment_ts, type = "multiplicative")
plot(decomposed_ts)
seasonal_comp<-decomposed_ts$seasonal
trend_comp<-decomposed_ts$trend

# Analyze Differencing of Actual Data
ts_diff<-diff(enrollment_ts)
adf.test(ts_diff)
plot(ts_diff)
acf(ts_diff)
pacf(ts_diff)

# Analyze ACF and PACF of Actual Data to test for stationarity
acf(enrollment_ts)
pacf(enrollment_ts)
adf.test(enrollment_ts) #Reject null hypothesis, TS is stationary

# Analyze Differencing of Seasonal Component

ts_seasonalDiff<-diff(enrollment_ts, lag = 3)
adf.test(ts_seasonalDiff)

acf(ts_seasonalDiff, lag.max=20)
pacf(ts_seasonalDiff, lag.max=20)

# Perform incremental ADF test to identify ideal lag count
incremental_adf_tests <- function(enrollment_ts, max_diffs = 10) {
  for (d in 0:max_diffs) {
    if (d == 0) {
      series_diff <- enrollment_ts
    } else {
      series_diff <- diff(enrollment_ts, differences = d)
    }
    
    adf_test_result <- adf.test(series_diff)
    p_value <- adf_test_result$p.value
    
    cat("Differencing order (d):", d, " | ADF p-value:", p_value, "\n")
    plot(series_diff, main=paste("Differenced Series (d =", d, ")"), ylab="Differenced Values", xlab="Time")
    
    if (p_value < 0.05) {
      cat("Stationarity achieved at differencing order (d):", d, "\n")
      return(d)
    }
  }
  
  cat("Stationarity not achieved up to differencing order (d):", max_diffs, "\n")
  return(max_diffs)
}

diff <- incremental_adf_tests(ts_seasonalDiff, max_diffs = 8)
cat("The appropriate differencing order (d) determined is:", diff, "\n")

# Auto-fit SARIMA model for seasonal time-series
fitSARIMA<-auto.arima(enrollment_ts, seasonal = TRUE, trace = TRUE, 
                     stepwise = FALSE, approximation = FALSE)
summary(fitSARIMA)

# Custom fit SARIMA model for seasonal time-series
modelSARIMA<-Arima(enrollment_ts, order=c(0,0,0),
                  seasonal=list(order=c(0,1,2), period=3),
                  include.mean = FALSE, method="ML")
coeftest(modelSARIMA)
confint(modelSARIMA)
summary(modelSARIMA)

# Create forecast and prediction

model_predict(modelSARIMA,n.ahead=31)
model_forecast<-forecast(modelSARIMA, h=31)
model_forecast$fitted

summary(model_forecast)
autoplot(model_forecast) +
  autolayer(model_forecast, series = "Forecasted") +
  autolayer(enrollment_ts, series = "Actual") +
  xlab("Date") +
  ylab("Value") +
  ggtitle("Actual vs. Forecasted Data")

#Validate accuracy
acf(model_forecast$residuals, lag.max=20, na.action=na.pass)
pacf(model_forecast$residuals, lag.max=20, na.action=na.pass)
Box.test(model_forecast$residuals, lag=20, type="Ljung-Box")
hist(model_forecast$residuals)
accuracy_res<-accuracy(model_forecast$fitted, enrollment_ts, d=2, D=3)
accuracy_res

# Perform linear regression
data <- data.frame(fit = model_forecast$fitted, seasonal = seasonal_comp, trend = trend_comp)
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
