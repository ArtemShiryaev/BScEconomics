# Load required packages
library(vars)
library(forecast)

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")


train2 <- train[1:1051920, ]
train2 <- train2[,-1]
train2 <- train2[,-1]
rm(train)

sum(is.na(as.matrix(train2)))
#  304

#Complete cases omiiting 
train2 <- na.omit(train2) 

sum(is.na(as.matrix(train2)))


#Load ts 
ts_data <- ts(data=train2, start = c(2018, 1 ), frequency = 525960)
plot.ts(ts_data[1:500, ])


# Split data into training and testing sets
train_data <- window(ts_data[1:525960, ])#, start = c(2018, 1), end = c(2018, 2))
test_data <- window(ts_data[525960:993790, ])#, start = c(2018, 2), end = c(2018, 3))


# Determine the maximum lag order you want to consider
max_lag <- 25

# Use the 'VARselect' function to determine the optimal lag order
var_select <- VARselect(ts_data , lag.max = max_lag, type = "const")

# Print the results
print(var_select)

# Plot the information criteria to visually determine the optimal lag order
plot(var_select$criteria[1, ])
abline(v = var_select$criteria[1, ], col = "red")


# Fit VAR model
var_model <- VAR(train_data, p = 10)

summary(var_model)
# Plot residuals
plot(var_model$varresult$Target$residuals, main = "Residuals of VAR Model")

# Evaluate assumptions
causality(var_model,cause = "Target")

# Test for serial correlation
serial.test(var_model)

acf(train_data, lag.max = 100, plot = T)

# Test for normality
normality.test(var_model)

# Test for heteroscedasticity
arch.test(var_model)

# Forecast with VAR model
var_preds <- forecast(var_model, h = length(test_data[,1]), plot = T)

library(forecast)
# Plot forecasts
plot(var_preds, main = "Forecasted Values", CI = 0.95)

# Evaluate forecasts with RMSE
var_forecast <- as.matrix(cbind(var_preds$forecast$Count$mean,
                                var_preds$forecast$Open$mean,
                                var_preds$forecast$High$mean,
                                var_preds$forecast$Low$mean,
                                var_preds$forecast$Close$mean,
                                var_preds$forecast$Volume$mean,
                                var_preds$forecast$VWAP$mean,
                                var_preds$forecast$Target$mean))


var_rmse <- apply(var_forecast - test_data[1:100,1:8], 2, function(x) sqrt(mean(x^2)))
print(var_rmse)
