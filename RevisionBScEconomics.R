# Revision

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")


train2 <- train[1:260000, ]

sum(is.na(as.matrix(train2)))
#  304

#Complete cases omiiting 
train2 <- na.omit(train2) 

sum(is.na(as.matrix(train2)))


df <- ts(data=train2[1:1000, -1 ], start = c(2018, 0 ), frequency = 525960)

plot.ts(df)




library(BigVAR)

# 3 x 7 coefficient matrix
B = BigVAR.fit(df,struct='Basic',p=100,lambda=1)[,,1]
# construct 7 x 99 lag matrix of Y
Z = VARXLagCons(df,p=100,oos=TRUE)$Z
# obtain out of sample forecasts
yhat = B%*%Z[,ncol(Z),drop=F]

yhat

plot(Z)

mod1<-constructModel(df,p=4,"Basic",
                     gran=c(50,10),h=100,
                     cv="Rolling",verbose=FALSE,IC=TRUE,model.controls=list(intercept=TRUE))

results=cv.BigVAR(mod1)
results


plot(results)




SparsityPlot.BigVAR.results(results)


predict(results,n.ahead=1)


pred <- predict(results,n.ahead=1, confint=TRUE)
plot(ts(pred))





#### SARIMAX and VAR


# Load required packages
library(forecast)
library(vars)

# Load data
data <- train2

# Convert data to time series
ts_data <- ts(data[, -1], start = c(2018, 0 ), frequency = 525960)

# Split data into training and testing sets
train_data <- window(ts_data[1:100, ])# , start = c(2018, 0 ), end = c(2018, 1), frequency = 525960)
test_data <- window(ts_data[101:190, ] )#, start = c(2018, 1), end = c(2018, 2), frequency = 525960)

p <- 14
d <- 2
q <- 5
P <- 15
D <- 2
Q <- 15
m <- 15 # varje 15  min
lag_order <- 24

# Fit SARIMAX model for each variable
sarimax_models <- list()
for (i in 1:ncol(train_data)) {
  sarimax_models[[i]] <- arima(train_data[, i], order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = m))
}

# Test SARIMAX models for each variable
sarimax_preds <- matrix(NA, ncol = ncol(train_data), nrow = length(test_data))
for (i in 1:ncol(train_data)) {
  sarimax_preds[, i] <- predict(sarimax_models[[i]], n.ahead = length(test_data))$pred
}
sarimax_rmse <- apply(sarimax_preds - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with SARIMAX models for each variable
sarimax_forecasts <- matrix(NA, ncol = ncol(train_data), nrow = 12)
for (i in 1:ncol(train_data)) {
  sarimax_forecasts[, i] <- forecast(sarimax_models[[i]], h = 12)$mean
}

# Fit VAR model
var_model <- VAR(train_data, p = lag_order, type = "const")

# Test VAR model
var_preds <- predict(var_model$varresult, n.ahead = length(test_data))
var_rmse <- apply(var_preds - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with VAR model
var_forecasts <- predict(var_model, n.ahead = 12)$fcst

# Evaluate forecasts with RMSE
sarimax_forecast_rmse <- apply(sarimax_forecasts - actual_values, 2, function(x) sqrt(mean(x^2)))
var_forecast_rmse <- apply(var_forecasts - actual_values, 2, function(x) sqrt(mean(x^2)))



###### nr2



# Load required packages
library(forecast)
library(vars)

# Load data
data <- train2

# Convert data to time series
ts_data <- ts(data[, -1], start = c(2018, 1), frequency = 525960)

# Split data into training and testing sets
train_data <- window(ts_data[1:500, -1])
test_data <- window(ts_data[501:600, -1])#, start = c(2021, 1))

sum(is.na(data))

p <- 14
d <- 2
q <- 5
P <- 15
D <- 2
Q <- 15
m <- 15 # weekly seasonality
lag_order <- 15

# Fit SARIMAX model for each variable
sarimax_models <- list()
for (i in 1:ncol(train_data)) {
  sarimax_models[[i]] <- arima(train_data[, i], order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = m), method = "CSS")
}

# Test SARIMAX models for each variable
sarimax_preds <- matrix(NA, ncol = ncol(train_data), nrow = length(test_data))
for (i in 1:ncol(train_data)) {
  sarimax_preds[, i] <- predict(sarimax_models[[i]], n.ahead = length(test_data))$pred
}
sarimax_rmse <- apply(sarimax_preds - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with SARIMAX models for each variable
sarimax_forecasts <- matrix(NA, ncol = ncol(train_data), nrow = 12)
for (i in 1:ncol(train_data)) {
  sarimax_forecasts[, i] <- forecast(sarimax_models[[i]], h = 12)$mean
}

# Fit VAR model
var_model <- VAR(train_data, p = lag_order, type = "const")

# Test VAR model
var_preds <- predict(var_model$varresult$Target, n.ahead = length(test_data))
var_rmse <- apply(var_preds - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with VAR model
var_forecasts <- predict(var_model, n.ahead = 12)$fcst

# Evaluate forecasts with RMSE
sarimax_forecast_rmse <- apply(sarimax_forecasts - test_data, 2, function(x) sqrt(mean(x^2)))
var_forecast_rmse <- apply(var_forecasts - test_data, 2, function(x) sqrt(mean(x^2)))
plot.ts(sarimax_forecasts)
plot.ts(as.ts(var_forecasts$Target))

fanchart(var_forecasts$Target)





######## 3



# Load required packages
library(forecast)
library(vars)

# Load data
data <- train2

# Convert data to time series
ts_data <- ts(data[, -1], start = c(2018, 1), frequency = 525960)

# Split data into training and testing sets
train_data <- window(ts_data, start = c(2018, 1), end = c(2019, 12))
test_data <- window(ts_data, start = c(2020, 1), end = c(2020, 12))

# Fit SARIMAX model for each variable using auto.arima function
sarimax_models <- list()
for (i in 1:ncol(train_data)) {
  sarimax_models[[i]] <- auto.arima(train_data[, i], seasonal = TRUE, max.order = c(5, 2, 5), max.P = 5, max.Q = 5)
}

# Test SARIMAX models for each variable
sarimax_preds <- matrix(NA, ncol = ncol(train_data), nrow = length(test_data[,1]))
for (i in 1:ncol(train_data)) {
  sarimax_preds[, i] <- forecast(sarimax_models[[i]], h = length(test_data[,1]))$mean
}
sarimax_rmse <- apply(sarimax_preds[,-1] - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with SARIMAX models for each variable
sarimax_forecasts <- matrix(NA, ncol = ncol(train_data), nrow = 12)
for (i in 1:ncol(train_data)) {
  sarimax_forecasts[, i] <- forecast(sarimax_models[[i]], h = 12)$mean
}

# Fit VAR model
var_model <- VAR(train_data, p = 24, type = "const")

# Test VAR model
var_preds <- predict(var_model, n.ahead = length(test_data))
var_preds_df <- as.matrix(cbind(var_preds$fcst$Count[,1],
                          var_preds$fcst$Open[,1],
                          var_preds$fcst$High[,1],
                          var_preds$fcst$Low[,1],
                          var_preds$fcst$Close[,1],
                          var_preds$fcst$Volume[,1],
                          var_preds$fcst$VWAP[,1],
                          var_preds$fcst$Target[,1]))


var_rmse <- apply(var_preds_df[1:100, ] - test_data, 2, function(x) sqrt(mean(x^2)))

# Forecast with VAR model
var_forecasts <- predict(var_model, n.ahead = 12)

# Evaluate forecasts with RMSE
sarimax_forecast_rmse <- apply(sarimax_forecasts - window(ts_data), 2, function(x) sqrt(mean(x^2)))
var_forecast_rmse <- apply(var_forecasts - window(ts_data), 2, function(x) sqrt(mean(x^2)))


autoplot(var_forecasts)
