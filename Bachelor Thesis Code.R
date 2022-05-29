#======================================
# Umeå University
# Bachelor Thesis in Economics
# Spring Semester 2022
# Artem Angelchev Shiryaev
# Final Version
# Updated 2022-05-29
#======================================


#######################################
######## Libraries & Packages #########
#######################################
library(vars)
library(VARshrink)
library(BigVAR)
library(glmnet)
library(lars)
library(urca)



# library(forecast)
# library(tseries)
# library(onlineVAR)
# library(FitAR)
# library(doMC)



getwd()
setwd()

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace


#######################################
############# DATA PROCESSING #########
#######################################


## Loading data
# Original dataset NOT INCLUDED ON GITHUB DUE TO SIZE

# Bitcoin only
load("~/GitHub/BScEconomics/data.RData")

# Checking missing values
sum(is.na(as.matrix(Bitcoin)))
#  304

#Complete cases omiiting 
Bitcoin <- na.omit(Bitcoin) 

sum(is.na(as.matrix(Bitcoin)))
# 0


# Dropping ASSET-ID from original dataset
Bitcoin <- Bitcoin[,-2]

# First week of 2018
data  <- Bitcoin[1:25200, ]
data2 <- Bitcoin[25201:50401, ] # Used in some models for benchmarking

# Dropping Bitcoin from RAM
rm(Bitcoin)

# Visiualization of data
plot1 <- ts(as.matrix(data[1:25200, -1 ]),
            start = c(2018, 0 ), frequency = 525960)
plot(plot1, yax.flip = TRUE,
     col = "forestgreen",
     main = "First week of 2018"
)

rm(plot1)


#######################################
#### CHECKING STATIONARITY WITH ADF ###
#######################################

# From library urca
test.adf1 <- ur.df(data[,1], type = c("none"), selectlags = "BIC");test.adf1
test.adf2 <- ur.df(data[,2], type = c("none"), selectlags = "BIC");test.adf2
test.adf3 <- ur.df(data[,3], type = c("none"), selectlags = "BIC");test.adf3
test.adf4 <- ur.df(data[,4], type = c("none"), selectlags = "BIC");test.adf4
test.adf5 <- ur.df(data[,5], type = c("none"), selectlags = "BIC");test.adf5
test.adf6 <- ur.df(data[,6], type = c("none"), selectlags = "BIC");test.adf6
test.adf7 <- ur.df(data[,7], type = c("none"), selectlags = "BIC");test.adf7
test.adf8 <- ur.df(data[,8], type = c("none"), selectlags = "BIC");test.adf8

# Interpretation help function developed by
# Author: Hank Roark
# Date: October 2019

source("~/GitHub/BScEconomics/LASSO Scripts/R/interp_urdf.R")

# Also works with trend & drift ADF
interp_urdf(test.adf1, level = "5pct")



test.adf <- c(test.adf1@teststat,
              test.adf2@teststat,
              test.adf3@teststat,
              test.adf4@teststat,
              test.adf5@teststat,
              test.adf6@teststat,
              test.adf7@teststat,
              test.adf8@teststat)
temp.name    <- c("Count", 
                  "Open",
                  "High",
                  "Low",
                  "Close",
                  "Volume",
                  "VWAP",
                  "Target"
                  )
names(test.adf) <- temp.name
## Stationary 
test.adf
#       Count         Open         High          Low        Close       Volume         VWAP       Target 
#-161.1063050  -16.3616190   -0.6165626   -0.5551648   -0.5605079   -0.6109764  -26.0014005   -0.5960510

# Count, Open and VWAP does not have a unit root,
# High, Low, Close, Volume and Target has a unit root
# Which means it's non stationary

## To get in dept summary of each Augmented Dickey-Fuller test uncomment and run below:
# summary(test.adf1)
# summary(test.adf2)
# summary(test.adf3)
# summary(test.adf4)
# summary(test.adf5)
# summary(test.adf6)
# summary(test.adf7)
# summary(test.adf8)


# Remove data to clear workspace
rm(test.adf,
   test.adf1,
   test.adf2,
   test.adf3,
   test.adf4,
   test.adf5,
   test.adf6,
   test.adf7,
   test.adf8
   )

# We difference the time series once, remove time series - since it becomes 0

tempdiff  <- diff(as.ts(data),  differences=1);tempdiff  <- tempdiff[, -1]

# Used in some models for benchmarking later in script
tempdiff2 <- diff(as.ts(data2), differences=1);tempdiff2 <- tempdiff2[, -1]


adf1 <- ur.df(tempdiff[,1], type = c("none"), selectlags = "BIC")
adf2 <- ur.df(tempdiff[,2], type = c("none"), selectlags = "BIC")
adf3 <- ur.df(tempdiff[,3], type = c("none"), selectlags = "BIC")
adf4 <- ur.df(tempdiff[,4], type = c("none"), selectlags = "BIC")
adf5 <- ur.df(tempdiff[,5], type = c("none"), selectlags = "BIC")
adf6 <- ur.df(tempdiff[,6], type = c("none"), selectlags = "BIC")
adf7 <- ur.df(tempdiff[,7], type = c("none"), selectlags = "BIC")
adf8 <- ur.df(tempdiff[,8], type = c("none"), selectlags = "BIC")
adf  <-     c(adf1@teststat,
              adf2@teststat,
              adf3@teststat,
              adf4@teststat,
              adf5@teststat,
              adf6@teststat,
              adf7@teststat,
              adf8@teststat)

temp.name    <- c("Count", 
                  "Open",
                  "High",
                  "Low",
                  "Close",
                  "Volume",
                  "VWAP",
                  "Target"
)
names(adf) <- temp.name
## Stationary 
adf
#     Count      Open      High       Low     Close    Volume      VWAP    Target 
# -151.0217 -106.0343 -146.8883 -118.8654 -106.0456 -155.3755 -101.9962 -126.2707 

# All of the time series reject H_0 on a 5% significance level, 
# meaning that the series does not have a uni root


# Clearing up work space
rm(temp.name,
   adf,
   adf1,
   adf2,
   adf3,
   adf4,
   adf5,
   adf6,
   adf7,
   adf8,
   data2
)




# Selecting Optimal Lag
# WARNING: Takes 2+ hrs on laptop for this dataset size
lagLenght <- VARselect(tempdiff, lag.max = 250,
                       type = "const")

#DiffBitcoin[,8], lag.max = 250,
#exogen = DiffBitcoin[,1:7],

# Results of the Various selection criterions
# I only apply SC(n), also known as BIC
lagLenght$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 61     31     16     61






# Scaling the dataset, to mean = 0 and sd 1
# Bitcoin <- scale(Bitcoin)


ARX.Model <- ar(tempdiff[, 8], 
                aic = T,
                order.max = 500,
                p = lagLenght$selection[3], 
                lag.max = 183,
                exogen = tempdiff[ ,1:7],
                type = "both",
                se.fit = T
                
)


x11(); par(mai=rep(0.4, 4)); 

plot(ARX.Model$partialacf, type = "l")

plot(ARX.Model$resid, type = "l",
     ylab = "ARX Residuals",
     xlab = "Time",
     main = "Residuals of ARX Model on First week of 2018",
     col  = 4
)

ks.test(x = ARX.Model$resid, y = pnorm, alternative = "two.sided")

# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  ARX.Model$resid
# D = 0.49664, p-value < 2.2e-16
# alternative hypothesis: two-sided




#######################################
######### VARX-RIDGE REGRESSION #######
#######################################
VAR.Ridge <- VARshrink(tempdiff,
                       p = lagLenght$selection[3], #183
                       type = "const",
                       method = c("ridge")
)

summary(VAR.Ridge)

Forecast.Ridge <- predict(object = VAR.Ridge,
                          ci = 0.95,
                          n.ahead = 3600 # 10 hour forecast
                     
)




## PLOT
FC.Target <- as.ts(Forecast.Ridge$fcst$Target)


x11(); par(mai=rep(0.4, 4)); 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(FC.Target[1:360, 1],
     ylab = "Returns",
     xlab = "Time",
     main = "Forecasted Returns 6 Hours, VARX - Ridge Model",
     col = "red",
     type = "l",
     ylim = c(-0.02,0.02)
)


lines(FC.Target[, 2],
      col = 3)

lines(FC.Target[, 3],
      col = 5)

lines(tempdiff2[1:360, 8],
      type = "l",
      col  = 9)

lines(FC.Target[, 4],
      col = "blue",
      type = "l",
      lwd = 1)

lines(-FC.Target[, 4],
      col = "blue",
      type = "l",
      lwd = 1)


legend(#"top", 
  #x = 325  , y  = 0.1,
  "topright",
  legend = c("Forecast", 
             "95% CI",
             "Lowest",
             "Highest",
             "Real Values"), 
  col = c("red","blue", "3","5", "9"), 
  pch = c(17,19), 
  bty = "n", 
  #pt.cex = 2, 
  #cex = 1.2, 
  text.col = "black", 
  inset = c(-0.25, 0.1),
  title="Time Series"
)


##############################
### Testing residuals Kolmogorov Smirnov
##############################

ridge.res <- VAR.Ridge[["varresult"]][["Target"]][["residuals"]]

ks.test(x = ridge.res, y = pnorm, alternative = "two.sided")



# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  ridge.res
# D = 0.49606, p-value < 2.2e-16
# alternative hypothesis: two-sided



##############################
##### LASSO ESTIMATION ######
#############################



# Custom Functions
source("~/GitHub/BScEconomics/LASSO Scripts/R/lassovar-forecast-internal.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/lassovar-internal.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/lassovar-postols.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/predict.lassovar.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/residuals.lassovar.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/summary.lassovar.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/lassovar-ada.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/lassovar.R")
source("~/GitHub/BScEconomics/LASSO Scripts/R/forecast.lassovar.R")





# Transforming data to fit custom functions
trans.data <- scale(tempdiff)

Bitcoin_Subset    <- trans.data[1:5000, ]
Bitcoin_Benchmark <- trans.data[5001:9500, ]
Bitcoin_Whole     <- trans.data[10001:11000, ]


Target_Train <- Bitcoin_Subset[,8]
Target_Test  <- Bitcoin_Benchmark[,8] 
Target_All   <- Bitcoin_Whole[,8]




VAR.lasso <- lassovar(dat = Bitcoin_Subset, 
                     #exo = Bitcoin_Subset[,1:8],
                     lags = lagLenght$selection[3],  #183
                     adaptive = c('none'), # Adaptive lasso can also be used
                     mc = F, # Parallelism can be activated
                     ncores = 1, 
                     horizon = 360,  # 6 hour horizion
                     dfmax = NULL)

summary.lassovar(VAR.lasso)



coef_test <- coef(VAR.lasso)
(nrow(coef_test)); ncol(coef_test)
nrow(Bitcoin_Benchmark[,8]); ncol(Bitcoin_Benchmark[,8])
BenchmarkT <- t(Target_Test[2:(nrow(coef_test))])
nrow(BenchmarkT);ncol(BenchmarkT)


## Doesn't work only predicts the next "time-period" and is not able to forecast a longer period.
Pred.VAR.lasso <- predict.lassovar(object = VAR.lasso, 
                          fc.data = as.matrix(BenchmarkT),
                          #fc.data = as.matrix(Bitcoin_Benchmark[,8])
                          n.ahead  = 5000 # only forecasts one period 
)

# Only plots one interval
plot(Pred.VAR.lasso)


# Forecasts very well
# Select either whole VAR or add exogenous variables
Forecast.VAR.lasso <- forecast.lassovar(dat = Bitcoin_Whole,#[, 8:9], 
                               #exo = Bitcoin_Whole[ ,1:7],
                               fc.train = 950,
                               lags = 183,
                               horizon = 50,
                               ic=c('BIC'), #,'AIC'),
                               adaptive=c('none'), #,'group','ridge'),
                               silent=F,
                               trend=FALSE,
                               post=F
)







Forecast.VAR.lasso
summary(Forecast.VAR.lasso)

summary(Forecast.VAR.lasso[["coefficients"]][[1]])
x11(); par(mai=rep(0.4, 4));
plot(residuals.lassovar(VAR.lasso))
summary.lassovar(VAR.lasso)





#############################
## BigVAR LASSO ESTIMATION ##
#############################


# Only first 600 entries used, otherwise estimation processes takes too long
# personally tried 4+ hrs, uses "Rolling" Cross validation
BIGVAR.LASSO <- constructModel(tempdiff[1:600, ],
                               p=lagLenght$selection[3],  #183
                               struct = "Basic",
                               gran = c(50,10)
                         )

BIGVAR.LASSO



# Parallelalism is supposed to work
# But not supported on R version 4.2.0
# require(doParallel)
# require(doMC)
# registerDoMC(cores = 4)
Forecast.BIGVAR.LASSO   <- cv.BigVAR(BIGVAR.LASSO)
                                     #parallel = T,
                                     #trace.it = T)

plot(Forecast.BIGVAR.LASSO,
     title = "VARX - Lasso, BigVAR")

summary(Forecast.BIGVAR.LASSO)


BIGVAR.coef <- Forecast.BIGVAR.LASSO@betaPred


sum(!is.na(BIGVAR.coef[8,]))


# Only first 1 out for all 129 is non zero?
plot(BIGVAR.coef[8,1:3],
     type = "l")


# temp.Pred <- predict(BIGVAR.coef[8, 1:2], 
#                           n.ahead = 360,
#                           ci = 0.95)
# 
# plot(temp.Pred,
#      type = "l")
# 


lasso.res <- Forecast.BIGVAR.LASSO@resids


# Nearly all around Zero
fitted <- Forecast.BIGVAR.LASSO@fitted

plot(fitted[,8])


#############################
## KOLMOGOROV-SMIRNOV TEST
#############################

ks.test(x = lasso.res, y = pnorm, alternative = "two.sided")

# Asymptotic one-sample Kolmogorov-Smirnov test
# 
# data:  lasso.res
# D = 0.39343, p-value < 2.2e-16
# alternative hypothesis: two-sided



#############################
##### LARS - Least Angle Regression (not included in thesis)
#############################


LARS.MODEL <- lars(x     = as.matrix(tempdiff[ ,1:7]),
                   y     = as.matrix(tempdiff[ ,8]),
                   type  = "lasso",
                   trace = T,
                   normalize = T,
                   intercept = T,
                   use.Gram = T
)

plot(LARS.MODEL)
summary(LARS.MODEL)


Forecast.LARS.MODEL <- predict(LARS.MODEL,
                               newx = tempdiff2[ , 1:7],
                               type = "fit",
                               Mode="lambda"
)
Forecast.LARS.MODEL 

plot(ts(Forecast.LARS.MODEL$fit[1:25200,8]))
library(graphics)
plot(DiffBitcoin2[1:25000 , 8],
     #col = "red",
     type = "l",
     main = "LARS, Weekly Forecast",
     ylab = "Returns",
     xlab = "Time"
)

lines(ts(Forecast.LARS.MODEL$fit[1:25000,8]))
