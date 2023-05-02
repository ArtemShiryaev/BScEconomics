#======================================
# Umeå University
# Bachelor Thesis in Economics
# Spring Semester 2022
# Artem Angelchev Shiryaev
# Revision and Updates
#======================================



# Revision Date 2023-05-02
# R version 4.2.1 used



#######################################
######## Libraries & Packages #########
#######################################

## List of Packages
list.of.packages <- c("highfrequency", 
                      "data.table", 
                      "xts",
                      "tibble",
                      "tidyquant",
                      "vars",
                      "urca",
                      "forecast",
                      "dymo",
                      "MTS",
                      "mvtsplot")
# Install if applicable
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages, require, character.only = TRUE)




getwd()
#setwd()

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace
gc()

#######################################
############# DATA PROCESSING #########
#######################################


## Loading data
# Original dataset

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")




## Subsetting dataset into each respective classes

BNB  <- train[train$Asset_ID == 0, ]  # Binance Coin
BTC  <- train[train$Asset_ID == 1, ]  # Bitcoin
BCH  <- train[train$Asset_ID == 2, ]  # Bitcoin Cash
ADA  <- train[train$Asset_ID == 3, ]  # Cardano 
DOGE <- train[train$Asset_ID == 4, ]  # Dogecoin
EOS  <- train[train$Asset_ID == 5, ]  # EOS.IO
ETH  <- train[train$Asset_ID == 6, ]  # Ethererum
ETC  <- train[train$Asset_ID == 7, ]  # Ethereum Classic
IOTA <- train[train$Asset_ID == 8, ]  # IOTA
LTC  <- train[train$Asset_ID == 9, ]  # Litecoin
MKR  <- train[train$Asset_ID == 10, ] # Maker
XMR  <- train[train$Asset_ID == 11, ] # Monero
XLM  <- train[train$Asset_ID == 12, ] # Stellar
TRX  <- train[train$Asset_ID == 13, ] # Tron

#Removing original data set to save RAM 
rm(train)



# OWN FUNCTIONS to transform original timestamps into usable time series format

Fix_time <- function(r){
  ti= seq(from = ymd_hm("1970-01-01 00:00"),
          to = ymd_hm("2021-09-21 00:00"), by =  "1 min", tz = "UTC")
  
  tbl <- tibble(t =ti,
                x = 1:length(t))
  q <- length(ti)
  w <- nrow(as.matrix(r))
  g <- (q - w)
  r$timestamp <- ti[g:(q-1)]
  r <- as.data.table(r)
  r <- as.xts.data.table(r)
  return(r)
}

# Remove NA function from time series dataset

Remove_NA <- function(r){
  if (sum(is.na(as.matrix(r))) == 0){
  return(r)
  } else {
    d <- na.omit(r)
    return(d)
    }
  
}

# Both functions in one
Both_fun <- function(r){
  r <- Fix_time(r)
  r <- Remove_NA(r)
  return(r)
}


# Applying both functions to each asset
BNB <- Both_fun(BNB); BTC <- Both_fun(BTC); BCH <- Both_fun(BCH); ADA <- Both_fun(ADA); DOGE <- Both_fun(DOGE);
EOS <- Both_fun(EOS); ETH <- Both_fun(ETH); ETC <- Both_fun(ETC); IOTA <- Both_fun(IOTA); LTC <- Both_fun(LTC);
MKR <- Both_fun(MKR); XMR <- Both_fun(XMR); XLM <- Both_fun(XLM); TRX <- Both_fun(TRX)




## AGGREGATE THE TIME SERIES TO REDUCE SIZE OF DATASET to hourly data rather than minute data

tt <- 60 # 60 mins instead of 1 minute hfd
BNB  <- aggregateTS(BNB, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Binance Coin
BTC  <- aggregateTS(BTC, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Bitcoin
BCH  <- aggregateTS(BCH, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Bitcoin Cash
ADA  <- aggregateTS(ADA, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Cardano 
DOGE <- aggregateTS(DOGE, FUN =  "mean", alignBy = "minutes", alignPeriod = tt) # Dogecoin
EOS  <- aggregateTS(EOS, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # EOS.IO
ETH  <- aggregateTS(ETH, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Ethererum
ETC  <- aggregateTS(ETC, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Ethereum Classic
IOTA <- aggregateTS(IOTA, FUN =  "mean", alignBy = "minutes", alignPeriod = tt) # IOTA
LTC  <- aggregateTS(LTC, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Litecoin
MKR  <- aggregateTS(MKR, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Maker
XMR  <- aggregateTS(XMR, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Monero
XLM  <- aggregateTS(XLM, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Stellar
TRX  <- aggregateTS(TRX, FUN =  "mean",  alignBy = "minutes", alignPeriod = tt) # Tron



gc()





# FIRST CHECK BTC


#######################################
#### CHECKING STATIONARITY WITH ADF ###
#######################################

# Augmented Dicker fuller function to check for stationarity
adf_testing <- function(data,adf.type,selection){
  library(urca)
  source("~/GitHub/BScEconomics/LASSO Scripts/R/interp_urdf.R")
  test.adf_temp <- 1:ncol(data)
  res <- NULL
  temp.var.adf <- adf.type
  temp.selec <- selection
  for (i in 1:ncol(data)) {
    temp.var <-  ur.df(data[ ,i], type = temp.var.adf, selectlags = temp.selec)
    test.adf_temp <- temp.var@teststat
    res[i] <- temp.var@teststat
  }
  res <- res[-1]
  temp.name    <- c("Count", 
                    "Open",
                    "High",
                    "Low",
                    "Close",
                    "Volume",
                    "VWAP",
                    "Target"
  )
  names(res) <- temp.name
  return(res)
}
  
adf.BTC <- adf_testing(BTC,"none","BIC")#;adf.BTC

#       Count         Open         High          Low        Close       Volume         VWAP       Target 
# -31.0835829    0.1732056    0.1845908    0.1554837    0.1711138  -44.1645633    0.1717384 -125.4182999 

# For BTC the univariate time series
# Open, High, Low, Close, VWAP are non-stationary, differences are taken and adf is checked again

# Which makes sense given 
#plot(BTC[1:1000, ])


diff.BTC  <- diff(BTC[, c(3,4,5,6,8)],  differences=1);diff.BTC <- diff.BTC[-1, ]; BTC <- BTC[-1, ]
BTC$Open <- diff.BTC$Open;BTC$High <- diff.BTC$High;BTC$Low <- diff.BTC$Low;BTC$Close <- diff.BTC$Close;BTC$VWAP <- diff.BTC$VWAP
#diff.BTC$Asset_ID <- BTC$Asset_ID[-1,]; diff.BTC  <- Remove_NA(diff.BTC)

adf.BTC <- adf_testing(BTC,"none","AIC");adf.BTC

#     Count       Open       High        Low      Close     Volume       VWAP     Target 
#-31.08319 -119.14989 -118.71679 -120.96774 -119.19247  -44.16425 -119.26432 -125.37487 


library(aTSA)


## Plotting PACF and ACF and CCF plots

acf(BTC[,-1], lag.max = 100)
pacf(BTC[,-1], lag.max = 100)

library(corrplot)

M <- cor(BTC[,-1])
corrplot.mixed(M, order = 'AOE')

# Selecting Optimal Lag
# WARNING: Takes 2+ hrs on laptop for this dataset size
lagLenght <- VARselect(BTC, lag.max = 30, type = "const")


# Results of the Various selection criterions
# I only apply SC(n), also known as BIC
#lagLenght$selection
## AIC(n)  HQ(n)  SC(n) FPE(n) 
## 61     31     16     61

lagLenght$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#100     27     16    100



adf.ETH <- adf_testing(ETH,"none","AIC");adf.ETH
# Count         Open         High          Low        Close       Volume         VWAP       Target 
# -49.3816367    0.2783176    0.3003519    0.2172956    0.2611758  -74.6011184    0.2624967 -127.0936085 

diff.ETH   <- diff(ETH,  differences=1);diff.ETH <- diff.ETH[-1, ]  #tempdiff[,1]  <- BTC$Asset_ID ;tempdiff
diff.ETH$Asset_ID <- ETH$Asset_ID[-1,]; diff.ETH  <- Remove_NA(diff.ETH)

adf.ETH <- adf_testing(diff.ETH,"none","AIC");adf.ETH

# Count      Open      High       Low     Close    Volume      VWAP    Target 
# -205.3190 -134.4802 -134.1245 -136.5165 -134.7907 -211.3984 -134.6689 -221.4474 



temp.data <- diff.BTC[-1,]
temp2.data <- diff.ETH[-2, ]
library(MTS)

model1 <- VARX(zt = temp.data[,-1], xt = diff.ETH$Target , p = lagLenght$selection[3])
#Information criteria:  
#  AIC:  43.90753 
# BIC:  44.17327 


# Pred

pred1 <- VARXpred(model1, newxt = diff.ETH$Volume, hstep = 1000, orig = 0)

MTSplot(pred1$pred)
MTSdiag(model = model1)

caus.test <- GrangerTest(X = temp.data[,-1],p = 1)

## VARMA Model

# Decide which VMA order
VMA.model1 <- VMAorder(x = temp.data[ ,-1] , lag = 500)
VMA.model2 <- VMAorder(x = BTC[ ,-1] , lag = 300)

gc()
# 25 lag takes a long time
VARMA.mod1 <- VARMA(as.matrix(temp.data[1:1000, -1]) , p = 3, q = 2)



pred2 <- VARMApred(VARMA.mod1, h = 10)

MTSplot(pred2$pred)

MTSdiag(model = VARMA.mod1)


VARMAX_model1 <- VARMAX



x <- matrix(rnorm(2000), 100, 2)
mvtsplot(x)

mvtsplot(pred2$pred, levels = 14)


library(dplyr)
library(dymo)
model.dymo1 <- dymo( as.data.frame(temp.data[1:24000, -1]), seq_len = 2000, ci = 0.7)



library(dplyr)
install.packages('tdplyr',repos=c('https://r-repo.teradata.com','https://cloud.r-project.org'))


temp.data3 <- temp.data[,-1]
library(Arima)
mod2 <- auto.arima(temp.data3[,8],
                   xreg = temp.data[,-8])
