#======================================
# Umeå University
# Bachelor Thesis in Economics
# Spring Semester 2022
# Artem Angelchev Shiryaev
# Revision and Updates
#======================================



# Revision Date 2023-04-30




#######################################
######## Libraries & Packages #########
#######################################



getwd()
#setwd()

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace
gc()

#######################################
############# DATA PROCESSING #########
#######################################


## Loading data
# Original dataset NOT INCLUDED ON GITHUB DUE TO SIZE

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")

# # Checking missing values
# sum(is.na(as.matrix(train)))
# #  750347
# 
# #Complete cases omiiting 
# train <- na.omit(train) 
# 
# sum(is.na(as.matrix(train)))
# # 0


library(highfrequency)
library(data.table)
library(xts)
library(tidyquant)
library(tibble)
library(quantmod)



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

rm(train)



# OWN FUNCTIONS
# Fixing time series orders
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

##### Remove NA

Remove_NA <- function(r){
  if (sum(is.na(as.matrix(r))) == 0){
  return(r)
  } else {
    d <- na.omit(r)
    return(d)
    }
  
}

Both_fun <- function(r){
  r <- Fix_time(r)
  r <- Remove_NA(r)
  return(r)
}



BNB <- Both_fun(BNB); BTC <- Both_fun(BTC); BCH <- Both_fun(BCH); ADA <- Both_fun(ADA); DOGE <- Both_fun(DOGE);
EOS <- Both_fun(EOS); ETH <- Both_fun(ETH); ETC <- Both_fun(ETC); IOTA <- Both_fun(IOTA); LTC <- Both_fun(LTC);
MKR <- Both_fun(MKR); XMR <- Both_fun(XMR); XLM <- Both_fun(XLM); TRX <- Both_fun(TRX)




##### AGGREGATE THE TIME SERIES TO REDUCE SIZE OF DATASET

tt <- 60 # 60 mins instead of 1 minute hfd
BNB  <- aggregateTS(BNB,  alignBy = "minutes", alignPeriod = tt) # Binance Coin
BTC  <- aggregateTS(BTC,  alignBy = "minutes", alignPeriod = tt) # Bitcoin
BCH  <- aggregateTS(BCH,  alignBy = "minutes", alignPeriod = tt) # Bitcoin Cash
ADA  <- aggregateTS(ADA,  alignBy = "minutes", alignPeriod = tt) # Cardano 
DOGE <- aggregateTS(DOGE, alignBy = "minutes", alignPeriod = tt) # Dogecoin
EOS  <- aggregateTS(EOS,  alignBy = "minutes", alignPeriod = tt) # EOS.IO
ETH  <- aggregateTS(ETH,  alignBy = "minutes", alignPeriod = tt) # Ethererum
ETC  <- aggregateTS(ETC,  alignBy = "minutes", alignPeriod = tt) # Ethereum Classic
IOTA <- aggregateTS(IOTA, alignBy = "minutes", alignPeriod = tt) # IOTA
LTC  <- aggregateTS(LTC,  alignBy = "minutes", alignPeriod = tt) # Litecoin
MKR  <- aggregateTS(MKR,  alignBy = "minutes", alignPeriod = tt) # Maker
XMR  <- aggregateTS(XMR,  alignBy = "minutes", alignPeriod = tt) # Monero
XLM  <- aggregateTS(XLM,  alignBy = "minutes", alignPeriod = tt) # Stellar
TRX  <- aggregateTS(TRX,  alignBy = "minutes", alignPeriod = tt) # Tron



gc()





# FIRST CHECK BTC
data <- BTC

#######################################
#### CHECKING STATIONARITY WITH ADF ###
#######################################
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
  
adf.BTC <- adf_testing(BTC,"none","AIC");adf.BTC
# Count         Open         High          Low        Close       Volume         VWAP       Target 
#-50.1421128    0.2303747    0.2423791    0.1848168    0.2211279  -73.2204551    0.2227553 -127.5955009 


diff.BTC  <- diff(BTC,  differences=1);diff.BTC <- diff.BTC[-1, ]  #tempdiff[,1]  <- BTC$Asset_ID ;tempdiff
diff.BTC$Asset_ID <- BTC$Asset_ID[-1,]; diff.BTC  <- Remove_NA(diff.BTC)

adf.BTC <- adf_testing(diff.BTC,"none","AIC");adf.BTC

# Count      Open      High       Low     Close    Volume      VWAP    Target 
#-205.2493 -131.3066 -130.3958 -135.2840 -131.2498 -211.8842 -131.4471 -219.7688 



# Selecting Optimal Lag
# WARNING: Takes 2+ hrs on laptop for this dataset size
lagLenght <- VARselect(diff.BTC, lag.max = 100, type = "const")


# Results of the Various selection criterions
# I only apply SC(n), also known as BIC
lagLenght$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 61     31     16     61

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
VMA.model1 <- VMAorder(x = t(temp.data[ ,-1]) , lag = 25)


library(ggplot2)
library(vars)
library(VARshrink)
library(BigVAR)
library(glmnet)
library(lars)
library(urca)
library(forecast)

