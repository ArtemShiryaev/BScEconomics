library(highfrequency)
library(data.table)
library(xts)
library(tidyquant)
library(tibble)
library(quantmod)





data <- as.data.table(TotalCryptoMarketData)
as.xts.data.table

data <- as.xts.data.table(data)


plot(data, col = "blue", lty = "dashed",
     main = "Monthly Cryptocurrency Market Cap in Billions of USD"
     #ylab = "Billions of US Dollars",
     #sub = " in Billions of USD"
     )



data2 <- as.data.table(USTC_ALL_graph_coinmarketcap)

data2 <- data2[,-2]
data2 <- data2[,c(2,1)]


data2 <- as.xts.data.table(data2)
data2 <- data2$marketCap/1000000000

plot(data2, col = "blue", lty = "dashed",
     main = "TerraUSD Open Price in USD",
     ylim = 
     #ylab = "Billions of US Dollars",
     #sub = " in Billions of USD"
)



data3 <- as.data.table(BTC_USD_1_)

data3 <- data3[,-3]
data3 <- data2[,c(2,1)]


data3 <- as.xts.data.table(data3)
data2 <- data2$marketCap/1000000000
tikz('plot.tex', standAlone = TRUE, width=5.5, height=5.5)


plot(data3, col = "blue", lty = "dashed",
     main = "Bitcoin Open Price in USD",
     ylim = 
       #ylab = "Billions of US Dollars",
       #sub = " in Billions of USD"
)


dev.off()









load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")


BTC  <- train[train$Asset_ID == 1, ]  # Bitcoin

rm(train)
gc()


getwd()
setwd("C:/Users/zerzy/Desktop/BSc Economics Revision 2023/RANDOM_PLOTS")

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
                      "mvtsplot",
                      "aTSA")
# Install if applicable
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages, require, character.only = TRUE)


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


BTC <- Both_fun(BTC);

tt <- 60
BTC_temp_data  <- aggregateTS(BTC[43:282], FUN =  "mean",  alignBy = "minutes", alignPeriod = tt);gc()



plot(temp)

data_temp <- BTC$Target[43:283]
temp_returns <- rep(BTC_temp_data$Target[1], 60)
temp_returns2 <- rep(BTC_temp_data$Target[2], 60)
temp_returns3 <- rep(BTC_temp_data$Target[3], 60)
temp_returns4 <- rep(BTC_temp_data$Target[4], 61)

com_returns <- c(temp_returns,temp_returns2,temp_returns3,temp_returns4)

data_temp$Target <- com_returns

#temp <- BTC_temp_data$Target







#Load the tikzDevice package, if you dont have it, install with:
library(tikzDevice)
# install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
require(tikzDevice)

# The following wwill create normal.tex in the working
# directory the first time this is run it may take a long time because the
# process of calulating string widths for proper placement is
# computationally intensive, the results will get cached for the current R
# session or will get permenantly cached if you set
# options( tikzMetricsDictionary='/path/to/dictionary' ) which will be
# created if it does not exist.  Also if the flag standAlone is not set to
# TRUE then a file is created which can be included with \include{}
tikz('PLOT2.tex', standAlone = TRUE, width=5, height=5)



plot(BTC$Target[43:283], col = "blue", lty = "dashed",
     main = "Bitcoin Returns 240 minutes",
     ylab = "Minute Returns")
     #xlab = "Minute TIme")
     legend(x = "bottomright", legend=c("Minute Returns","Hourly Returns"), col=c("blue","red"), lty=c(2,1), ncol=1, cex = 1.5)
     lines(data_temp, col = "red", pch = "*", type="l", lwd=2.0)


#Close the device
dev.off()

# Compile the tex file
tools::texi2dvi('PLOT2.tex',pdf=T)


