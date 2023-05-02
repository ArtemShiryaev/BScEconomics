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


