# WHOLE SCRIPT



library(tikzDevice)
library(ggplot2)
library(ggtikz)
library(tseries)
library(xtable)
library(vars)
require(graphics)


getwd()

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/BitcoinCompleteCases.RData")
load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")



ks.test(x = Bitcoin$Target, y = pnorm, B = 2000)

ETH <- train[train$Asset_ID == 6, ]
Litecoin <- train[train$Asset_ID == 9, ]
Bitcoin <- train[train$Asset_ID == 1, ]



joint.data <- rbind(Bitcoin,ETH,Litecoin)


sum(is.na(as.matrix(joint.data)))
# 1165

joint.data <- na.omit(joint.data) 

sum(is.na(as.matrix(joint.data)))
# 0 

data <- Bitcoin[1:25200, ]

# VIZ
z <- #ts(as.matrix(Bitcoin[1:15000, -1 ],9 ),
  ts(as.matrix(Bitcoin[1:1577880, -1 ],9 ),
     start = c(2018, 0 ), frequency = 525960)
plot(z, yax.flip = TRUE,
     col = "blue",
     main = "Bitcoin 2018 - 2021"
)


zz <- #ts(as.matrix(Bitcoin[1:15000, -1 ],9 ),
  ts(as.matrix(Bitcoin[1:25200, -1 ],9 ),
     start = c(2018, 0 ), frequency = 525960)
plot(zz, yax.flip = TRUE,
     col = "forestgreen",
     main = "First week of 2018"
)


tempdiff <- diff(as.ts(Bitcoin), differences=1);tempdiff <- tempdiff[, -1]


zzz <- #ts(as.matrix(Bitcoin[1:15000, -1 ],9 ),
  ts(as.matrix(tempdiff[1:25200, ],8 ),
     start = c(2018, 0 ), frequency = 525960)
plot(zzz, yax.flip = TRUE,
     col = "blue",
     main = "Differenced First Week of 2018"
)


BTC.USD.Plot <- ts(BTC.USD$Adj.Close,
                   start = c(2015, 0 ), frequency = 365)

plot(BTC.USD.Plot, yax.flip = TRUE,
     col = "blue",
     main = "Bitcoin adj. prices 2015 - 2022"
)





# Unit Root Tests

attach(data)
adf1 <- ur.df(Target,
              type = "none",
              selectlags = "BIC"); adf1

plot(Target)



DiffBitcoin <- diff(as.ts(Bitcoin[1:25200, ]), differences=1);DiffBitcoin <- DiffBitcoin[, -1]

plot(DiffBitcoin)

ARX.Model <- ar(DiffBitcoin[, 8], 
                aic = T,
                order.max = 500,
                p = 183,
                lag.max = 183,
                exogen = DiffBitcoin[ ,1:7],
                type = "both",
                se.fit = T

)


plot(ARX.Model$partialacf, type = "l")

plot(ARX.Model$resid, type = "l",
     ylab = "ARX Residuals",
     xlab = "Time",
     main = "Residuals of ARX Model on First week of 2018",
     col  = 4
)

ks.test(x = ARX.Model$resid, y = pnorm, alternative = "two.sided")



# 
# test <- arima(DiffBitcoin[, 10], 
#               order = c(29, 0L, 0L),
#               xreg = data[, 3:9],
#               #method = "ML",
#               transform.pars = T
#   
#   
# );test



pacf(DiffBitcoin[,8])





newdata <- Bitcoin[25201:50400, ]

DiffBitcoin2 <- diff(as.ts(Bitcoin[25201:50400, ]), differences=1);DiffBitcoin2 <- DiffBitcoin2[, -1]


# Forecast
Forecast1 <- predict(ARX.Model, newxreg = DiffBitcoin2,
                     n.ahead = 25200,
                     ci = 0.95)
od <- options(digits = 22)
x11(); par(mai=rep(0.4, 4)); plot(Forecast1)

temp.data <- DiffBitcoin2[1:360, 8]


plot(temp.data,
        ylab = "Returns",
        xlab = "Time",
        main = "Forecasted Returns 6 Hours, ARX Model",
        col = "red",
     type = "l"
        )


lines(Forecast1$pred[1:360],
      col = "blue")
lines(Forecast1$se[1:360],
      col = 9,
      type = "S")
lines(-Forecast1$se[1:360],
      col = 9,
      type = "S"
      )





library(VARshrink)
library(BigVAR)
library(glmnet)
library(onlineVAR)
#library(FitAR)
library(lars)
library(forecast)













library(vars)

lagselect <- VARselect(DiffBitcoin, 
                       lag.max = 200
                       )



lagLenght <- VARselect(DiffBitcoin, lag.max = 250,
                       exogen = DiffBitcoin,
                       type = "const")

Model2 <- VARshrink(DiffBitcoin,
                    #exogen = DiffBitcoin,
                    p = 183,#lagLenght$selection[3], 
                    type = "const",
                    method = c("ridge")
)

summary(Model2)

summary(Model2$varresult$Target)

library(vars)

Forecast2 <- predict(n.ahead = 3600,
                     #dumvar = DiffBitcoin2[1:25199, ],
                     ci = 0.95,
                     object = Model2
)

x11(); 
par(mai=rep(0.4, 4)); 
plot(Forecast2)#     title = "Forecasted 10 Hours VARX - Ridge Model")

FC.Target <- as.ts(Forecast2$fcst$Target)


x11(); par(mai=rep(0.4, 4)); 

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(FC.Target[, 1],
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

lines(DiffBitcoin2[1:360, 8],
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



lines(Forecast1$se[1:360],
      col = 9,
      type = "S")
lines(-Forecast1$se[1:360],
      col = 9,
      type = "S"
)



ridge.res <- Model2[["varresult"]][["Target"]][["residuals"]]

ks.test(x = ridge.res, y = pnorm, alternative = "two.sided")

ks.test(x = lasso.res, y = pnorm, alternative = "two.sided")


xxx <- rnorm(50)
yyy <- rnorm(30)
# Do x and y come from the same distribution?
ks.test(xxx, yyy)

## LASSO

library(lars)

Model3 <- lars(x     = as.matrix(DiffBitcoin[ ,1:7]),
               y     = as.matrix(DiffBitcoin[ ,8]),
               type  = "lasso",
               trace = T,
               normalize = T,
               intercept = T,
               use.Gram = T
               )
plot(Model3)


summary(Model3)


Forecast3 <- predict(Model3,
                    newx = DiffBitcoin2[ , 1:7],
                    type = "fit",
                    Mode="lambda"
                    )
Forecast3

plot(ts(Forecast3$fit[1:25200,8]))
library(graphics)
plot(DiffBitcoin2[1:25000 , 8],
      col = "red",
     type = "l",
     main = "VARX-LASSO, Weekly Forecast",
     ylab = "Returns",
     xlab = "Time"
     )

lines(ts(Forecast3$fit[1:25000,8]))


legend(#"top", 
  #x = 325  , y  = 0.1,
  "topright",
  legend = c("Forecast", 
             "95% CI",
             #"Lowest",
             #"Highest",
             "Real Values"), 
  col = c("red","blue", "3","5", "9"), 
  pch = c(17,19), 
  bty = "n", 
  #pt.cex = 2, 
  #cex = 1.2, 
  text.col = "black", 
  #inset = c(-0.25, 0.1),
  title="Time Series"
)



