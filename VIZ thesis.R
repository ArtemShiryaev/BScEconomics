# VIS WORKING 


# 
# p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
# out <- tempfile(fileext = ".tikz")
# tikz(out)
# # Add a red circle in the middle of the plot.
# ggtikz(p, "\\fill[red] (0.5,0.5) circle (2mm);", xy="plot")
# dev.off()

library(tikzDevice)
library(ggplot2)
library(ggtikz)
library(tseries)
library(xtable)
require(graphics)


getwd()

load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/BitcoinCompleteCases.RData")

data <- Bitcoin[1:25000, ]

#rm(Bitcoin)

temp.sum <- summary(data)
desc.stat <- xtable(temp.sum, math.style.exponents = TRUE); digits(desc.stat) <- 2

xtable(desc.stat)

temp.df <- as.data.frame(data$Target)
temp.time <- as.data.frame(data$timestamp)



#plot(data)

#library()

plot.ts(temp.df,
        xlab = "Time",
        ylab = "Target Returns",
        panel = lines#,  #+ geom_abline(aes( temp.time))
        
)
title(
        
        "plot(ts(..), axes=FALSE, ann=FALSE, frame.plot=TRUE, mar..., oma...)")

#+ abline(a = c(5000,10000,20000), color = "red")


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


#abline(v=c(2018.03), col="red")

# library(tidyr)
# library(dplyr)
# 
# ggplot(Bitcoin , aes(x = timestamp , y = Target)) + 
#         geom_line(aes(color = variable), size = 1) +
#         scale_color_manual(values = c("#00AFBB", "#E7B800")) +
#         theme_minimal()

# '
# 
# library(corrplot)
# 
# corrplot(cor(Bitcoin),        # Correlation matrix
#          method = "shade", # Correlation plot method
#          type = "full",    # Correlation plot style (also "upper" and "lower")
#          diag = TRUE,      # If TRUE (default), adds the diagonal
#          tl.col = "black", # Labels color
#          bg = "white",     # Background color
#          title = "Correlation plot",       # Main title
#          col = NULL)       # Color palette'

attach(Bitcoin)



library(tseries)
library(urca)

adf1 <- ur.df(Bitcoin$Target[1:25000],
      type = "none",
      selectlags = "BIC")
summary(adf1)

xtable(summary(adf1))

tempteest <- adf.test(tempdiff,
                      type)

### EXAMPLE


data(Raotbl3)
attach(Raotbl3)
lc.df <- ur.df(y=lc, lags=3, type='trend')
summary(lc.df)


summary(tempteest)
xtable(tempteest)

temp <- pacf(Target, lag.max = 100, plot = T
             )


temp <- pacf(Target[1:25200], lag.max = 100, plot = T,
             main = "Log Returns: First Week of 2018"
)

tempdiff  <- diff(as.ts(Bitcoin$Target[1:25200]), differences=2)


plot(tempdiff,#[1:3500],
     type = "l",
     col = "blue"
     )


tempdiff  <- log(Bitcoin$High[1:25200])


adf2 <- ur.df(tempdiff,
              lags = 100,
              type = "trend",
              selectlags = "BIC")
summary(adf2)
temp <- pacf(tempdiff, lag.max = 40, plot = T,
             main = "Differenced Log Returns: First Week of 2018"
)


tempdiff  <- diff(as.ts(Bitcoin$Target[1:25200]), differences=2)

adf3 <- ur.df(tempdiff,
              lags = 100,
              type = "trend",
              selectlags = "BIC")
library(tseries)
adf.test(tempdiff)

temp <- pacf(tempdiff, lag.max = 100, plot = T,
             main = "Differenced Twice Log Returns: First Week of 2018"
)


tempdiff  <- diff(as.ts(Bitcoin$Target[1:25200]), differences=3)
plot(tempdiff)

temp <- pacf(tempdiff, lag.max = 100, plot = T,
             main = "Differenced Thrice Log Returns: First Week of 2018"
)


tempdiff  <- diff(as.ts(Bitcoin$Target[1:25200]), differences=4)

adf4 <- ur.df(tempdiff,
              lags = 100,
              type = "trend",
              selectlags = "BIC")

temp <- pacf(tempdiff, lag.max = 100, plot = T,
             main = "Differenced Quadrice Log Returns: First Week of 2018"
)

detach(Bitcoin)

rm(Bitcoin)


#########
# Prediction
library(vars) # vec2var
# library(forecast)
# library(tseries)
library(VARshrink)
library(BigVAR)
library(glmnet)
library(onlineVAR)
library(FitAR)
library(lars)

Bitcoin <- scale(Bitcoin)

DiffBitcoin <- diff(as.ts(Bitcoin[1:25200, ]), differences=4);DiffBitcoin <- DiffBitcoin[, -1]

lagLenght <- VARselect(DiffBitcoin[,8], lag.max = 250,
                       exogen = DiffBitcoin[,1:7],
                       type = "const")

xtable(lagLenght)

lagLenght$selection

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 250    249    183    250 

temp.Model1 <- ar(x = DiffBitcoin[1:15000, 8],
                  #aic = T,
                  order.max = 183,#lagLenght$selection[3],
                  exogen = DiffBitcoin[1:15000, 1:7]
)

temp.Forecast1 <- predict(temp.Model1,
                           newdata = DiffBitcoin[15001:20000, 8],
                           n.ahead = 5000,
                           dumvar = DiffBitcoin[15001:20000, 1:7],
                           ci = 0.95
)
plot(temp.Forecast1)
summary(temp.Forecast1)


x11(); par(mai=rep(0.4, 4)); 

plot(x = temp.Forecast1$pred,#[1:360],
                                  type = "l",
                                  main = "Forecasted returns 6 hours",
                                  col = 1
                                  #start = c(0, 0 ), frequency = 60)
) 
lines(DiffBitcoin[15000:20000, 8],#[15001:15361, 8],
      type = "l",
      col = 2)
# lines(data$year,                             # Draw third time series
#       data$ts3,
#       type = "l",
#       col = 4)
legend("topright",                           # Add legend to plot
       c("ts1", "ts2", "ts3"),
       lty = 1,
       col = 2:4)

Model1 <- VAR(DiffBitcoin[1:25196, 7:8],
              exogen = DiffBitcoin[1:25196, 1:7] ,
              p = 183,#lagLenght$selection[3], 
              type = "const",
              lag.max = 250 #lagLenght$selection[1]
              #ic = c("SC")
)

DiffBitcoin2 <- diff(as.ts(Bitcoin[25201:50400, ]), differences=4);DiffBitcoin2 <- DiffBitcoin2[, -1]


Forecast <- predict(Model1, n.ahead = 500, 
                    ci = 0.95, dumvar = DiffBitcoin2[1:500 ,1:7])

x11(); par(mai=rep(0.4, 4)); plot(Forecast)


TimeDiff <- as.ts(Bitcoin[1:25200, ])
DiffBitcoin$timestamp <- as.ts(TimeDiff[1:25196, 1])
DiffBitcoin3 <- as.ts(cbind(DiffBitcoin,TimeDiff[1:25196, 1]))
Model2 <- VARshrink(DiffBitcoin[, 7:8],
                    exogen = DiffBitcoin[, 1:7],
                    p = 183,#lagLenght$selection[3], 
                    type = "const",
                    method = c("ridge")
)



Forecast2 <- predict(object = Model2,
                     n.ahead = 365,
                     dumvar = DiffBitcoin2[1:1000 ,1:7],
                     ci = 0.95
)

x11(); par(mai=rep(0.4, 4)); plot(Forecast2)





Model7 <- cv.lars(x = as.matrix(DiffBitcoin[,1:7]),
                  y = as.matrix(DiffBitcoin[,8]), 
                  K = 100,#nrow(DiffBitcoin), 
                  trace = T,
                  plot.it = TRUE, se = TRUE,
                  max.steps=80,
                  type = c("lasso"),#"lasso"),# "lar", "forward.stagewise", "stepwise"),
                  mode=c("fraction")#, "step"), ...)
)

data(diabetes)

fits <- predict.lars(Model7, #newx = as.data.frame(DiffBitcoin2), 
                     type="coef",
                     mode = "norm")


forecast7 <- predict(Model7,
                     n.ahead = 5000,
                     dumvar = DiffBitcoin2[1:5000 ,1:7],
                     ci = 0.95)
plot(Model7)







Model8 <- lars(x = as.matrix(DiffBitcoin[, 1:7]),
               y = as.matrix(DiffBitcoin[,8]),
               trace = T,
               max.steps= 800,
               type = c("lasso")#"lasso"),# "lar", "forward.stagewise", "stepwise"),
              # normalize = TRUE, intercept = TRUE, 
               #Gram = as.matrix(DiffBitcoin), eps = 1e-12,
               #use.Gram = TRUE
)

Forecast8 <- predict(Model8,
                     newx = as.matrix(DiffBitcoin2[1:500,8]),
                     n.ahead = 500,
                     ci = 0.95
)

plot(Forecast8)
