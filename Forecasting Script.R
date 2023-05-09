load("C:/Users/zerzy/Documents/GitHub/BScEconomics/AggregatedDatasetFinal.RData")


library(forecast)
#32605

mod1 <- auto.arima(as.ts(BTC$Target[1:20000]),
                   xreg= as.ts(BTC[1:20000,2:7])
)

forecasted.mod1 <- forecast(mod1, h = 12605, 
                            xreg =as.ts(BTC[20001:32605,2:7] )
)
plot(forecasted.mod1)
lines(as.ts(BTC$Target[20001:32605]), col = "red", lty = c ("blank"))
