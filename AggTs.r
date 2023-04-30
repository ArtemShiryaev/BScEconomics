library(highfrequency)
library(data.table)
library(xts)
library(tidyquant)
library(tibble)



test <- as.ts(Bitcoin)
test <- xts(test[,-1], order.by= as.matrix(test[,1]), frequency = 525960)



ti= seq(from = ymd_hm("1970-01-01 00:00"),
        to = ymd_hm("2021-09-21 00:00"), by =  "1 min", tz = "UTC")

tbl <- tibble(t =ti,
              x = 1:length(t))
test$timestamp <- ti[25246759:27203040]

test2 <- test

test2 <- as.xts(test2)


test2 <- as.data.table(test2)



tsagg5min <- aggregateTS(test2, alignBy = "minutes", alignPeriod = 60)
head(tsagg5min)

temp <- as.ts(tsagg5min)


plot.ts(temp)
plot(tsagg5min
     )

