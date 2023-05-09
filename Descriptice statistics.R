##### Descriptive statistics

library(summarytools)

gc()


load("~/GitHub/Big Data/Project Deep Learning/CNN_LSTM_Project/DatasetKaggle.RData")

df.descriptive <- summary(train)
df.descriptive

df.by.descriptive <- by(train, train$Asset_ID, summary)
df.by.descriptive


df.df.sum <- dfSummary(train)

df.df.sum 




library(reporttools)
data("heart", package = "survival") # load the jasa dataset
vars0 <- with(jasa, data.frame("Transplantation" = factor(jasa$transplant, 
                                                          levels = 0:1, 
                                                          labels = c("no", "yes")),
                               "Age" = jasa$age,
                               "Surgery" = factor(jasa$surgery,
                                                  levels = 0:1,
                                                  labels = c("no", "yes")),
                               "Survival status" = factor(jasa$fustat, levels = 0:1,
                                                          labels = c("alive", "dead")),
                               "HLA A2 score" = jasa$hla.a2, "Birthday" = jasa$birth.dt,
                               "Acceptance into program" = jasa$accept.dt,
                               "End of follow up" = jasa$fu.date, "Follow up time" = futime,
                               "Mismatch score" = mscore, check.names = FALSE))
attach(vars0, warn.conflicts = FALSE)


vars1 <- vars0[, c("Surgery", "Survival status", "HLA A2 score")]
cap1 <- "Patient characteristics: nominal variables."
tableNominal(vars = vars1, cap = cap1, vertical = FALSE, lab = "tab: nominal1", longtable = FALSE)



library(stargazer)


star.df <- stargazer(train)

BNB  <- stargazer(train[train$Asset_ID == 0, ])  # Binance Coin
BTC  <- stargazer(train[train$Asset_ID == 1, ])  # Bitcoin
BCH  <- stargazer(train[train$Asset_ID == 2, ])  # Bitcoin Cash
ADA  <- stargazer(train[train$Asset_ID == 3, ])  # Cardano 
DOGE <- stargazer(train[train$Asset_ID == 4, ])  # Dogecoin
EOS  <- stargazer(train[train$Asset_ID == 5, ])  # EOS.IO
ETH  <- stargazer(train[train$Asset_ID == 6, ])  # Ethererum
ETC  <- stargazer(train[train$Asset_ID == 7, ])  # Ethereum Classic
IOTA <- stargazer(train[train$Asset_ID == 8, ])  # IOTA
LTC  <- stargazer(train[train$Asset_ID == 9, ])  # Litecoin
MKR  <- stargazer(train[train$Asset_ID == 10, ]) # Maker
XMR  <- stargazer(train[train$Asset_ID == 11, ]) # Monero
XLM  <- stargazer(train[train$Asset_ID == 12, ]) # Stellar
TRX  <- stargazer(train[train$Asset_ID == 13, ]) # Tron






load("~/GitHub/BScEconomics/AggregatedDatasetFinal.RData")



stargazer(as.data.frame.ts(BNB))
stargazer(as.data.frame.ts(BTC))#  <- stargazer(train[train$Asset_ID == 1, ])  # Bitcoin
stargazer(as.data.frame.ts(BCH))  #<- stargazer(train[train$Asset_ID == 2, ])  # Bitcoin Cash
stargazer(as.data.frame.ts(ADA))  #<- stargazer(train[train$Asset_ID == 3, ])  # Cardano 
stargazer(as.data.frame.ts(DOGE)) #<- stargazer(train[train$Asset_ID == 4, ])  # Dogecoin
stargazer(as.data.frame.ts(EOS))  #<- stargazer(train[train$Asset_ID == 5, ])  # EOS.IO
stargazer(as.data.frame.ts(ETH))  #<- stargazer(train[train$Asset_ID == 6, ])  # Ethererum
stargazer(as.data.frame.ts(ETC))  #<- stargazer(train[train$Asset_ID == 7, ])  # Ethereum Classic
stargazer(as.data.frame.ts(IOTA)) #<- stargazer(train[train$Asset_ID == 8, ])  # IOTA
stargazer(as.data.frame.ts(LTC))  #<- stargazer(train[train$Asset_ID == 9, ])  # Litecoin
stargazer(as.data.frame.ts(MKR))  #<- stargazer(train[train$Asset_ID == 10, ]) # Maker
stargazer(as.data.frame.ts(XMR))  #<- stargazer(train[train$Asset_ID == 11, ]) # Monero
stargazer(as.data.frame.ts(XLM))  #<- stargazer(train[train$Asset_ID == 12, ]) # Stellar
stargazer(as.data.frame.ts(TRX))  #<- stargazer(train[train$Asset_ID == 13, ]) # Tron
