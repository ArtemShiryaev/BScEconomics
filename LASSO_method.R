library(BigVAR)
#> Loading required package: lattice
data(Y)

# 
# # 3 x 7 coefficient matrix
# B = BigVAR.fit(Y,struct='Basic',p=2,lambda=1)[,,1]
# # co gran=c(50,10),
                         # h=1,
                         # cv="Rolling",
                         # verbose= T,
                         # IC=TRUE,
                         # model.controls=list(intercept=TRUE),
                         # #VARX = DiffBitcoin[1:25199, 1:7]
                         # )

library(glmnet)
Forecast4 <- cv.BigVAR(Model4)
# Forecast4nstruct 7 x 99 lag matrix of Y
# Z = VARXLagCons(Y,p=2,oos=TRUE)$Z
# # obtain out of sample forecasts
# yhat = B%*%Z[,ncol(Z),drop=F]
# 
# 

Model4 <- constructModel(DiffBitcoin[1:600, ],
                         p=183,
                         struct = "Basic",
                         gran = c(50,10))
Model5 <- constructModel(DiffBitcoin[1:600, ],
                        p=183,
                        struct = "Basic",
                        gran = c(50,10))

Model4

Model5 

library(glmnet)
Forecast4 <- cv.BigVAR(Model4)
                        
plot(Forecast4,
     title = "VARX - Lasso")

summary(Forecast4)


sum(!is.na(coef[8,]))


plot(coef[8,1:5],
     type = "l")
Forecast4.Pred <- predict(coef[8,], 
                          n.ahead = 360,
                          ci = 0.95)

plot(Forecast4.Pred,
     type = "l")



lasso.res <- Forecast4@resids

fitted <- Forecast4@fitted

plot(fitted[,8])

Forecast1


lasso.coef <- lasso.beta[8,1:1465,1:200]

plot(lasso.coef[,1])
