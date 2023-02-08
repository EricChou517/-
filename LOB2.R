AMZN_LOB <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-08_34200000_57600000_orderbook_30.csv",header = F)
AMZN_Mes <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-08_34200000_57600000_message_30.csv",header = F)

options(digits=7)
AMZN_all_sell <- unique(sort(as.numeric(c(AMZN_LOB[,1],AMZN_LOB[,5],AMZN_LOB[,9],AMZN_LOB[,13],AMZN_LOB[,17])))/10000)
AMZN_all_sell[1:5]
AMZN_sell_LOB <- AMZN_Mes[AMZN_Mes$V2==1 & AMZN_Mes$V6==-1,]
AMZN_sell_LOB1 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[1] , ]
mean(diff(AMZN_sell_LOB1$V1))
AMZN_sell_LOB2 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[2] , ]
mean(diff(AMZN_sell_LOB2$V1))
AMZN_sell_LOB3 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[3] , ]
mean(diff(AMZN_sell_LOB3$V1))
AMZN_sell_LOB4 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[4] , ]
mean(diff(AMZN_sell_LOB4$V1))
AMZN_sell_LOB5 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[5] , ]
mean(diff(AMZN_sell_LOB5$V1))
AMZN_sell_LOB_mean <- c(mean(diff(AMZN_sell_LOB1$V1)),mean(diff(AMZN_sell_LOB2$V1)),mean(diff(AMZN_sell_LOB3$V1)),mean(diff(AMZN_sell_LOB4$V1)),mean(diff(AMZN_sell_LOB5$V1)))

options(digits=7)
AMZN_all_buy <- unique(sort(as.numeric(c(AMZN_LOB[,3],AMZN_LOB[,7],AMZN_LOB[,11],AMZN_LOB[,15],AMZN_LOB[,19])))/10000)
AMZN_all_buy[1:5]
AMZN_buy_LOB <- AMZN_Mes[AMZN_Mes$V2==1 & AMZN_Mes$V6==-1,]
AMZN_buy_LOB1 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[1] , ]
mean(diff(AMZN_buy_LOB1$V1))
AMZN_buy_LOB2 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[2] , ]
mean(diff(AMZN_buy_LOB2$V1))
AMZN_buy_LOB3 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[3] , ]
mean(diff(AMZN_buy_LOB3$V1))
AMZN_buy_LOB4 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[4] , ]
mean(diff(AMZN_buy_LOB4$V1))
AMZN_buy_LOB5 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[5] , ]
mean(diff(AMZN_buy_LOB5$V1))
AMZN_buy_LOB_mean <- c(mean(diff(AMZN_buy_LOB1$V1)),mean(diff(AMZN_buy_LOB2$V1)),mean(diff(AMZN_buy_LOB3$V1)),mean(diff(AMZN_buy_LOB4$V1)),mean(diff(AMZN_buy_LOB5$V1)))



obj <- function(paras){
  alpha=paras[1]
  k=paras[2]
  sum <- (mean(diff(AMZN_sell_LOB1$V1))-(k/(1^alpha)))^2+(mean(diff(AMZN_sell_LOB2$V1))-(k/(2^alpha)))^2+(mean(diff(AMZN_sell_LOB3$V1))-(k/(3^alpha)))^2+(mean(diff(AMZN_sell_LOB4$V1))-(k/(4^alpha)))^2+(mean(diff(AMZN_sell_LOB5$V1))-(k/(5^alpha)))^2
  res <- log(sum)
  finalres <- res
  return(finalres)
}
opt <- optim(c(0.01,0.01) , obj)
alpha1 <- opt$par[1]
k1 <- opt$par[2]
((k1/(1^alpha1)))
((k1/(2^alpha1)))
((k1/(3^alpha1)))
((k1/(4^alpha1)))
((k1/(5^alpha1)))


obj <- function(paras){
  alpha=paras[1]
  k=paras[2]
  sum <- (mean(diff(AMZN_buy_LOB1$V1))-(k/(1^alpha)))+(mean(diff(AMZN_buy_LOB2$V1))-(k/(2^alpha)))+(mean(diff(AMZN_buy_LOB3$V1))-(k/(3^alpha)))+(mean(diff(AMZN_buy_LOB4$V1))-(k/(4^alpha)))+(mean(diff(AMZN_buy_LOB5$V1))-(k/(5^alpha)))
  res <- log(sum)
  finalres <- res
  return(finalres)
}
opt <- optim(c(0.01,0.01) , obj)
alpha1 <- opt$par[1]
k1 <- opt$par[2]
((k1/(1^alpha1)))
((k1/(2^alpha1)))
((k1/(3^alpha1)))
((k1/(4^alpha1)))
((k1/(5^alpha1)))

plot(AMZN_Mes[AMZN_Mes$V2==4,]$V5/10000,type="l")
