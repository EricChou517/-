AMZN_LOB <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-02_34200000_57600000_orderbook_30.csv",header = F)
AMZN_Mes <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-02_34200000_57600000_message_30.csv",header = F)

AMZN_LOB <- AMZN_LOB[1:length(AMZN_LOB)-1,]
AMZN_Mes <- AMZN_LOB[2:length(AMZN_Mes),]

for (i in seq(5,100,20)){
t1.ask <- as.numeric(sort(AMZN_LOB[i,seq(1,20,4)]/10000)) #賣價，低到高
t1.ask.V <- as.numeric(AMZN_LOB[i,seq(2,21,4)])
t1.bid <- as.numeric(sort(AMZN_LOB[i,seq(3,22,4)]/10000)) #買價
t1.bid.V <- as.numeric(AMZN_LOB[i,seq(4,23,4)])

png(filename=paste0("AMZN_row",i,".png"),width = 804)
barplot(c(-t1.bid.V,t1.ask.V),c(t1.bid,t1.ask),axes = TRUE,col=c(rep("skyblue",5),rep("red",5)),names.arg=c(t1.bid,t1.ask))
dev.off()

}


FB_LOB <- read.csv("/Volumes/Fast SSD/FB_2015-01-01_2015-06-30_30/FB_2015-01-02_34200000_57600000_orderbook_30.csv",header = F)
FB_Mes <- read.csv("/Volumes/Fast SSD/FB_2015-01-01_2015-06-30_30/FB_2015-01-02_34200000_57600000_message_30.csv",header = F)


for (i in seq(5,10000,2000)){
  t1.ask <- as.numeric(sort(FB_LOB[i,seq(1,20,4)]/10000)) #賣價，低到高
  t1.ask.V <- as.numeric(FB_LOB[i,seq(2,21,4)])
  t1.bid <- as.numeric(sort(FB_LOB[i,seq(3,22,4)]/10000)) #買價
  t1.bid.V <- as.numeric(FB_LOB[i,seq(4,23,4)])
  
  png(filename=paste0("FB_row",i,".png"),width = 804)
  barplot(c(-t1.bid.V,t1.ask.V),c(t1.bid,t1.ask),axes = TRUE,col=c(rep("skyblue",5),rep("red",5)),names.arg=c(t1.bid,t1.ask))
  dev.off()
  
}


options(digits=7)
AMZN_all_sell <- unique(sort(as.numeric(c(AMZN_LOB[,1],AMZN_LOB[,5],AMZN_LOB[,9],AMZN_LOB[,13],AMZN_LOB[,17])))/10000)
AMZN_all_sell[1:5]
AMZN_sell_LOB <- AMZN_Mes[AMZN_Mes$V2==1 & AMZN_Mes$V6==-1,]
AMZN_sell_LOB1 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[1] , ]
AMZN_sell_LOB2 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[2] , ]
AMZN_sell_LOB3 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[3] , ]
AMZN_sell_LOB4 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[4] , ]
AMZN_sell_LOB5 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_sell[5] , ]
AMZN_sell_LOB <- rbind(AMZN_sell_LOB1,AMZN_sell_LOB2,AMZN_sell_LOB3,AMZN_sell_LOB4,AMZN_sell_LOB5)
AMZN_sell_LOB$V7 <- as.numeric(rownames(AMZN_sell_LOB))
AMZN_sell_LOB <- AMZN_sell_LOB[order(AMZN_sell_LOB$V7),]
AMZN_sell_LOB <- AMZN_sell_LOB[,1:6]
mean(diff(AMZN_sell_LOB$V1))

options(digits=7)
AMZN_all_buy <- unique(sort(as.numeric(c(AMZN_LOB[,3],AMZN_LOB[,7],AMZN_LOB[,11],AMZN_LOB[,15],AMZN_LOB[,19])))/10000)
AMZN_all_buy[1:5]
AMZN_buy_LOB <- AMZN_Mes[AMZN_Mes$V2==1 & AMZN_Mes$V6==-1,]
AMZN_buy_LOB1 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[1] , ]
AMZN_buy_LOB2 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[2] , ]
AMZN_buy_LOB3 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[3] , ]
AMZN_buy_LOB4 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[4] , ]
AMZN_buy_LOB5 <- AMZN_Mes[AMZN_Mes$V5/10000 == AMZN_all_buy[5] , ]
AMZN_buy_LOB <- rbind(AMZN_buy_LOB1,AMZN_buy_LOB2,AMZN_buy_LOB3,AMZN_buy_LOB4,AMZN_buy_LOB5)
AMZN_buy_LOB$V7 <- as.numeric(rownames(AMZN_buy_LOB))
AMZN_buy_LOB <- AMZN_buy_LOB[order(AMZN_buy_LOB$V7),]
AMZN_buy_LOB <- AMZN_buy_LOB[,1:6]
mean(diff(AMZN_buy_LOB$V1))



FB_all_sell <- unique(sort(as.numeric(c(FB_LOB[,1],FB_LOB[,5],FB_LOB[,9],FB_LOB[,13],FB_LOB[,17])))/10000)
FB_all_sell[1:5]
FB_sell_LOB <- FB_Mes[FB_Mes$V2==1 & FB_Mes$V6==-1,]
FB_sell_LOB1 <- FB_Mes[FB_Mes$V5/10000 == FB_all_sell[1] , ]
mean(diff(FB_sell_LOB1$V1))
FB_sell_LOB2 <- FB_Mes[FB_Mes$V5/10000 == FB_all_sell[2] , ]
mean(diff(FB_sell_LOB2$V1))
FB_sell_LOB3 <- FB_Mes[FB_Mes$V5/10000 == FB_all_sell[3] , ]
mean(diff(FB_sell_LOB3$V1))
FB_sell_LOB4 <- FB_Mes[FB_Mes$V5/10000 == FB_all_sell[4] , ]
mean(diff(FB_sell_LOB4$V1))
FB_sell_LOB5 <- FB_Mes[FB_Mes$V5/10000 == FB_all_sell[5] , ]
mean(diff(FB_sell_LOB5$V1))
FB_sell_LOB <- rbind(FB_sell_LOB1,FB_sell_LOB2,FB_sell_LOB3,FB_sell_LOB4,FB_sell_LOB5)
FB_sell_LOB$V7 <- as.numeric(rownames(FB_sell_LOB))
FB_sell_LOB <- FB_sell_LOB[order(FB_sell_LOB$V7),]
FB_sell_LOB <- FB_sell_LOB[,1:6]
mean(diff(FB_sell_LOB$V1))

options(digits=7)
FB_all_buy <- unique(sort(as.numeric(c(FB_LOB[,3],FB_LOB[,7],FB_LOB[,11],FB_LOB[,15],FB_LOB[,19])))/10000)
FB_all_buy[1:5]
FB_buy_LOB <- FB_Mes[FB_Mes$V2==1 & FB_Mes$V6==-1,]
FB_buy_LOB1 <- FB_Mes[FB_Mes$V5/10000 == FB_all_buy[1] , ]
mean(diff(FB_buy_LOB1$V1))
FB_buy_LOB2 <- FB_Mes[FB_Mes$V5/10000 == FB_all_buy[2] , ]
mean(diff(FB_buy_LOB2$V1))
FB_buy_LOB3 <- FB_Mes[FB_Mes$V5/10000 == FB_all_buy[3] , ]
mean(diff(FB_buy_LOB3$V1))
FB_buy_LOB4 <- FB_Mes[FB_Mes$V5/10000 == FB_all_buy[4] , ]
mean(diff(FB_buy_LOB4$V1))
FB_buy_LOB5 <- FB_Mes[FB_Mes$V5/10000 == FB_all_buy[5] , ]
mean(diff(FB_buy_LOB5$V1))
FB_buy_LOB <- rbind(FB_buy_LOB1,FB_buy_LOB2,FB_buy_LOB3,FB_buy_LOB4,FB_buy_LOB5)
FB_buy_LOB$V7 <- as.numeric(rownames(FB_buy_LOB))
FB_buy_LOB <- FB_buy_LOB[order(FB_buy_LOB$V7),]
FB_buy_LOB <- FB_buy_LOB[,1:6]
mean(diff(FB_buy_LOB$V1))


#sell

obj <- function(paras){
  alpha=paras[1]
  k=paras[2]
  sum <- (mean(diff(FB_sell_LOB1$V1))-(k/(1^alpha)))+(mean(diff(FB_sell_LOB2$V1))-(k/(2^alpha)))+(mean(diff(FB_sell_LOB3$V1))-(k/(3^alpha)))+(mean(diff(FB_sell_LOB4$V1))-(k/(4^alpha)))+(mean(diff(FB_sell_LOB5$V1))-(k/(5^alpha)))
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
  sum <- (mean(diff(FB_buy_LOB1$V1))-(k/(1^alpha)))+(mean(diff(FB_buy_LOB2$V1))-(k/(2^alpha)))+(mean(diff(FB_buy_LOB3$V1))-(k/(3^alpha)))+(mean(diff(FB_buy_LOB4$V1))-(k/(4^alpha)))+(mean(diff(FB_buy_LOB5$V1))-(k/(5^alpha)))
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


####

FB_LOB <- read.csv("/Volumes/Fast SSD/FB_2015-01-01_2015-06-30_30/FB_2015-01-05_34200000_57600000_orderbook_30.csv",header = F)
FB_Mes <- read.csv("/Volumes/Fast SSD/FB_2015-01-01_2015-06-30_30/FB_2015-01-05_34200000_57600000_message_30.csv",header = F)

FB_LOB <- FB_LOB[1:length(FB_LOB$V1)-1,]
FB_Mes <- FB_Mes[2:length(FB_Mes$V2),]

FB_all <- cbind(FB_Mes,FB_LOB)
FB_all <- FB_all[FB_all$V6==1,]
FB_all <- FB_all[FB_all$V2==2 | FB_all$V2==3 ,]
FB_Mes <- FB_all[,1:6]
FB_LOB <- FB_all[,7:126]

options(digits=14)

#length(FB_LOB$V1)
FB_sell_LOB <- data.frame(rbind(rep(NA,6)))
colnames(FB_sell_LOB) <- c("V1","V2","V3","V4","V5","V6")
FB_sell_LOB <- na.omit(FB_sell_LOB)
length(FB_Mes$V1)
index <- c()
for (i in 1:length(FB_Mes$V1)){
  FB_all_price <- sort(as.numeric(c(FB_LOB[i,3],FB_LOB[i,7],FB_LOB[i,11],FB_LOB[i,15],FB_LOB[i,19]))/10000)
  if((FB_Mes[i,]$V5/10000 == FB_all_price[1] | FB_Mes[i,]$V5/10000 == FB_all_price[2] | FB_Mes[i,]$V5/10000 == FB_all_price[3] | FB_Mes[i,]$V5/10000 == FB_all_price[4] | FB_Mes[i,]$V5/10000 == FB_all_price[5]) == TRUE)
  {index <- c(index,i)
  print(i)
  }
}

FB_sell_LOB <- FB_Mes[index,]

FB_sell_LOB <- na.omit(FB_sell_LOB)
FB_all_price <- unique(sort(as.numeric(c(FB_LOB[i,3],FB_LOB[i,7],FB_LOB[i,11],FB_LOB[i,15],FB_LOB[i,19])))/10000)
FB_all_price <- FB_all_price[(length(FB_all_price)-4):length(FB_all_price)]
FB_all_price <- rev(FB_all_price)

FB_sell_LOB1 <- FB_sell_LOB[FB_sell_LOB$V5/10000 == FB_all_price[1] , ]
#length(FB_sell_LOB1$V1)/23400
#1/(length(FB_sell_LOB1$V1)/23400)
mean(diff(FB_sell_LOB1$V1))
mean(FB_sell_LOB1$V4)/mean(diff(FB_sell_LOB1$V1))

FB_sell_LOB2 <- FB_sell_LOB[FB_sell_LOB$V5/10000 == FB_all_price[2] , ]
mean(diff(FB_sell_LOB2$V1))
mean(FB_sell_LOB2$V4)/mean(diff(FB_sell_LOB2$V1))

FB_sell_LOB3 <- FB_sell_LOB[FB_sell_LOB$V5/10000 == FB_all_price[3] , ]
mean(diff(FB_sell_LOB3$V1))
mean(FB_sell_LOB3$V4)/mean(diff(FB_sell_LOB3$V1))

FB_sell_LOB4 <- FB_sell_LOB[FB_sell_LOB$V5/10000 == FB_all_price[4] , ]
mean(diff(FB_sell_LOB4$V1))
mean(FB_sell_LOB4$V4)/mean(diff(FB_sell_LOB4$V1))

FB_sell_LOB5 <- FB_sell_LOB[FB_sell_LOB$V5/10000 == FB_all_price[5] , ]
mean(diff(FB_sell_LOB5$V1))
mean(FB_sell_LOB5$V4)/mean(diff(FB_sell_LOB5$V1))

sell <- c(mean(FB_sell_LOB1$V4)/mean(diff(FB_sell_LOB1$V1)),mean(FB_sell_LOB2$V4)/mean(diff(FB_sell_LOB2$V1)),mean(FB_sell_LOB3$V4)/mean(diff(FB_sell_LOB3$V1)),mean(FB_sell_LOB4$V4)/mean(diff(FB_sell_LOB4$V1)),mean(FB_sell_LOB5$V4)/mean(diff(FB_sell_LOB5$V1)))

sell <- c(82.29,61.15,57.27,55.35,42.43)
logsell <- c()
for(i in 1:5){
  logsell[i] <- log(sell[i])
}

x <- c(1:5)
parabola <- lm(logsell~I(x)+I(x^2))
line <- parabola$coefficients[1]+parabola$coefficients[2]*x+parabola$coefficients[3]*(x^2)
barplot(sell)
lines(exp(line),col="red")

###

bestsell <- FB_LOB[,1]/10000
bestbuy <- FB_LOB[,3]/10000
spread <- round((bestsell-bestbuy)*100)
spread.ratio <- c()
for(i in 1:8){
  spread.ratio[i] <- log(length(spread[spread==i])/length(spread))
}

x <- c(1:8)
parabola <- lm(spread.ratio~I(x)+I(x^2))
line <- parabola$coefficients[1]+parabola$coefficients[2]*x+parabola$coefficients[3]*(x^2)
hist(spread,probability = T,xlim = c(1,8))
lines(exp(line),col="red")

