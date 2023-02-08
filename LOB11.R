AMZN_LOB <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-02_34200000_57600000_orderbook_30.csv",header = F)
AMZN_Mes <- read.csv("/Volumes/Fast SSD/AMZN_2015-01-01_2015-06-30_30/AMZN_2015-01-02_34200000_57600000_message_30.csv",header = F)

options(digits=7)
AMZN_all_sell <- unique(sort(as.numeric(c(AMZN_LOB[,1],AMZN_LOB[,5],AMZN_LOB[,9],AMZN_LOB[,13],AMZN_LOB[,17])))/10000)
AMZN_all_sell[1:5]
AMZN_sell_LOB <- AMZN_Mes[AMZN_Mes$V2==2 | AMZN_Mes$V2==3,]

AMZN_sell_LOB1 <- AMZN_sell_LOB[AMZN_sell_LOB$V5/10000 == AMZN_all_sell[1] , ]
mean(diff(AMZN_sell_LOB1$V1))
AMZN_sell_LOB2 <- AMZN_sell_LOB[AMZN_sell_LOB$V5/10000 == AMZN_all_sell[2] , ]
mean(diff(AMZN_sell_LOB2$V1))
AMZN_sell_LOB3 <- AMZN_sell_LOB[AMZN_sell_LOB$V5/10000 == AMZN_all_sell[3] , ]
mean(diff(AMZN_sell_LOB3$V1))
AMZN_sell_LOB4 <- AMZN_sell_LOB[AMZN_sell_LOB$V5/10000 == AMZN_all_sell[4] , ]
mean(diff(AMZN_sell_LOB4$V1))
AMZN_sell_LOB5 <- AMZN_sell_LOB[AMZN_sell_LOB$V5/10000 == AMZN_all_sell[5] , ]
mean(diff(AMZN_sell_LOB5$V1))

AMZN_sell_LOB <- rbind(AMZN_sell_LOB1,AMZN_sell_LOB2,AMZN_sell_LOB3,AMZN_sell_LOB4,AMZN_sell_LOB5)
AMZN_sell_LOB$V7 <- as.numeric(rownames(AMZN_sell_LOB))
AMZN_sell_LOB <- AMZN_sell_LOB[order(AMZN_sell_LOB$V7),]
AMZN_sell_LOB <- AMZN_sell_LOB[,1:6]
mean(diff(AMZN_sell_LOB1$V1))





