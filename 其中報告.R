library(quantmod)
library(highfrequency)
SMA(AMZN$V5/10000)
plot(AMZN$V5/10000,type="l")
addTA(SMA(AMZN$V5/10000), on=1, col="blue")

asset.names = c("AMZN", "BRK.B", "FB","GOOG","JNJ","JPM","MSFT","V","WMT","XOM")



##########

# Suppose irregular timepoints:
start <- as.POSIXct("2010-01-01 09:30:00") 

ta <- start + AMZN$V1-34200
# Yielding the following timeseries:
a <- xts::as.xts(AMZN$V5/10000,1:length(ta), order.by = ta) 
colnames(a) <- "PRICE"
AMZN_pre <- aggregatets(a$PRICE,on = "secs",k = 5 )

tb <- start + FB$V1-34200
b <- xts::as.xts(FB$V5/10000,1:length(tb), order.by = tb)
colnames(b) <- "PRICE"
FB_pre <- aggregatets(b$PRICE,on = "secs",k = 5 )


tc <- start + GOOG$V1-34200
c <- xts::as.xts(GOOG$V5/10000,1:length(tc), order.by = tc)
colnames(c) <- "PRICE"
GOOG_pre <- aggregatets(c$PRICE,on = "secs",k = 5 )


pre <- cbind(AMZN_pre,FB_pre,GOOG_pre)

cov <- rKernelCov(diff(log(pre)),align.by = "seconds", align.period = 5, makeReturns = FALSE, kernel.type = "rectangular", kernel.param = 1, kernel.dofadj = TRUE)
logret <- as.data.frame(diff(log(pre)))
rownames(logret) <- c(1:length(logret$PRICE))
mu.1=colSums(na.omit(logret))
marko_W <- efficient.portfolio(mu.1,cov,0.003)$weights

V <- 100
AMZN_prof <-  sum(marko_W[1]*diff(log(AMZN_B))) - sum(marko_W[1]*diff(log(AMZN_S)))
FB_prof <-  sum(marko_W[1]*diff(log(FB_B))) - sum(marko_W[1]*diff(log(FB_S))) 
GOOG_prof <-  sum(marko_W[1]*diff(log(GOOG_B))) - sum(marko_W[1]*diff(log(GOOG_S))) 
(V*sum(AMZN_prof,FB_prof,GOOG_prof))+V

########
  

colnames(pre) <- c("AMZN_pre","FB_pre","GOOG_pre")
chartSeries(a$PRICE)
addTA(SMA(a$PRICE), on=1, col="blue")
AMZN_ma10mins <- runMean(a$PRIC,n=120)
AMZN_ma20mins <- runMean(a$PRIC,n=240)
addTA(AMZN_ma10mins, on=1, col="blue")
addTA(AMZN_ma20mins, on=1, col="red")
points(which(AMZN_UpBuy==1))

AMZN_smaLS <- cbind(AMZN_ma10mins,lag(AMZN_ma10mins),AMZN_ma20mins,lag(AMZN_ma20mins))
AMZN_smaLs<-na.omit(AMZN_smaLS)

colnames(AMZN_smaLs)<-c("AMZN_ma10mins","AMZN_lagma10mins","AMZN_ma20mins","AMZN_lagma20mins")
head(AMZN_smaLs)

Upcross<-function(x){  ifelse(x[2]<x[4]&x[1]>x[3],1,0) }
Downcross<-function(x){  ifelse(x[2]>x[4]&x[1]<x[3],-1,0) }

AMZN_Upsig<-apply(AMZN_smaLs,1,Upcross)
AMZN_Upsig<-xts(AMZN_Upsig,order.by = index(AMZN_smaLs)) 
AMZN_UpBuy<-lag(AMZN_Upsig,36)
AMZN_UpBuy[AMZN_UpBuy==1]
AMZN_Upsig[AMZN_Upsig==1]
AMZN_B <- as.numeric(pre$AMZN_pre[which(AMZN_Upsig==1)])

AMZN_Downsig<-apply(AMZN_smaLS,1,Downcross)
AMZN_Downsig<-xts(AMZN_Downsig,order.by = index(AMZN_smaLS))
AMZN_DownSell<-lag(AMZN_Downsig,36)
AMZN_DownSell[AMZN_DownSell==-1]
AMZN_S <- as.numeric(pre$AMZN_pre[which(AMZN_DownSell==-1)])

#####


#chartSeries(pre$FB_pre)
#addTA(SMA(pre$FB_pre), on=1, col="blue")
FB_ma10mins <- runMean(pre$FB_pre,n=120)
FB_ma20mins <- runMean(pre$FB_pre,n=240)
#addTA(ma10mins, on=1, col="blue")
#addTA(ma20mins, on=1, col="red")


FB_smaLS <- cbind(FB_ma10mins,lag(FB_ma10mins),FB_ma20mins,lag(FB_ma20mins))
FB_smaLs<-na.omit(FB_smaLS)

colnames(FB_smaLs)<-c("FB_ma10mins","FB_lagma10mins","FB_ma20mins","FB_lagma20mins")
head(FB_smaLs)

Upcross<-function(x){  ifelse(x[2]<x[4]&x[1]>x[3],1,0) }
Downcross<-function(x){  ifelse(x[2]>x[4]&x[1]<x[3],-1,0) }

FB_Upsig<-apply(FB_smaLs,1,Upcross)
FB_Upsig<-xts(FB_Upsig,order.by = index(FB_smaLs)) 
FB_UpBuy<-lag(FB_Upsig,36)
FB_UpBuy[FB_UpBuy==1]
FB_Upsig[FB_Upsig==1]
FB_B <- as.numeric(pre$FB_pre[which(FB_Upsig==1)])

FB_Downsig<-apply(FB_smaLS,1,Downcross)
FB_Downsig<-xts(FB_Downsig,order.by = index(FB_smaLS))
FB_DownSell<-lag(FB_Downsig,36)
FB_DownSell[FB_DownSell==-1]
FB_S <- as.numeric(pre$FB_pre[which(FB_DownSell==-1)])

####
GOOG_ma10mins <- runMean(pre$GOOG_pre,n=120)
GOOG_ma20mins <- runMean(pre$GOOG_pre,n=240)
#addTA(ma10mins, on=1, col="blue")
#addTA(ma20mins, on=1, col="red")


GOOG_smaLS <- cbind(GOOG_ma10mins,lag(GOOG_ma10mins),GOOG_ma20mins,lag(GOOG_ma20mins))
GOOG_smaLs<-na.omit(GOOG_smaLS)

colnames(GOOG_smaLs)<-c("GOOG_ma10mins","GOOG_lagma10mins","GOOG_ma20mins","GOOG_lagma20mins")
head(GOOG_smaLs)

Upcross<-function(x){  ifelse(x[2]<x[4]&x[1]>x[3],1,0) }
Downcross<-function(x){  ifelse(x[2]>x[4]&x[1]<x[3],-1,0) }

GOOG_Upsig<-apply(GOOG_smaLs,1,Upcross)
GOOG_Upsig<-xts(GOOG_Upsig,order.by = index(GOOG_smaLs)) 
GOOG_UpBuy<-lag(GOOG_Upsig,36)
GOOG_UpBuy[GOOG_UpBuy==1]
GOOG_Upsig[GOOG_Upsig==1]
GOOG_B <- as.numeric(pre$GOOG_pre[which(GOOG_Upsig==1)])

GOOG_Downsig<-apply(GOOG_smaLS,1,Downcross)
GOOG_Downsig<-xts(GOOG_Downsig,order.by = index(GOOG_smaLS))
GOOG_DownSell<-lag(GOOG_Downsig,36)
GOOG_DownSell[GOOG_DownSell==-1]
GOOG_S <- as.numeric(pre$GOOG_pre[which(GOOG_DownSell==-1)])
###
profit=sum(S-B) 
ARR=mean((S-B)/B)

MRC(pre, pairwise = F, makePsd = FALSE)

