AMZN_path <- "/Volumes/Fast SSD/AMZN_message" 
files <- list.files(path=AMZN_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
AMZN = lapply(paste(AMZN_path,files,sep="/"), a)
for (i in 1:124){
  AMZN[[i]] <- (AMZN[[i]][AMZN[[i]]$V2==4,][1,]$V5)/10000
}
AMZN.P <- as.list(AMZN)
plot.ts(AMZN.P,col=4)

BRK.B_path <- "/Volumes/Fast SSD/BRK.B_message" 
files <- list.files(path=BRK.B_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
BRK.B = lapply(paste(BRK.B_path,files,sep="/"), a)
for (i in 1:124){
  BRK.B[[i]] <- (BRK.B[[i]][BRK.B[[i]]$V2==4,][1,]$V5)/10000
}
BRK.B.P <- as.list(BRK.B)
plot.ts(BRK.B.P,col=4)

FB_path <- "/Volumes/Fast SSD/FB_message" 
files <- list.files(path=FB_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
FB = lapply(paste(FB_path,files,sep="/"), a)
for (i in 1:124){
  FB[[i]] <- (FB[[i]][FB[[i]]$V2==4,][1,]$V5)/10000
}
FB.P <- as.list(FB)
plot.ts(FB.P,col=4)

GOOG_path <- "/Volumes/Fast SSD/GOOG_message" 
files <- list.files(path=GOOG_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
GOOG = lapply(paste(GOOG_path,files,sep="/"), a)
for (i in 1:124){
  GOOG[[i]] <- (GOOG[[i]][GOOG[[i]]$V2==4,][1,]$V5)/10000
}
GOOG.P <- as.list(GOOG)
plot.ts(GOOG.P,col=4)

JNJ_path <- "/Volumes/Fast SSD/JNJ_message" 
files <- list.files(path=JNJ_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
JNJ = lapply(paste(JNJ_path,files,sep="/"), a)
for (i in 1:124){
  JNJ[[i]] <- (JNJ[[i]][JNJ[[i]]$V2==4,][1,]$V5)/10000
}
JNJ.P <- as.list(JNJ)
plot.ts(JNJ.P,col=4)

JPM_path <- "/Volumes/Fast SSD/JPM_message" 
files <- list.files(path=JPM_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
JPM = lapply(paste(JPM_path,files,sep="/"), a)
for (i in 1:124){
  JPM[[i]] <- (JPM[[i]][JPM[[i]]$V2==4,][1,]$V5)/10000
}
JPM.P <- as.list(JPM)
plot.ts(JPM.P,col=4)

MSFT_path <- "/Volumes/Fast SSD/MSFT_message" 
files <- list.files(path=MSFT_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
MSFT = lapply(paste(MSFT_path,files,sep="/"), a)
for (i in 1:124){
  MSFT[[i]] <- (MSFT[[i]][MSFT[[i]]$V2==4,][1,]$V5)/10000
}
MSFT.P <- as.list(MSFT)
plot.ts(MSFT.P,col=4)

V_path <- "/Volumes/Fast SSD/V_message" 
files <- list.files(path=V_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
V = lapply(paste(V_path,files,sep="/"), a)
for (i in 1:124){
  V[[i]] <- (V[[i]][V[[i]]$V2==4,][1,]$V5)/10000
}
V.P <- as.list(V)
plot.ts(V.P,col=4)

WMT_path <- "/Volumes/Fast SSD/WMT_message" 
files <- list.files(path=WMT_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
WMT = lapply(paste(WMT_path,files,sep="/"), a)
for (i in 1:124){
  WMT[[i]] <- (WMT[[i]][WMT[[i]]$V2==4,][1,]$V5)/10000
}
WMT.P <- as.list(WMT)
plot.ts(WMT.P,col=4)

XOM_path <- "/Volumes/Fast SSD/XOM_message" 
files <- list.files(path=XOM_path, pattern="*.csv")
a<-function(x) read.csv(x, stringsAsFactors = FALSE,nrow=1000,header = F)
XOM = lapply(paste(XOM_path,files,sep="/"), a)
for (i in 1:124){
  XOM[[i]] <- (XOM[[i]][XOM[[i]]$V2==4,][1,]$V5)/10000
}
XOM.P <- as.list(XOM)
plot.ts(XOM.P,col=4)


library("IntroCompFinR")
library(stockPortfolio) # Base package for retrieving returns
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP

asset.names = c("AMZN", "BRK.B", "FB","GOOG","JNJ","JPM","MSFT","V","WMT","XOM")
er = rep(0.0015,10)
names(er) = asset.names
data <- c(AMZN.P,BRK.B.P,FB.P,GOOG.P,JNJ.P,JPM.P,MSFT.P,V.P,WMT.P,XOM.P)

price.matrix <- matrix(ncol = 10,nrow =102)
price.matrix[,1] <- as.numeric(AMZN.P)[1:102]
price.matrix[,2] <- as.numeric(BRK.B.P)[1:102]
price.matrix[,3] <- as.numeric(FB.P)[1:102]
price.matrix[,4] <- as.numeric(GOOG.P)[1:102]
price.matrix[,5] <- as.numeric(JNJ.P)[1:102]
price.matrix[,6] <- as.numeric(JPM.P)[1:102]
price.matrix[,7] <- as.numeric(MSFT.P)[1:102]
price.matrix[,8] <- as.numeric(V.P)[1:102]
price.matrix[,9] <- as.numeric(WMT.P)[1:102]
price.matrix[,10] <- as.numeric(XOM.P)[1:102]
covmat <- cov(price.matrix)
dimnames(covmat) <- list(asset.names, asset.names)
r.free <- 0.005/2
ew = rep(1,10)/10
equalWeight.portfolio <- getPortfolio(er=er,cov.mat=covmat,weights=ew)
class(equalWeight.portfolio)
equalWeight.portfolio
plot(equalWeight.portfolio)
######


library(portfolio.optimization)

er <- mu/40
covmat <- sigma2
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, covmat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue")

#
# compute global minimum variance portfolio with no short sales
gmin.port.ns = globalMin.portfolio(er, covmat, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=r.free)
plot(gmin.port.ns, col="blue")


#
# compute efficient portfolio subject to target return
target.return = er["MSFT"]
e.port.msft = efficient.portfolio(er, covmat, target.return)
e.port.msft
summary(e.port.msft, risk.free=r.free)
plot(e.port.msft, col="blue")

#
# compute efficient portfolio subject to target return with no short sales
target.return = er["MSFT"]
e.port.msft.ns = efficient.portfolio(er, covmat, target.return, shorts=FALSE)
e.port.msft.ns
summary(e.port.msft.ns, risk.free=r.free)
plot(e.port.msft.ns, col="blue")

#
# compute tangency portfolio
tan.port <- tangency.portfolio(er, covmat,r.free)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")

#
# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

#
# compute portfolio frontier
colnames(mu) <- c("AMZN","FB","GOOG","BRK.B","JNJ", "JPM","MSFT")
ef <- efficient.frontier(mu/40, sigma2, alpha.min=-2, 
                         alpha.max=2, nport=20)
attributes(ef)
ef

plot(ef)
plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)    
sr.tan = (tan.port$er - r.free)/tan.port$sd
abline(a=r.free, b=sr.tan, col="green", lwd=2)


# plot portfolio frontier with tangency portfolio
sd.vals = sqrt(diag(covmat))
mu.vals = er
plot(ef$sd, ef$er, ylim=c(0, max(ef$er)), xlim=c(0, max(ef$sd)),
     xlab="portfolio sd", ylab="portfolio er", main="Efficient Portfolios")
text(sd.vals, mu.vals, labels=names(mu.vals))
abline(a=r.free, b=sr.tan)

#
# compute portfolio frontier with no short sales
ef.ns <- efficient.frontier(er, covmat, alpha.min=0, 
                            alpha.max=2, nport=20, shorts=FALSE)
attributes(ef.ns)
ef.ns
summary(ef.ns)

plot(ef.ns)
plot(ef.ns, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port.ns$sd, gmin.port.ns$er, col="green", pch=16, cex=2)
points(tan.port.ns$sd, tan.port.ns$er, col="red", pch=16, cex=2)
text(gmin.port.ns$sd, gmin.port.ns$er, labels="GLOBAL MIN", pos=2)
text(tan.port.ns$sd, tan.port.ns$er, labels="TANGENCY", pos=2)    
sr.tan.ns = (tan.port.ns$er - r.free)/tan.port.ns$sd
abline(a=r.free, b=sr.tan.ns, col="green", lwd=2)


# plot portfolio frontier with tangency portfolio
sd.vals = sqrt(diag(covmat))
mu.vals = er
plot(ef.ns$sd, ef.ns$er, ylim=c(0, max(ef.ns$er)), xlim=c(0, max(ef.ns$sd)),
     xlab="portfolio sd", ylab="portfolio er", main="Efficient Portfolios")
text(sd.vals, mu.vals, labels=names(mu.vals))
abline(a=r.free, b=sr.tan.ns)


####################

options(digits=4, scipen=100)
rm(list=ls(all=T))
Sys.setlocale('LC_ALL', 'C')
library(fPortfolio)
library(tseries)
library(quantmod)

###########################

library(zoo)
library(xts) 
library(TTR) 
library(quantmod) 
all.ret <- data.frame(AMZN.logreturn)
all.ret <- cbind(all.ret ,BRK.B.logreturn )
all.ret <- cbind(all.ret ,FB.logreturn )
all.ret <- cbind(all.ret ,GOOG.logreturn )
all.ret <- cbind(all.ret ,JNJ.logreturn )
all.ret <- cbind(all.ret ,JPM.logreturn )
all.ret <- cbind(all.ret ,MSFT.logreturn )
all.ret <- cbind(all.ret ,V.logreturn )
all.ret <- cbind(all.ret ,WMT.logreturn )
all.ret <- cbind(all.ret ,XOM.logreturn )
rownames(all.ret) <- as.character(seq(as.Date("2015-01-02"), length.out=nrow(all.ret), by = "day"))
chart.CumReturns(all.ret,legend.loc = 'topleft') 

frontier(all.ret) 
library(timeDate)  
library(timeSeries)  
library(fBasics) 
library(fAssets)  
library(fPortfolio)
Frontier = portfolioFrontier(all.ret,constraints = "short") 
Frontier 
plot(Frontier) 
