setwd("/Volumes/Fast SSD/AMZN message")
getwd()
csvpath = "/Volumes/Fast SSD/AMZN message"
csvfiles = list.files( path = csvpath, pattern="*.csv")
for (i in 1:2){
  read.csv(csvfiles[i])
}


folder <- "/Volumes/Fast SSD/AMZN message"
file_list <- list.files(path=folder, pattern="*.csv") 
for (i in 1:2){
  assign(file_list[i], read.csv(paste(folder, file_list[i], sep='/'),nrow=500,header = FALSE))
}

index <- c("AMNZ","BRK.B","FB","GOOG","JNJ","JPM","MSFT","V","WMT","XOM")

for (j in index){
  k <- list.files(paste0("/Volumes/Fast SSD/",i,"_2015-01-01_2015-06-30_30"))[j]
  assign(i,read.csv(paste0("/Volumes/Fast SSD/",i,"_2015-01-01_2015-06-30_30",k),header = F,nrows = 100))
  m <- eval(parse(text = paste0(i,"[",i,"$V2==4,]")))
  t <- 2
  while(nrow(m) == 0){
    assign(i,read.csv(paste0("/Volumes/Fast SSD/",i,"_2015-01-01_2015-06-30_30",k),header = F,nrows = 100*t))
    m <- eval(parse(text = paste0(i,"[",i,"$V2==4,]")))
    t <- t + 1
  }
}


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

v1 <- 100
H <- matrix(nrow=124,ncol=11)
#for (i in 1:124){
#  H[i,] <- c(0,(0.1*v1)/AMZN.P[[i]],(0.1*v1)/BRK.B.P[[i]],(0.1*v1)/FB.P[[i]]
#       ,(0.1*v1)/GOOG.P[[i]],(0.1*v1)/JNJ.P[[i]],(0.1*v1)/JPM.P[[i]]
#       ,(0.1*v1)/MSFT.P[[i]],(0.1*v1)/V.P[[i]],(0.1*v1)/WMT.P[[i]],(0.1*v1)/XOM.P[[i]])
#}

#w <- c(0,H[2]*AMZN[[1]]/v1,H[3]*BRK.B.P[[1]]/v1,H[4]*FB.P[[1]]/v1,H[5]*GOOG.P[[1]]/v1
#      ,H[6]*JNJ.P[[1]]/v1,H[7]*JPM.P[[1]]/v1,H[8]*MSFT.P[[1]]/v1,H[9]*V.P[[1]]/v1
#       ,H[10]*WMT.P[[1]]/v1,H[11]*XOM.P[[1]]/v1)


#####not change
r <- 0.04/250
v <- c(100)
return <- c()
w<- 1/10
v1[i+1] <- (1+r)* (1-w1[i+1])*v1[i]+ ((v1[i]*w1[i+1]/testdata[i,2]))*(testdata[i,5])

#for (i in 1:124){
#  H[i,] <- c(0,(0.1*v1)/AMZN.P[[i]],(0.1*v1)/BRK.B.P[[i]],(0.1*v1)/FB.P[[i]]
#             ,(0.1*v1)/GOOG.P[[i]],(0.1*v1)/JNJ.P[[i]],(0.1*v1)/JPM.P[[i]]
#             ,(0.1*v1)/MSFT.P[[i]],(0.1*v1)/V.P[[i]],(0.1*v1)/WMT.P[[i]],(0.1*v1)/XOM.P[[i]])
#}
for (i in 1:124){
  v[i+1] <- ((v[i]*w)/AMZN.P[[i]])+((v[i]*w)/BRK.B.P[[i]])+((v[i]*w)/FB.P[[i]])+((v[i]*w)/GOOG.P[[i]])+((v[i]*w)/JNJ.P[[i]])+((v[i]*w)/JPM.P[[i]])+((v[i]*w)/MSFT.P[[i]])+((v[i]*w)/V.P[[i]])+((v[i]*w)/WMT.P[[i]]+((v[i]*w)/XOM.P[[i]]))
}

AMZN.logreturn <- c()
AMZN.P <- as.numeric(AMZN.P)
for (i in 1:123){
  AMZN.logreturn[i] <- log(AMZN.P[i+1]/AMZN.P[i])
}

BRK.B.logreturn <- c()
BRK.B.P <- as.numeric(BRK.B.P)
for (i in 1:123){
  BRK.B.logreturn[i] <- log(BRK.B.P[i+1]/BRK.B.P[i])
}
FB.logreturn <- c()
FB.P <- as.numeric(FB.P)
for (i in 1:123){
  FB.logreturn[i] <- log(FB.P[i+1]/FB.P[i])
}
GOOG.logreturn <- c()
GOOG.P <- as.numeric(GOOG.P)
for (i in 1:123){
  GOOG.logreturn[i] <- log(GOOG.P[i+1]/GOOG.P[i])
}
JNJ.logreturn <- c()
JNJ.P <- as.numeric(JNJ.P)
for (i in 1:123){
  JNJ.logreturn[i] <- log(JNJ.P[i+1]/JNJ.P[i])
}
JPM.logreturn <- c()
JPM.P <- as.numeric(JPM.P)
for (i in 1:123){
  JPM.logreturn[i] <- log(JPM.P[i+1]/JPM.P[i])
}
MSFT.logreturn <- c()
MSFT.P <- as.numeric(MSFT.P)
for (i in 1:123){
  MSFT.logreturn[i] <- log(MSFT.P[i+1]/MSFT.P[i])
}
V.logreturn <- c()
V.P <- as.numeric(V.P)
for (i in 1:123){
  V.logreturn[i] <- log(V.P[i+1]/V.P[i])
}

WMT.logreturn <- c()
WMT.P <- as.numeric(WMT.P)
for (i in 1:123){
  WMT.logreturn[i] <- log(WMT.P[i+1]/WMT.P[i])
}

XOM.logreturn <- c()
XOM.P <- as.numeric(XOM.P)
for (i in 1:123){
  XOM.logreturn[i] <- log(XOM.P[i+1]/XOM.P[i])
}

library(data.table)


#####month change
r <- 0.04/250
v <- c(100)
w1 <- 0.1
AMZN.portfolioreturn <- c()
AMZN.V <- c(10)

index <- 1
x=1
y=20

for (i in seq(1,124,20)){
  AMZN.portfolioreturn[i+19] <- w1*AMZN.logreturn[i]
  AMZN.V[i+20] = ((w1*AMZN.V[i])/AMZN.P[i])*AMZN.P[i+1] + AMZN.V[1]
  }
f.monthly <- data.frame(AMZN.V[-1],AMZN.portfolioreturn)


BRK.B.portfolioreturn <- c()
BRK.B.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
    BRK.B.portfolioreturn[i+19] <- w1*BRK.B.logreturn[i]
    BRK.B.V[i+20] = ((w1*BRK.B.V[i])/BRK.B.P[i])*BRK.B.P[i+1] + BRK.B.V[1]
  }
f.monthly <- cbind(f.monthly,BRK.B.V[-1],BRK.B.portfolioreturn)


FB.portfolioreturn <- c()
FB.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  FB.portfolioreturn[i+19] <- w1*FB.logreturn[i]
  FB.V[i+20] = ((w1*FB.V[i])/FB.P[i])*FB.P[i+1] + FB.V[1]
}
f.monthly <- cbind(f.monthly,FB.V[-1],FB.portfolioreturn)

GOOG.portfolioreturn <- c()
GOOG.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  GOOG.portfolioreturn[i+19] <- w1*GOOG.logreturn[i]
  GOOG.V[i+20] = ((w1*GOOG.V[i])/GOOG.P[i])*GOOG.P[i+1] + GOOG.V[1]
}
f.monthly <- cbind(f.monthly,GOOG.V[-1],GOOG.portfolioreturn)

JNJ.portfolioreturn <- c()
JNJ.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  JNJ.portfolioreturn[i+19] <- w1*JNJ.logreturn[i]
  JNJ.V[i+20] = ((w1*JNJ.V[i])/JNJ.P[i])*JNJ.P[i+1] + JNJ.V[1]
}
f.monthly <- cbind(f.monthly,JNJ.V[-1],JNJ.portfolioreturn)


JPM.portfolioreturn <- c()
JPM.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  JPM.portfolioreturn[i+19] <- w1*JPM.logreturn[i]
  JPM.V[i+20] = ((w1*JPM.V[i])/JPM.P[i])*JPM.P[i+1] + JPM.V[1]
}
f.monthly <- cbind(f.monthly,JPM.V[-1],JPM.portfolioreturn)


MSFT.portfolioreturn <- c()
MSFT.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  MSFT.portfolioreturn[i+19] <- w1*MSFT.logreturn[i]
  MSFT.V[i+20] = ((w1*MSFT.V[i])/MSFT.P[i])*MSFT.P[i+1] + MSFT.V[1]
}
f.monthly <- cbind(f.monthly,MSFT.V[-1],MSFT.portfolioreturn)

V.portfolioreturn <- c()
V.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  V.portfolioreturn[i+19] <- w1*V.logreturn[i]
  V.V[i+20] = ((w1*V.V[i])/V.P[i])*V.P[i+1] + V.V[1]
}
f.monthly <- cbind(f.monthly,V.V[-1],V.portfolioreturn)


WMT.portfolioreturn <- c()
WMT.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  WMT.portfolioreturn[i+19] <- w1*WMT.logreturn[i]
  WMT.V[i+20] = ((w1*WMT.V[i])/WMT.P[i])*WMT.P[i+1] + WMT.V[1]
}
f.monthly <- cbind(f.monthly,WMT.V[-1],WMT.portfolioreturn)

XOM.portfolioreturn <- c()
XOM.V <- c(10)
index <- 1
for (i in seq(1,124,20)){
  XOM.portfolioreturn[i+19] <- w1*XOM.logreturn[i]
  XOM.V[i+20] = ((w1*XOM.V[i])/XOM.P[i])*XOM.P[i+1] + XOM.V[1]
}
f.monthly <- cbind(f.monthly,XOM.V[-1],XOM.portfolioreturn)
f.monthly <- na.omit(f.monthly)

all.PR.monthly <- c()
for (i in 1:7){
  all.PR.monthly[i] <- f.monthly[2][i,]+f.monthly[4][i,]+f.monthly[6][i,]+f.monthly[8][i,]+f.monthly[10][i,]+f.monthly[12][i,]+f.monthly[14][i,]+f.monthly[16][i,]+f.monthly[18][i,]+f.monthly[20][i,]
}
all.V.monthly <- c()
for (i in 1:7){
  all.V.monthly[i] <- f.monthly[1][i,]+f.monthly[3][i,]+f.monthly[5][i,]+f.monthly[7][i,]+f.monthly[9][i,]+f.monthly[11][i,]+f.monthly[13][i,]+f.monthly[15][i,]+f.monthly[17][i,]+f.monthly[19][i,]
}
all.logreturn.monthly <- c()
for (i in 1:7){
  all.logreturn.monthly[i] <- log(all.V.monthly[i+1]/all.V.monthly[i])
}

##########################################

#####weekly change

r <- 0.04/250
v <- c(100)
w1 <- 0.1

AMZN.portfolioreturn <- c()
AMZN.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  AMZN.V[i+5] = ((w1*AMZN.V[i])/AMZN.P[i])*AMZN.P[i+1] + AMZN.V[1]
  AMZN.portfolioreturn[i+4] <-  (AMZN.V[i+5]-AMZN.V[i])/AMZN.V[i]
}
f.weekly <- data.frame(AMZN.V[-1],AMZN.portfolioreturn)


BRK.B.portfolioreturn <- c()
BRK.B.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
    BRK.B.V[i+5] = ((w1*BRK.B.V[i])/BRK.B.P[i])*BRK.B.P[i+1] + BRK.B.V[1]
    BRK.B.portfolioreturn[i+4] <-  (BRK.B.V[i+5]-BRK.B.V[i])/BRK.B.V[i]
  }
f.weekly <- cbind(f.weekly,BRK.B.V[-1],BRK.B.portfolioreturn)


FB.portfolioreturn <- c()
FB.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  FB.V[i+5] = ((w1*FB.V[i])/FB.P[i])*FB.P[i+1] + FB.V[1]
  FB.portfolioreturn[i+4] <-  (FB.V[i+5]-FB.V[i])/FB.V[i]
}
f.weekly <- cbind(f.weekly,FB.V[-1],FB.portfolioreturn)

GOOG.portfolioreturn <- c()
GOOG.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  GOOG.portfolioreturn[i+4] <- w1*GOOG.logreturn[i]
  GOOG.V[i+5] = ((w1*GOOG.V[i])/GOOG.P[i])*GOOG.P[i+1] + GOOG.V[1]
}
f.weekly <- cbind(f.weekly,GOOG.V[-1],GOOG.portfolioreturn)

JNJ.portfolioreturn <- c()
JNJ.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  JNJ.portfolioreturn[i+4] <- w1*JNJ.logreturn[i]
  JNJ.V[i+5] = ((w1*JNJ.V[i])/JNJ.P[i])*JNJ.P[i+1] + JNJ.V[1]
}
f.weekly <- cbind(f.weekly,JNJ.V[-1],JNJ.portfolioreturn)

JPM.portfolioreturn <- c()
JPM.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  JPM.portfolioreturn[i+4] <- w1*JPM.logreturn[i]
  JPM.V[i+5] = ((w1*JPM.V[i])/JPM.P[i])*JPM.P[i+1] + JPM.V[1]
}
f.weekly <- cbind(f.weekly,JPM.V[-1],JPM.portfolioreturn)

MSFT.portfolioreturn <- c()
MSFT.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  MSFT.portfolioreturn[i+4] <- w1*MSFT.logreturn[i]
  MSFT.V[i+5] = ((w1*MSFT.V[i])/MSFT.P[i])*MSFT.P[i+1] + MSFT.V[1]
}
f.weekly <- cbind(f.weekly,MSFT.V[-1],MSFT.portfolioreturn)

V.portfolioreturn <- c()
V.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  V.portfolioreturn[i+4] <- w1*V.logreturn[i]
  V.V[i+5] = ((w1*V.V[i])/V.P[i])*V.P[i+1] + V.V[1]
}
f.weekly <- cbind(f.weekly,V.V[-1],V.portfolioreturn)

WMT.portfolioreturn <- c()
WMT.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  WMT.portfolioreturn[i+4] <- w1*WMT.logreturn[i]
  WMT.V[i+5] = ((w1*WMT.V[i])/WMT.P[i])*WMT.P[i+1] + WMT.V[1]
}
f.weekly <- cbind(f.weekly,WMT.V[-1],WMT.portfolioreturn)

XOM.portfolioreturn <- c()
XOM.V <- c(10)
index <- 1
for (i in seq(1,124,5)){
  XOM.portfolioreturn[i+4] <- w1*XOM.logreturn[i]
  XOM.V[i+5] = ((w1*XOM.V[i])/XOM.P[i])*XOM.P[i+1] + XOM.V[1]
}
f.weekly <- cbind(f.weekly,XOM.V[-1],XOM.portfolioreturn)
f.weekly <- na.omit(f.weekly)

all.PR.weekly <- c()
for (i in 1:25){
  all.PR.weekly[i] <- f.weekly[2][i,]+f.weekly[4][i,]+f.weekly[6][i,]+f.weekly[8][i,]+f.weekly[10][i,]+f.weekly[12][i,]+f.weekly[14][i,]+f.weekly[16][i,]+f.weekly[18][i,]+f.weekly[20][i,]
}

all.V.weekly <- c()
for (i in 1:25){
  all.V.weekly[i] <- f.weekly[1][i,]+f.weekly[3][i,]+f.weekly[5][i,]+f.weekly[7][i,]+f.weekly[9][i,]+f.weekly[11][i,]+f.weekly[13][i,]+f.weekly[15][i,]+f.weekly[17][i,]+f.weekly[19][i,]
}
all.logreturn.weekly <- c()
for (i in 1:25){
  all.logreturn.weekly[i] <- log(all.V.weekly[i+1]/all.V.weekly[i])
}


###############################
####### daily change
r <- 0.04/250
v <- c(100)
w1 <- 0.1

AMZN.portfolioreturn <- c()
AMZN.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
    AMZN.portfolioreturn[i] <- w1*AMZN.logreturn[i]
    AMZN.V[i+1] = ((w1*AMZN.V[i])/AMZN.P[i])*AMZN.P[i+1] + AMZN.V[1]
  }
f.daily <- data.frame(AMZN.V[-1],AMZN.portfolioreturn)


BRK.B.portfolioreturn <- c()
BRK.B.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
    BRK.B.portfolioreturn[i] <- w1*BRK.B.logreturn[i]
    BRK.B.V[i+1] = ((w1*BRK.B.V[i])/BRK.B.P[i])*BRK.B.P[i+1] + BRK.B.V[1]
  }
f.daily <- cbind(f.daily,BRK.B.V[-1],BRK.B.portfolioreturn)


FB.portfolioreturn <- c()
FB.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  FB.portfolioreturn[i] <- w1*FB.logreturn[i]
  FB.V[i+1] = ((w1*FB.V[i])/FB.P[i])*FB.P[i+1] + FB.V[1]
}
f.daily <- cbind(f.daily,FB.V[-1],FB.portfolioreturn)


GOOG.portfolioreturn <- c()
GOOG.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  GOOG.portfolioreturn[i] <- w1*GOOG.logreturn[i]
  GOOG.V[i+1] = ((w1*GOOG.V[i])/GOOG.P[i])*GOOG.P[i+1] + GOOG.V[1]
}
f.daily <- cbind(f.daily,GOOG.V[-1],GOOG.portfolioreturn)

JNJ.portfolioreturn <- c()
JNJ.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  JNJ.portfolioreturn[i] <- w1*JNJ.logreturn[i]
  JNJ.V[i+1] = ((w1*JNJ.V[i])/JNJ.P[i])*JNJ.P[i+1] + JNJ.V[1]
}
f.daily <- cbind(f.daily,JNJ.V[-1],JNJ.portfolioreturn)

JPM.portfolioreturn <- c()
JPM.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  JPM.portfolioreturn[i] <- w1*JPM.logreturn[i]
  JPM.V[i+1] = ((w1*JPM.V[i])/JPM.P[i])*JPM.P[i+1] + JPM.V[1]
}
f.daily <- cbind(f.daily,JPM.V[-1],JPM.portfolioreturn)

MSFT.portfolioreturn <- c()
MSFT.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  MSFT.portfolioreturn[i] <- w1*MSFT.logreturn[i]
  MSFT.V[i+1] = ((w1*MSFT.V[i])/MSFT.P[i])*MSFT.P[i+1] + MSFT.V[1]
}
f.daily <- cbind(f.daily,MSFT.V[-1],MSFT.portfolioreturn)

V.portfolioreturn <- c()
V.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  V.portfolioreturn[i] <- w1*V.logreturn[i]
  V.V[i+1] = ((w1*V.V[i])/V.P[i])*V.P[i+1] + V.V[1]
}
f.daily <- cbind(f.daily,V.V[-1],V.portfolioreturn)

WMT.portfolioreturn <- c()
WMT.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  WMT.portfolioreturn[i] <- w1*WMT.logreturn[i]
  WMT.V[i+1] = ((w1*WMT.V[i])/WMT.P[i])*WMT.P[i+1] + WMT.V[1]
}
f.daily <- cbind(f.daily,WMT.V[-1],WMT.portfolioreturn)


XOM.portfolioreturn <- c()
XOM.V <- c(10)
index <- 1
x=1
y=124
for (i in x:y){
  XOM.portfolioreturn[i] <- w1*XOM.logreturn[i]
  XOM.V[i+1] = ((w1*XOM.V[i])/XOM.P[i])*XOM.P[i+1] + XOM.V[1]
}
f.daily <- cbind(f.daily,XOM.V[-1],XOM.portfolioreturn)

all.PR.daily <- c()
for (i in 1:123){
  all.PR.daily[i] <- f.daily[2][i,]+f.daily[4][i,]+f.daily[6][i,]+f.daily[8][i,]+f.daily[10][i,]+f.daily[12][i,]+f.daily[14][i,]+f.daily[16][i,]+f.daily[18][i,]+f.daily[20][i,]
}

all.V.daily <- c()
for (i in 1:123){
  all.V.daily[i] <- f.daily[1][i,]+f.daily[3][i,]+f.daily[5][i,]+f.daily[7][i,]+f.daily[9][i,]+f.daily[11][i,]+f.daily[13][i,]+f.daily[15][i,]+f.daily[17][i,]+f.daily[19][i,]
}

all.logreturn.daily <- c()
for (i in 1:123){
  all.logreturn.daily[i] <- log(all.V.daily[i+1]/all.V.daily[i])
}


par(mfrow=c(2,2))
plot.ts(all.PR.daily,main="PR(t) for resetting portfolio daily")
abline(h=0)
plot.ts(all.PR.weekly,main="PR(t) for resetting portfolio weekly")
abline(h=0)
plot.ts(all.PR.monthly,main="PR(t) for resetting portfolio monthly")
abline(h=0)

