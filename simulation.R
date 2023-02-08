options(digits=22)

AMZN <- read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-01-30_34200000_57600000_message_30.csv",header = F)
FB <- read.csv("/Volumes/Fast SSD/FB_message/FB_2015-01-30_34200000_57600000_message_30.csv",header = F)

AMZN <- AMZN[AMZN$V2==4,]
FB <- FB[FB$V2==4,]

AMZN.logret <- diff(log(AMZN$V5/10000))
FB.logret <- diff(log(FB$V5/10000))

AMZN.mu <- sum(AMZN.logret)
FB.mu <- sum(FB.logret)

AMZN.sigma <- sum(AMZN.logret^2)
FB.sigma <- sum(FB.logret^2)


AMZN.time <- union(as.numeric((AMZN$V1-34200)/(57600-34200)),as.numeric((AMZN$V1-34200)/(57600-34200)))
FB.time <- union(as.numeric((FB$V1-34200)/(57600-34200)),as.numeric((FB$V1-34200)/(57600-34200)))

#Simulate intra-daily trading prices

s0 <- AMZN$V5[1]/10000
n <- length(AMZN$V5)
AMZN.time.delta <- diff(AMZN.time)
a <- rnorm(n,0,1)
P <- c(log(s0))
for (i in 1:length(AMZN.time.delta)){
  P[i+1] <- P[i]+AMZN.mu*AMZN.time.delta[i]+sqrt(AMZN.sigma)*sqrt(AMZN.time.delta[i])*a[i]
}
realP <- exp(P)

par(mfrow=c(2,1))
plot.ts(realP)
plot.ts(AMZN$V5/10000)

##Estimate co-volatility

cov <- refreshTime(AMZN,FB)

##Simulate 2-dim intra-daily trading prices

alltime <- sort(union(AMZN.time,FB.time))
index <- c(1:length(alltime))

alltimedf <- data.frame(index,alltime)

AMZN.index <- c()
for (i in 1:length(AMZN.time)){
  AMZN.index <- c(AMZN.index,alltimedf[alltime==AMZN.time[i],1])
}


FB.index <- c()
for (i in 1:length(FB.time)){
  FB.index <- c(FB.index,alltimedf[alltime==FB.time[i],1])
}


AMZN.s0 <- AMZN$V5[1]/10000
FB.s0 <- FB$V5[1]/10000
alltime.delta <- diff(alltime)
n <- length(alltime.delta)
library(mvtnorm)
milti.a <- rmvnorm(n,c(0,0),matrix(c(1,cov/sqrt(AMZN.sigma*FB.sigma),cov/sqrt(AMZN.sigma*FB.sigma),1),ncol=2))

AMZN.P <- c(log(AMZN.s0))
for (i in 1:length(milti.a)){
  AMZN.P[i+1] <- AMZN.P[i]+AMZN.mu*alltime.delta[i]+sqrt(AMZN.sigma)*sqrt(alltime.delta[i])*milti.a[i]
}

FB.P <- c(log(FB.s0))
for (i in 1:length(milti.a)){
  FB.P[i+1] <- FB.P[i]+FB.mu*alltime.delta[i]+sqrt(FB.sigma)*sqrt(alltime.delta[i])*milti.a[i]
}

AMZN.sim.P <- exp(AMZN.P[AMZN.index])
FB.sim.P <- exp(FB.P[FB.index])


par(mfrow=c(2,1))
plot(AMZN.time,AMZN.sim.P,type='l')
plot((AMZN$V1-34200)/(57600-34200),AMZN$V5/10000,type='l',xlab ='AMZN.time',ylab = "AMZN_real_P")

par(mfrow=c(2,1))
plot(FB.time,FB.sim.P,type='l')
plot((FB$V1-34200)/(57600-34200),FB$V5/10000,type='l',xlab ='FB.time',ylab = "FB_real_P")

#########################

AMZN <- read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-01-30_34200000_57600000_message_30.csv",header = F)
FB <- read.csv("/Volumes/Fast SSD/FB_message/FB_2015-01-30_34200000_57600000_message_30.csv",header = F)

path <- "/Volumes/Fast SSD/"
stock_name=c("AMZN","FB","BRK.B","MSFT","JPM","V","GOOG","WMT","XOM","JNJ")

get_log_return_daily=function(path,stock_name,time_sep){
  
  time <- seq(34200,57600,time_sep) #每5分鐘一次
  time <- time[-1]
  
  df <- matrix(NA,ncol=10,nrow=length(time))
  
  for (j in 1:10){
    
    data <- read.csv(paste0(path,stock_name[j],"_message/",stock_name[j],"_2015-01-30_34200000_57600000_message_30.csv"),header=FALSE)
    data <- data[which(data[,2]==4),]
    time_index <- rep(NA,length(time))
    
    S <- c()
    for (k in 1:length(time_index)){
      #time_index[k] <- max(data[,1][data[,1]<(time[k])])
      time_index[k] <- sum(data[,1]<time[k])
      S[k] <- (data[time_index[k],5])/10000
    }
    
    print(j)
    df[,j] <- S
    
  }
  
  colnames(df) <- stock_name
  return(df_S = df)
}

time <- seq(10,25,3)
for (j in 1:6){
  assign(paste0(time[j],"secs"),get_log_return_daily(path,stock_name,time[j]))
  print(j)
}


secs_10 <- as.data.frame(`10secs`)
secs_13 <- as.data.frame(`13secs`)
secs_16 <- as.data.frame(`16secs`)
secs_19 <- as.data.frame(`19secs`)
secs_22 <- as.data.frame(`22secs`)
secs_25 <- as.data.frame(`25secs`)


for (j in 1:6){
  for (i in 1:10){
    assign(paste0(stock_name[i],time[j],"secs_vol") , sum((diff(log(na.omit(eval(parse(text = paste0("secs_",time[j])))[,i]))))^2))
  }
}

for (j in 1:10){
  assign(paste0(stock_name[j],"_vol") , c())
  for (i in 1:6){
    assign(paste0(stock_name[j],"_vol") , c(eval(parse(text = paste0(stock_name[j],"_vol"))),eval(parse(text = paste0(stock_name[j],time[i],"secs_vol")))))
  }
}


x.freq <- c(10 ,13 ,16 ,19, 22, 25)
par(mfrow=c(2,2))
plot(x.freq,AMZN_vol,type="l",main = "RV of AMZN as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,BRK.B_vol,type="l",main = "RV of BRK.B as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,FB_vol,type="l",main = "RV of FB as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,GOOG_vol,type="l",main = "RV of GOOG as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,JPM_vol,type="l",main = "RV of JPM as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,MSFT_vol,type="l",main = "RV of MSFT as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,JNJ_vol,type="l",main = "RV of JNJ as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,V_vol,type="l",main = "RV of V as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,WMT_vol,type="l",main = "RV of WMT as a function of sampling time interval",xlab="sampling frequency (secs)")
plot(x.freq,XOM_vol,type="l",main = "RV of XOM as a function of sampling time interval",xlab="sampling frequency (secs)")

######
get_acf=function(path,stock_name){
  stock_acf <- c()
  for (j in 1:10){
    
    assign(paste0("all_",stock_name[j]) , read.csv(paste0(path,stock_name[j],"_message/",stock_name[j],"_2015-01-30_34200000_57600000_message_30.csv"),header=FALSE))
    assign(paste0("all_",stock_name[j]) , (eval(parse(text=paste0("all_",stock_name[j]))))[(eval(parse(text=paste0("all_",stock_name[j]))))$V2==4,])
    
    d1 <- eval(parse(text=paste0("all_",stock_name[j])))
    d1$V5 <- (d1$V5)/10000
    logret <- diff(log(d1$V5))
    
    gamma0 <- sum((logret-mean(logret))^2)
    gamma1 <- c()
    
    for (i in 2:length(logret)){
      gamma1[i-1] <- (logret[i]-mean(logret))*(logret[i-1]-mean(logret))
    }
    
    gamma1 <- sum(gamma1)
    stock_acf[j] <- (gamma1 / gamma0)
  }
  return(stock_acf)
}
stock_acf <- get_acf(path,stock_name)
stock_acf <- as.data.frame(stock_acf)
rownames(stock_acf) <- stock_name
#####

s0 <- AMZN$V5[1]/10000
n <- length(AMZN$V5)
AMZN.time.delta <- diff(AMZN.time)
a <- rnorm(n,0,1)
P <- c(log(s0))
for (i in 1:length(AMZN.time.delta)){
  P[i+1] <- P[i]+AMZN.mu*AMZN.time.delta[i]+sqrt(AMZN.sigma)*sqrt(AMZN.time.delta[i])*a[i]
}
P_sim_AMZN <- exp(P)
plot.ts(x = AMZN.time , P_sim_AMZN,type="l")

eta <- 10^(-7)
ec <- rnorm(length(P),0,sqrt(eta))
lines(x = AMZN.time , exp(log(P_sim_AMZN)+ec),col="red")
P_with_micro <- exp(log(P_sim_AMZN)+ec)

(AMZN.time*(57600-34200))*34200
AMZN_micro <- data.frame((AMZN.time*(57600-34200))*34200,P_with_micro)


 

####



obj <- function(paras){
  theta <- paras[1]
  v <- paras[2]
  n <- length(seq(1,length(logret),10))
  m <- diag((1+theta^2),n,n)
  
  for (i in 1:n-1){ 
    m[i,i+1]=(-theta)
    m[i+1,i]=(-theta)}
  
  finalres <- ((-n/2)*log(2*pi*(v^2)))-(0.5*log((1-theta^((2*n)+2))/(1-theta^2)))-(1/(2*v^2))*t(logret[seq(1,length(logret),10)])%*%solve(m)%*%(logret[seq(1,length(logret),10)])
  
  return(-finalres)
  
}

est4 <- optim(c(0.00000001,0.00000001), obj)

sigma.eta <- (est4$par[1])*(est4$par[2]^2)

sigma.real <- ( est4$par[2]^2 * (1+(est4$par[1])^2) - (2*sigma.eta) )*n

sum((logret[seq(1,length(logret),10)])^2)  

eta/n


################HW6

get_RVandVolume <- function(path,stock_name){

  
  for (j in 1:10){
    
    data <- read.csv(paste0(path,stock_name[j],"_message/",stock_name[j],"_2015-01-30_34200000_57600000_message_30.csv"),header=FALSE)
    data <- data[which(data[,2]==4),]
    time <- (data[,1]-34200)/(57600-34200)
    data.df <- data.frame(time , log(data[,5]/10000) )
    
    pre13RV <- c()
    pre26RV <- c()
    pre39RV <- c()
    len13 <- c()
    len26 <- c()
    len39 <- c()
        
    pre13 <- seq(0,1,1/13) #30mins
    for (i in 1:length(pre13)-1){
      PP <- data.df[which(pre13[i]< (data.df$time) & (data.df$time)<pre13[i+1]),2]
      pre13RV[i] <- sum((diff(PP))^2)
      len13[i] <- length(PP)
    }
    
    pre26 <- seq(0,1,1/26) #20mins
    for (i in 1:length(pre26)-1){
      PP <- data.df[which(pre26[i]<(data.df$time) & (data.df$time)<pre26[i+1]),2]
      pre26RV[i] <- sum((diff(PP))^2)
      len26[i] <- length(PP)
    }
    
    pre39 <- seq(0,1,1/39) #10mins
    for (i in 1:length(pre39)-1){
      PP <- data.df[which(pre39[i]<(data.df$time) & (data.df$time)<pre39[i+1]),2]
      pre39RV[i] <- sum((diff(PP))^2)
      len39[i] <- length(PP)
    }
    
    
    png(paste(stock_name[j],"RV_Volume.png"),width = 1024)
    par(mfrow=c(1,2))
    plot(pre39[-1],pre39RV,type="l",ylim=c(min(pre39RV),max(pre13RV)),col=1,ylab="RV",xlab="Time",main = paste0(stock_name[j],"_1/30_RV"))
    lines(pre26[-1],pre26RV,col=2)
    lines(pre13[-1],pre13RV,col=4)
    legend("topright",c("ticks30mins","ticks20mins","ticks10mins"),lty=c(1,1,1),col=c(4,2,1))
    
    plot(pre39[-1],len39,type="l",ylim=c(min(len39),max(len13)),col=1,ylab="Volume",xlab="Time",main = paste0(stock_name[j],"_1/30_Trade Volume"))
    lines(pre26[-1],len26,col=2)
    lines(pre13[-1],len13,col=4)
    legend("topright",c("ticks30mins","ticks20mins","ticks10mins"),lty=c(1,1,1),col=c(4,2,1))
    dev.off()
    
  
  }
  

}
get_RVandVolume(path,stock_name)


###


library(mvtnorm)


real_V <- c()
sum_r <- c()
Q2 <- c()
Q4 <- c()
MSE_SB <- c()
MSE_SK.2 <- c()
MSE_multi <- c()
MSE_Sp <- c()
MSE_SRK <- c()
MSE_SRK2 <- c()

AMZN <- read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-01-30_34200000_57600000_message_30.csv",header = F)
FB <- read.csv("/Volumes/Fast SSD/FB_message/FB_2015-01-30_34200000_57600000_message_30.csv",header = F)
GOOG <- read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-01-30_34200000_57600000_message_30.csv",header = F)
AMZN <- AMZN[AMZN$V2==4,]
FB <- FB[FB$V2==4,]
GOOG <- GOOG[GOOG$V2==4,]


AMZN.time <- union(as.numeric((AMZN$V1-34200)/(57600-34200)),as.numeric((AMZN$V1-34200)/(57600-34200)))
FB.time <- union(as.numeric((FB$V1-34200)/(57600-34200)),as.numeric((FB$V1-34200)/(57600-34200)))
GOOG.time <- union(as.numeric((GOOG$V1-34200)/(57600-34200)),as.numeric((GOOG$V1-34200)/(57600-34200)))

alltime <- sort(union(AMZN.time,FB.time))
index <- c(1:length(alltime))

alltimedf <- data.frame(index,alltime)

for(z in 1:100){
  
s0_AMZN <- AMZN$V5[1]/10000
s0_GOOG <- GOOG$V5[1]/10000
s0_FB <- FB$V5[1]/10000
n <- 5000
delta <- 1/n
#s0 <- 346
#n <- 25000
#delta <- 1/n
sigma_AMZN <- sum(diff(log(AMZN[AMZN$V2==4,]$V5))^2) #0.000633222
sigma_FB <- sum(diff(log(FB[FB$V2==4,]$V5))^2)
sigma_GOOG <- sum(diff(log(GOOG[GOOG$V2==4,]$V5))^2)
eta <- 10^(-7)

dist <- rmvnorm(n,c(0,0),matrix(c(1,0,0,1),ncol=2))
ec <- rnorm(n+1,0,sqrt(eta))

V_AMZN <- sigma_AMZN
K <- 10
W <- sqrt(0.8*K*V_AMZN)

P_AMZN <- c(log(s0_AMZN))
AMZN_sigma_list <- c(sigma_AMZN)
for (i in 1:n){
  AMZN_sigma_list[i+1] <- AMZN_sigma_list[i] + K*(V_AMZN-AMZN_sigma_list[i])*delta + W*sqrt(AMZN_sigma_list[i])*sqrt(delta)*dist[i,1] # + (1/4)*(W^2)*(delta)*((dist[i,1]^2)-1)
}
par(mfrow=c(2,2))
plot(P_AMZN,type="l")

##
V_GOOG <- sigma_GOOG
K <- 10
W <- sqrt(0.8*K*V_GOOG)

P_GOOG <- c(log(s0_GOOG))
GOOG_sigma_list <- c(sigma_GOOG)
for (i in 1:n){
  GOOG_sigma_list[i+1] <- GOOG_sigma_list[i] + K*(V_GOOG-GOOG_sigma_list[i])*delta + W*sqrt(GOOG_sigma_list[i])*sqrt(delta)*dist[i,1] + (1/4)*(W^2)*(delta)*((dist[i,1]^2)-1)
  P_GOOG[i+1] <- P_GOOG[i]+ sqrt(GOOG_sigma_list[i+1])*sqrt(delta)*dist[i,2]
}
plot(P_GOOG,type="l")
plot(GOOG$V5/10000,type="l")

##

V_FB <- sigma_FB
K <- 10
W <- sqrt(0.8*K*V_FB)

P_FB <- c(log(s0_FB))
FB_sigma_list <- c(sigma_FB)
for (i in 1:n){
  FB_sigma_list[i+1] <- FB_sigma_list[i] + K*(V_FB-FB_sigma_list[i])*delta + W*sqrt(FB_sigma_list[i])*sqrt(delta)*dist[i,1]  + (1/4)*(W^2)*(delta)*((dist[i,1]^2)-1)
}
plot(P_FB,type="l")
###
n <- 5000
sigma_s <- diag(x = c(sigma_AMZN,sigma_GOOG,sigma_FB),3,3 ,names = TRUE) %*% cce(yuima)$cormat %*% diag(x = c(sigma_AMZN,sigma_GOOG,sigma_FB),3,3 ,names = TRUE)
chol_sigma <- chol(sigma_s)
delta <- 1/n
dist1 <- rmvnorm(n,c(0,0,0),diag(x=delta,3,3))
AMZN_mu <- sum(diff(log(AMZN$V5/10000)))
GOOG_mu <- sum(diff(log(GOOG$V5/10000)))
FB_mu <- sum(diff(log(FB$V5/10000)))
sig_w <- c()
for(i in 1:n){
sig_w[i] <- t(chol_sigma) %*% dist1[i,]
}
for(i in 1:n){
  P_AMZN[i+1] <- P_AMZN[i]+ AMZN_mu*delta + (sqrt(t(chol_sigma)) * dist1[i,])[1]
  P_GOOG[i+1] <- P_GOOG[i]+ GOOG_mu *delta + (sqrt(t(chol_sigma)) * dist1[i,])[2]
  P_FB[i+1] <- P_FB[i]+ FB_mu*delta + (sqrt(t(chol_sigma)) * dist1[i,])[3]
}
real_V_AMZN <- sum(AMZN_sigma_list)/n
real_V_GOOG <- sum(GOOG_sigma_list)/n
real_V_FB <- sum(FB_sigma_list)/n
cov(P_AMZN,P_GOOG,real_V_FB )


x <- zoo(P_AMZN)
y <- zoo(P_GOOG)
z <- zoo(P_FB)
yuima <- setData(list(x,y,z))
QMLE <-  (abs((cce(yuima)$covmat - cce(yuima,method = "QMLE")$covmat) / sqrt(abs(cce(yuima)$covmat))))
TSCV <- (abs((cce(yuima)$covmat - cce(yuima,method = "TSCV",K=7,frequency=5)$covmat / sqrt(abs(cce(yuima)$covmat)) )))
RK <- (abs(cce(yuima)$covmat - cce(yuima,method = "RK",utime=2)$covmat)/ sqrt(abs(cce(yuima)$covmat)) )
Pre <- (abs(cce(yuima)$covmat - cce(yuima,method = "PHY",refreshing=TRUE,theta=0.1)$covmat) / sqrt(abs(cce(yuima)$covmat)))

###

real_V[z] <- sum(sigma_list)/n

r <- diff(P)

sum_r[z] <- sum(r^2)

Q2[z] <- sum(sigma_list[-1]*delta) 
Q4[z] <- sum((sigma_list^2)*delta) 

## Q2 = 0.0005659194 , sigma0 = 0.0006332225

##最佳取樣頻率，與Q2比

alpha <- 4*(eta^2)
beta <- 3*alpha

tau <- seq(1,1000,0.0001)
f <- function(tau,alpha,beta,Q4){
  return((2*alpha*(tau^3))+(beta*(tau^2))-2*Q4)
}
freB <- uniroot(f,alpha=alpha,beta=beta,Q4=Q4[z],interval = c(1,1000))

SB <- sum(diff(P[seq(1,length(r),n/round(freB$root))])^2)#跟Q2比

MSE_SB[z] <- (Q2[z]-SB)^2

###two_scale

KK <- round((3*alpha/Q4[z])^(1/3)*(n-1)^(2/3))

if(KK>1){
  KK <- KK
}else{
  KK=2
}

SK.2 <- sum((diff(P[seq(1,length(P),KK)])^2))-(((n-KK)/(KK*(n-1)))*sum(r^2))

MSE_SK.2[z] <- (SK.2-Q2[z])^2

##### multi_scale
k <- seq(5,12)
ZhangNew <- function(k){
  multi_scale <- c(sum(r^2))
  for(KK in k){
    Sm <- c()
    ai <- c()
    S <- c()
    for(i in 1:KK){
    ai[i] <- (12*i/(KK^2))*((i/KK)-(1/2)-(1/(2*KK)))/(1-(1/KK^2))
    S[i] <- (sum((diff(P[seq(i+1,length(P),KK)])^2)))
    Sm[i] <- sum(S[1:i])/i
    }
    multi_scale[KK-3] <- sum(ai*Sm)
  }
  return(multi_scale[length(multi_scale)])
}
S_multi_scale <- ZhangNew(k)
MSE_multi[z] <- (S_multi_scale-Q2[z])^2
######pre-average

g_x <- function(x){ return(min(x,1-x)) }
c <- 1/3
kn <- floor(sqrt(n-1)*c)
weight <- c()
ri_n <- c()
sum_ri_n_over_j <- c()
for (i in 0:(n-kn+1)){
  for (j in 1:(kn-1)){ 
  ri_n[j] <-  (g_x(j/kn)*r[(i+j)])^2
  }
  sum_ri_n_over_j[i+1] <- sum(ri_n)
}
Sp <- (1/(kn*(1/12)))*sum(sum_ri_n_over_j)-(sum(r^2)/((n-1)*(2)*(c^2)*(1/12)))
MSE_Sp[z] <- abs(Sp-Q2[z])^2

#####Kernel

opt_x <- function(x){ return((1+x)*exp(x)) }
opt_x_2 <- function(x){ return(sin((pi/2)*exp(x))) }
H <- round(sqrt(n-1)*sqrt(0.1))
L_h <- function(H){
  Lh <- c()
  for(j in 1:n-H){
    Lh[j] <- r[j]*r[j+H]
  }
  return(sum(Lh))
}

SRK <- c()
for (h in 0:H){
  SRK[h+1] <- opt_x(h/(H+1))*L_h(h)
}
SRK <- 2*sum(SRK)
MSE_SRK[z] <- (SRK-Q2[z])^2

SRK2 <- c()
for (h in 0:H){
  SRK2[h+1] <- opt_x_2(h/(H+1))*L_h(h)
}
SRK2 <- 2*sum(SRK2)
MSE_SRK2[z] <- (SRK2-Q2[z])^2

}
######
MSE_SB_RE <- 0
MSE_SK.2_RE <- 0
MSE_multi_RE <- 0
MSE_Sp_RE <- 0
MSE_SRK_RE <- 0
MSE_SRK2_RE <- 0
for (i in 1:100){
  MSE_SB_RE <- MSE_SB_RE + MSE_SB[i]/((Q2[i])^2)
  MSE_SK.2_RE <- MSE_SK.2_RE + MSE_SK.2[i]/((Q2[i])^2)
  MSE_multi_RE <- MSE_multi_RE + MSE_multi[i]/((Q2[i])^2)
  MSE_Sp_RE <-  MSE_Sp_RE + MSE_Sp[i]/((Q2[i])^2)
  MSE_SRK_RE <- MSE_SRK_RE + MSE_SRK[i]/((Q2[i])^2)
  MSE_SRK2_RE <- MSE_SRK2_RE + MSE_SRK2[i]/((Q2[i])^2)
}

sqrt(MSE_SB_RE/100)
sqrt(MSE_SK.2_RE/100)
sqrt(MSE_multi_RE/100)
sqrt(MSE_Sp_RE/100)
sqrt(MSE_SRK_RE/100)
