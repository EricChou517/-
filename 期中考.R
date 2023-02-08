library(MarkowitzR)
library(portfolio.optimization)
library("IntroCompFinR")
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP
library(highfrequency)
library(fPortfolio)

AMZN_0130 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-01-30_34200000_57600000_message_30.csv",header = F)
AMZN_0130 <- (AMZN_0130[AMZN_0130$V2==4,])
AMZN_0130$V5 <- AMZN_0130$V5 /10^4


BRKB_0130 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-01-30_34200000_57600000_message_30.csv",header = F)
BRKB_0130 <- (BRKB_0130[BRKB_0130$V2==4,])
BRKB_0130$V5 <- BRKB_0130$V5 /10^4


as.xts(AMZN$V1, dateFormat="POSIXct")
library(timeSeries)
# Suppose irregular timepoints:
start <- as.POSIXct("2010-01-01 09:30:00") 
ta <- start + AMZN$V1-34200
# Yielding the following timeseries:
a <- xts::as.xts(AMZN$V5/10000,1:length(ta), order.by = ta) 
a$PRICE
aggregatets(a$PRICE,on = "secs",k = 5 )
minrv <- minRV(rdata =a$PRICE, align.by = "secs",align.period = 5, makeReturns = TRUE)



asset.names = c("AMZN", "BRK.B", "FB","GOOG","JNJ","JPM","MSFT","V","WMT","XOM")
er = rep(0.0015,10)
names(er) = asset.names
qdata_aggregated <- aggregateQuotes(sample_qdata, on = "seconds", k = 30) 
head(qdata_aggregated)

colnames(cov0630) <- c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10')
row.names(cov0630) <- c(1,2,3,4,5,6,7,8,9,10)


cov0630 <- matrix(cov0630,10,10)

# compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(er, covmat ,shorts = TRUE)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue")



readfiles.all <- function (){
  
  for (j in 1){
    
    for (i in asset.names){
      
      k <- list.files(paste0("/Volumes/Fast SSD/",i,"_message"))[j]
      assign(i, read.csv(paste0("/Volumes/Fast SSD/",i,"_message/",k),header = F))
      i <- eval(parse(text = paste0(i,"[",i,"$V2==4,]")))

    }

  }
  
}


monkey.strategy.day  <- function(){
  
  
  name <- c("AMZN", "BRK.B", "FB", "GOOG", "MSFT")
  index <- seq(1,41,2)
  
  
  V <- 100
  
  for (j in index){
    
    v <- c()
    
    for (i in name){
      k <- list.files(paste0("/Volumes/Fast SSD/",i,"_message"))[j]
      assign(i, read.csv(paste0("/Volumes/Fast SSD/",i,"_message/",k),header = F))
      m <- eval(parse(text = paste0(i,"[",i,"$V2==4,]")))
      assign(paste0(i,".return"), log(m[nrow(m),"V5"]/10000) - log(m[1,"V5"]/10000))
      
      v <- c(v, V/5 * (1 + eval(parse(text = paste0(i,".return")))))
    }
    
    V <- sum(v)
    
  }
  
  return(V)
}

equal.weight.day <- monkey.strategy.day()


get.variance <- function(){
  index <- 41
  name <- c("AMZN", "BRK.B", "FB", "GOOG", "MSFT") 
  x <- list()
  
  for (j in 1:length(index)){
    
    assign(paste0( "v.", j), c())
    
    for (i in name){
      k <- list.files(paste0("/Volumes/Fast SSD/",i,"_message"))[index[j]]
      m <- read.csv(paste0("/Volumes/Fast SSD/",i,"_message/",k),header = F)     
      m <- m[m$V2==4,]
      
      assign(paste0( "v.", j), c(eval(parse(text = paste0( "v.", j))),  var(diff(log(m$V5))) ))
      
    }
    
    x[[j]] <- eval(parse(text = paste0( "v.", j)))
  }
  
  return(x)
}

variance <- get.variance()
variance


refreshTime <- function(stock1, stock2){
  
  
  rownames(stock1) <- seq(1:nrow(stock1))
  rownames(stock2) <- seq(1:nrow(stock2))
  
  s <- 0
  
  t.ori.1 <- 1
  t.ori.2 <- 1
  
  freq <- c(nrow(stock1), nrow(stock2))
  k <- which.min(c(stock1[nrow(stock1),"V1"], stock2[nrow(stock2),"V1"]))
  
  final <- 1
  
  while (final != freq[k]){
    
    time0 <- c(stock1[t.ori.1 + 1, "V1"], stock2[t.ori.2 + 1, "V1"])
    
    
    if (time0[1] == time0[2]){
      u <- 1
      v <- 2
    }else{
      
      u <- which(time0 == max(time0))
      
      l <- c(1,2)[! c(1,2) %in% u]
    }
    
    
    d1 <- eval(parse(text = paste0("stock",u)))
    d2 <- eval(parse(text = paste0("stock",l)))
    
    tu <- eval(parse(text = paste0("t.ori.",u)))
    tl <- eval(parse(text = paste0("t.ori.",l)))
    
    
    d2.max.time <- d2[d2$V1 == tail(d2$V1[d2$V1 <= time0[u]], n = 1), ]
    d2.max.time <- d2.max.time[nrow(d2.max.time), ]
    
    
    
    s <- s + ((log(d2.max.time$V5/10000) - log(d2[tl, ]$V5/10000)) * (log(d1[tu + 1, ]$V5/10000) - log(d1[tu, ]$V5/10000)))
    
    assign(paste0("t.ori.",u), eval(parse(text = paste0("t.ori.",u))) + 1)
    assign(paste0("t.ori.",l), as.numeric(rownames(d2.max.time)))
    
    final <- eval(parse(text = paste0("t.ori.",k)))
    
    
    
  }
  
  
  return(s)
}



name <- c("AMZN", "BRK.B", "FB", "GOOG", "MSFT")
a=c()
i=1
for (i in 1:ncol(combn(name,2))){
  a[i] <- refreshTime(eval(parse(text = combn(name,2)[1,i])),eval(parse(text = combn(name,2)[2,i])))
}
a




