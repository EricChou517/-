AMZN_0130 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-01-30_34200000_57600000_message_30.csv",header = F)
AMZN_0130 <- (AMZN_0130[AMZN_0130$V2==4,])
AMZN_0130$V5 <- AMZN_0130$V5 /10^4
rownames(AMZN_0130) <- seq(1,nrow(AMZN_0130))

BRKB_0130 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-01-30_34200000_57600000_message_30.csv",header = F)
BRKB_0130 <- (BRKB_0130[BRKB_0130$V2==4,])
BRKB_0130$V5 <- BRKB_0130$V5 /10^4
rownames(BRKB_0130) <- seq(1,nrow(BRKB_0130))

FB_0130 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-01-30_34200000_57600000_message_30.csv",header = F)
FB_0130 <- (FB_0130[FB_0130$V2==4,])
FB_0130$V5 <- FB_0130$V5 /10^4
rownames(FB_0130) <- seq(1,nrow(FB_0130))

GOOG_0130 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-01-30_34200000_57600000_message_30.csv",header = F)
GOOG_0130 <- (GOOG_0130[GOOG_0130$V2==4,])
GOOG_0130$V5 <- GOOG_0130$V5 /10^4
rownames(GOOG_0130) <- seq(1,nrow(GOOG_0130))

JNJ_0130 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-01-30_34200000_57600000_message_30.csv",header = F)
JNJ_0130 <- (JNJ_0130[JNJ_0130$V2==4,])
JNJ_0130$V5 <- JNJ_0130$V5 /10^4
rownames(JNJ_0130) <- seq(1,nrow(JNJ_0130))

JPM_0130 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-01-30_34200000_57600000_message_30.csv",header = F)
JPM_0130 <- (JPM_0130[JPM_0130$V2==4,])
JPM_0130$V5 <- JPM_0130$V5 /10^4
rownames(JPM_0130) <- seq(1,nrow(JPM_0130))

MSFT_0130 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-01-30_34200000_57600000_message_30.csv",header = F)
MSFT_0130 <- (MSFT_0130[MSFT_0130$V2==4,])
MSFT_0130$V5 <- MSFT_0130$V5 /10^4
rownames(MSFT_0130) <- seq(1,nrow(MSFT_0130))

V_0130 = read.csv("/Volumes/Fast SSD/V_message/V_2015-01-30_34200000_57600000_message_30.csv",header = F)
V_0130 <- (V_0130[V_0130$V2==4,])
V_0130$V5 <- V_0130$V5 /10^4
rownames(V_0130) <- seq(1,nrow(V_0130))

WMT_0130 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-01-30_34200000_57600000_message_30.csv",header = F)
WMT_0130 <- (WMT_0130[WMT_0130$V2==4,])
WMT_0130$V5 <- WMT_0130$V5 /10^4
rownames(WMT_0130) <- seq(1,nrow(WMT_0130))

XOM_0130 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-01-30_34200000_57600000_message_30.csv",header = F)
XOM_0130 <- (XOM_0130[XOM_0130$V2==4,])
XOM_0130$V5 <- XOM_0130$V5 /10^4
rownames(XOM_0130) <- seq(1,nrow(XOM_0130))




AMZN_0227 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-02-27_34200000_57600000_message_30.csv",header = F)
AMZN_0227 <- (AMZN_0227[AMZN_0227$V2==4,])
AMZN_0227$V5 <- AMZN_0227$V5 /10^4
rownames(AMZN_0227) <- seq(1,nrow(AMZN_0227))

BRKB_0227 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-02-27_34200000_57600000_message_30.csv",header = F)
BRKB_0227 <- (BRKB_0227[BRKB_0227$V2==4,])
BRKB_0227$V5 <- BRKB_0227$V5 /10^4
rownames(BRKB_0227) <- seq(1,nrow(BRKB_0227))

FB_0227 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-02-27_34200000_57600000_message_30.csv",header = F)
FB_0227 <- (FB_0227[FB_0227$V2==4,])
FB_0227$V5 <- FB_0227$V5 /10^4
rownames(FB_0227) <- seq(1,nrow(FB_0227))

GOOG_0227 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-02-27_34200000_57600000_message_30.csv",header = F)
GOOG_0227 <- (GOOG_0227[GOOG_0227$V2==4,])
GOOG_0227$V5 <- GOOG_0227$V5 /10^4
rownames(GOOG_0227) <- seq(1,nrow(GOOG_0227))

JNJ_0227 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-02-27_34200000_57600000_message_30.csv",header = F)
JNJ_0227 <- (JNJ_0227[JNJ_0227$V2==4,])
JNJ_0227$V5 <- JNJ_0227$V5 /10^4
rownames(JNJ_0227) <- seq(1,nrow(JNJ_0227))

JPM_0227 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-02-27_34200000_57600000_message_30.csv",header = F)
JPM_0227 <- (JPM_0227[JPM_0227$V2==4,])
JPM_0227$V5 <- JPM_0227$V5 /10^4
rownames(JPM_0227) <- seq(1,nrow(JPM_0227))

MSFT_0227 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-02-27_34200000_57600000_message_30.csv",header = F)
MSFT_0227 <- (MSFT_0227[MSFT_0227$V2==4,])
MSFT_0227$V5 <- MSFT_0227$V5 /10^4
rownames(MSFT_0227) <- seq(1,nrow(MSFT_0227))

V_0227 = read.csv("/Volumes/Fast SSD/V_message/V_2015-02-27_34200000_57600000_message_30.csv",header = F)
V_0227 <- (V_0227[V_0227$V2==4,])
V_0227$V5 <- V_0227$V5 /10^4
rownames(V_0227) <- seq(1,nrow(V_0227))

WMT_0227 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-02-27_34200000_57600000_message_30.csv",header = F)
WMT_0227 <- (WMT_0227[WMT_0227$V2==4,])
WMT_0227$V5 <- WMT_0227$V5 /10^4
rownames(WMT_0227) <- seq(1,nrow(WMT_0227))

XOM_0227 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-02-27_34200000_57600000_message_30.csv",header = F)
XOM_0227 <- (XOM_0227[XOM_0227$V2==4,])
XOM_0227$V5 <- XOM_0227$V5 /10^4
rownames(XOM_0227) <- seq(1,nrow(XOM_0227))



AMZN_0331 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-03-31_34200000_57600000_message_30.csv",header = F)
AMZN_0331 <- (AMZN_0331[AMZN_0331$V2==4,])
AMZN_0331$V5 <- AMZN_0331$V5 /10^4
rownames(AMZN_0331) <- seq(1,nrow(AMZN_0331))

BRKB_0331 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-03-31_34200000_57600000_message_30.csv",header = F)
BRKB_0331 <- (BRKB_0331[BRKB_0331$V2==4,])
BRKB_0331$V5 <- BRKB_0331$V5 /10^4
rownames(BRKB_0331) <- seq(1,nrow(BRKB_0331))

FB_0331 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-03-31_34200000_57600000_message_30.csv",header = F)
FB_0331 <- (FB_0331[FB_0331$V2==4,])
FB_0331$V5 <- FB_0331$V5 /10^4
rownames(FB_0331) <- seq(1,nrow(FB_0331))

GOOG_0331 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-03-31_34200000_57600000_message_30.csv",header = F)
GOOG_0331 <- (GOOG_0331[GOOG_0331$V2==4,])
GOOG_0331$V5 <- GOOG_0331$V5 /10^4
rownames(GOOG_0331) <- seq(1,nrow(GOOG_0331))

JNJ_0331 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-03-31_34200000_57600000_message_30.csv",header = F)
JNJ_0331 <- (JNJ_0331[JNJ_0331$V2==4,])
JNJ_0331$V5 <- JNJ_0331$V5 /10^4
rownames(JNJ_0331) <- seq(1,nrow(JNJ_0331))

JPM_0331 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-03-31_34200000_57600000_message_30.csv",header = F)
JPM_0331 <- (JPM_0331[JPM_0331$V2==4,])
JPM_0331$V5 <- JPM_0331$V5 /10^4
rownames(JPM_0331) <- seq(1,nrow(JPM_0331))

MSFT_0331 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-03-31_34200000_57600000_message_30.csv",header = F)
MSFT_0331 <- (MSFT_0331[MSFT_0331$V2==4,])
MSFT_0331$V5 <- MSFT_0331$V5 /10^4
rownames(MSFT_0331) <- seq(1,nrow(MSFT_0331))

V_0331 = read.csv("/Volumes/Fast SSD/V_message/V_2015-03-31_34200000_57600000_message_30.csv",header = F)
V_0331 <- (V_0331[V_0331$V2==4,])
V_0331$V5 <- V_0331$V5 /10^4
rownames(V_0331) <- seq(1,nrow(V_0331))

WMT_0331 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-03-31_34200000_57600000_message_30.csv",header = F)
WMT_0331 <- (WMT_0331[WMT_0331$V2==4,])
WMT_0331$V5 <- WMT_0331$V5 /10^4
rownames(WMT_0331) <- seq(1,nrow(WMT_0331))

XOM_0331 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-03-31_34200000_57600000_message_30.csv",header = F)
XOM_0331 <- (XOM_0331[XOM_0331$V2==4,])
XOM_0331$V5 <- XOM_0331$V5 /10^4
rownames(XOM_0331) <- seq(1,nrow(XOM_0331))



AMZN_0430 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-04-30_34200000_57600000_message_30.csv",header = F)
AMZN_0430 <- (AMZN_0430[AMZN_0430$V2==4,])
AMZN_0430$V5 <- AMZN_0430$V5 /10^4
rownames(AMZN_0430) <- seq(1,nrow(AMZN_0430))

BRKB_0430 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-04-30_34200000_57600000_message_30.csv",header = F)
BRKB_0430 <- (BRKB_0430[BRKB_0430$V2==4,])
BRKB_0430$V5 <- BRKB_0430$V5 /10^4
rownames(BRKB_0430) <- seq(1,nrow(BRKB_0430))

FB_0430 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-04-30_34200000_57600000_message_30.csv",header = F)
FB_0430 <- (FB_0430[FB_0430$V2==4,])
FB_0430$V5 <- FB_0430$V5 /10^4
rownames(FB_0430) <- seq(1,nrow(FB_0430))

GOOG_0430 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-04-30_34200000_57600000_message_30.csv",header = F)
GOOG_0430 <- (GOOG_0430[GOOG_0430$V2==4,])
GOOG_0430$V5 <- GOOG_0430$V5 /10^4
rownames(GOOG_0430) <- seq(1,nrow(GOOG_0430))

JNJ_0430 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-04-30_34200000_57600000_message_30.csv",header = F)
JNJ_0430 <- (JNJ_0430[JNJ_0430$V2==4,])
JNJ_0430$V5 <- JNJ_0430$V5 /10^4
rownames(JNJ_0430) <- seq(1,nrow(JNJ_0430))

JPM_0430 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-04-30_34200000_57600000_message_30.csv",header = F)
JPM_0430 <- (JPM_0430[JPM_0430$V2==4,])
JPM_0430$V5 <- JPM_0430$V5 /10^4
rownames(JPM_0430) <- seq(1,nrow(JPM_0430))

MSFT_0430 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-04-30_34200000_57600000_message_30.csv",header = F)
MSFT_0430 <- (MSFT_0430[MSFT_0430$V2==4,])
MSFT_0430$V5 <- MSFT_0430$V5 /10^4
rownames(MSFT_0430) <- seq(1,nrow(MSFT_0430))

V_0430 = read.csv("/Volumes/Fast SSD/V_message/V_2015-04-30_34200000_57600000_message_30.csv",header = F)
V_0430 <- (V_0430[V_0430$V2==4,])
V_0430$V5 <- V_0430$V5 /10^4
rownames(V_0430) <- seq(1,nrow(V_0430))

WMT_0430 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-04-30_34200000_57600000_message_30.csv",header = F)
WMT_0430 <- (WMT_0430[WMT_0430$V2==4,])
WMT_0430$V5 <- WMT_0430$V5 /10^4
rownames(WMT_0430) <- seq(1,nrow(WMT_0430))

XOM_0430 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-04-30_34200000_57600000_message_30.csv",header = F)
XOM_0430 <- (XOM_0430[XOM_0430$V2==4,])
XOM_0430$V5 <- XOM_0430$V5 /10^4
rownames(XOM_0430) <- seq(1,nrow(XOM_0430))



AMZN_0529 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-05-29_34200000_57600000_message_30.csv",header = F)
AMZN_0529 <- (AMZN_0529[AMZN_0529$V2==4,])
AMZN_0529$V5 <- AMZN_0529$V5 /10^4
rownames(AMZN_0529) <- seq(1,nrow(AMZN_0529))

BRKB_0529 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-05-29_34200000_57600000_message_30.csv",header = F)
BRKB_0529 <- (BRKB_0529[BRKB_0529$V2==4,])
BRKB_0529$V5 <- BRKB_0529$V5 /10^4
rownames(BRKB_0529) <- seq(1,nrow(BRKB_0529))

FB_0529 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-05-29_34200000_57600000_message_30.csv",header = F)
FB_0529 <- (FB_0529[FB_0529$V2==4,])
FB_0529$V5 <- FB_0529$V5 /10^4
rownames(FB_0529) <- seq(1,nrow(FB_0529))

GOOG_0529 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-05-29_34200000_57600000_message_30.csv",header = F)
GOOG_0529 <- (GOOG_0529[GOOG_0529$V2==4,])
GOOG_0529$V5 <- GOOG_0529$V5 /10^4
rownames(GOOG_0529) <- seq(1,nrow(GOOG_0529))

JNJ_0529 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-05-29_34200000_57600000_message_30.csv",header = F)
JNJ_0529 <- (JNJ_0529[JNJ_0529$V2==4,])
JNJ_0529$V5 <- JNJ_0529$V5 /10^4
rownames(JNJ_0529) <- seq(1,nrow(JNJ_0529))

JPM_0529 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-05-29_34200000_57600000_message_30.csv",header = F)
JPM_0529 <- (JPM_0529[JPM_0529$V2==4,])
JPM_0529$V5 <- JPM_0529$V5 /10^4
rownames(JPM_0529) <- seq(1,nrow(JPM_0529))

MSFT_0529 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-05-29_34200000_57600000_message_30.csv",header = F)
MSFT_0529 <- (MSFT_0529[MSFT_0529$V2==4,])
MSFT_0529$V5 <- MSFT_0529$V5 /10^4
rownames(MSFT_0529) <- seq(1,nrow(MSFT_0529))

V_0529 = read.csv("/Volumes/Fast SSD/V_message/V_2015-05-29_34200000_57600000_message_30.csv",header = F)
V_0529 <- (V_0529[V_0529$V2==4,])
V_0529$V5 <- V_0529$V5 /10^4
rownames(V_0529) <- seq(1,nrow(V_0529))

WMT_0529 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-05-29_34200000_57600000_message_30.csv",header = F)
WMT_0529 <- (WMT_0529[WMT_0529$V2==4,])
WMT_0529$V5 <- WMT_0529$V5 /10^4
rownames(WMT_0529) <- seq(1,nrow(WMT_0529))

XOM_0529 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-05-29_34200000_57600000_message_30.csv",header = F)
XOM_0529 <- (XOM_0529[XOM_0529$V2==4,])
XOM_0529$V5 <- XOM_0529$V5 /10^4
rownames(XOM_0529) <- seq(1,nrow(XOM_0529))


AMZN_0630 = read.csv("/Volumes/Fast SSD/AMZN_message/AMZN_2015-06-30_34200000_57600000_message_30.csv",header = F)
AMZN_0630 <- (AMZN_0630[AMZN_0630$V2==4,])
AMZN_0630$V5 <- AMZN_0630$V5 /10^4
rownames(AMZN_0630) <- seq(1,nrow(AMZN_0630))

BRKB_0630 = read.csv("/Volumes/Fast SSD/BRK.B_message/BRK.B_2015-06-30_34200000_57600000_message_30.csv",header = F)
BRKB_0630 <- (BRKB_0630[BRKB_0630$V2==4,])
BRKB_0630$V5 <- BRKB_0630$V5 /10^4
rownames(BRKB_0630) <- seq(1,nrow(BRKB_0630))

FB_0630 = read.csv("/Volumes/Fast SSD/FB_message/FB_2015-06-30_34200000_57600000_message_30.csv",header = F)
FB_0630 <- (FB_0630[FB_0630$V2==4,])
FB_0630$V5 <- FB_0630$V5 /10^4
rownames(FB_0630) <- seq(1,nrow(FB_0630))

GOOG_0630 = read.csv("/Volumes/Fast SSD/GOOG_message/GOOG_2015-06-30_34200000_57600000_message_30.csv",header = F)
GOOG_0630 <- (GOOG_0630[GOOG_0630$V2==4,])
GOOG_0630$V5 <- GOOG_0630$V5 /10^4
rownames(GOOG_0630) <- seq(1,nrow(GOOG_0630))

JNJ_0630 = read.csv("/Volumes/Fast SSD/JNJ_message/JNJ_2015-06-30_34200000_57600000_message_30.csv",header = F)
JNJ_0630 <- (JNJ_0630[JNJ_0630$V2==4,])
JNJ_0630$V5 <- JNJ_0630$V5 /10^4
rownames(JNJ_0630) <- seq(1,nrow(JNJ_0630))

JPM_0630 = read.csv("/Volumes/Fast SSD/JPM_message/JPM_2015-06-30_34200000_57600000_message_30.csv",header = F)
JPM_0630 <- (JPM_0630[JPM_0630$V2==4,])
JPM_0630$V5 <- JPM_0630$V5 /10^4
rownames(JPM_0630) <- seq(1,nrow(JPM_0630))

MSFT_0630 = read.csv("/Volumes/Fast SSD/MSFT_message/MSFT_2015-06-30_34200000_57600000_message_30.csv",header = F)
MSFT_0630 <- (MSFT_0630[MSFT_0630$V2==4,])
MSFT_0630$V5 <- MSFT_0630$V5 /10^4
rownames(MSFT_0630) <- seq(1,nrow(MSFT_0630))

V_0630 = read.csv("/Volumes/Fast SSD/V_message/V_2015-06-30_34200000_57600000_message_30.csv",header = F)
V_0630 <- (V_0630[V_0630$V2==4,])
V_0630$V5 <- V_0630$V5 /10^4
rownames(V_0630) <- seq(1,nrow(V_0630))

WMT_0630 = read.csv("/Volumes/Fast SSD/WMT_message/WMT_2015-06-30_34200000_57600000_message_30.csv",header = F)
WMT_0630 <- (WMT_0630[WMT_0630$V2==4,])
WMT_0630$V5 <- WMT_0630$V5 /10^4
rownames(WMT_0630) <- seq(1,nrow(WMT_0630))

XOM_0630 = read.csv("/Volumes/Fast SSD/XOM_message/XOM_2015-06-30_34200000_57600000_message_30.csv",header = F)
XOM_0630 <- (XOM_0630[XOM_0630$V2==4,])
XOM_0630$V5 <- XOM_0630$V5 /10^4
rownames(XOM_0630) <- seq(1,nrow(XOM_0630))

names_0130 = c('AMZN_0130','BRKB_0130','FB_0130','GOOG_0130','JNJ_0130','JPM_0130','MSFT_0130','V_0130','WMT_0130','XOM_0130')
names_0227 = c('AMZN_0227','BRKB_0227','FB_0227','GOOG_0227','JNJ_0227','JPM_0227','MSFT_0227','V_0227','WMT_0227','XOM_0227')
names_0331 = c('AMZN_0331','BRKB_0331','FB_0331','GOOG_0331','JNJ_0331','JPM_0331','MSFT_0331','V_0331','WMT_0331','XOM_0331')
names_0430 = c('AMZN_0430','BRKB_0430','FB_0430','GOOG_0430','JNJ_0430','JPM_0430','MSFT_0430','V_0430','WMT_0430','XOM_0430')
names_0529 = c('AMZN_0529','BRKB_0529','FB_0529','GOOG_0529','JNJ_0529','JPM_0529','MSFT_0529','V_0529','WMT_0529','XOM_0529')
names_0630 = c('AMZN_0630','BRKB_0630','FB_0630','GOOG_0630','JNJ_0630','JPM_0630','MSFT_0630','V_0630','WMT_0630','XOM_0630')



refreshTime <- function(stock1, stock2){
  
  
  rownames(stock1) <- seq(1:nrow(stock1))
  rownames(stock2) <- seq(1:nrow(stock2))
  
  s <- 0
  
  t.ori.1 <- 1
  t.ori.2 <- 1
  
  freq <- c(nrow(stock1), nrow(stock2))
  k <- which.min(c(stock1[nrow(stock1),"V1"], stock2[nrow(stock2),"V1"]))
  
  final <- 1
  
  while ( final != freq[k]){
    
    time0 <- c(stock1[t.ori.1 + 1, "V1"], stock2[t.ori.2 + 1, "V1"])
    
    
    u <- which(time0 == max(time0))
    
    l <- c(1,2)[! c(1,2) %in% u]
    
    
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

a=c()
for (i in 1:ncol(combn(names_0130,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0130,2)[1,i])),eval(parse(text = combn(names_0130,2)[2,i])))
}


cov0130 <- data.frame(row.names =names_0130)

cov0130[1,1] <- var(diff(log(AMZN_0130$V5)))
cov0130[2,2] <- var(diff(log(BRKB_0130$V5)))
cov0130[3,3] <- var(diff(log(FB_0130$V5)))
cov0130[4,4] <- var(diff(log(GOOG_0130$V5)))
cov0130[5,5] <- var(diff(log(MSFT_0130$V5)))
cov0130[6,6] <- var(diff(log(JNJ_0130$V5)))
cov0130[7,7] <- var(diff(log(JPM_0130$V5)))
cov0130[8,8] <- var(diff(log(V_0130$V5)))
cov0130[9,9] <- var(diff(log(WMT_0130$V5)))
cov0130[10,10] <- var(diff(log(XOM_0130$V5)))

cov0130[2:10,1] <- a[1:9]
cov0130[3:10,2] <- a[10:17]
cov0130[4:10,3] <- a[18:24]
cov0130[5:10,4] <- a[25:30]
cov0130[6:10,5] <- a[31:35]
cov0130[7:10,6] <- a[36:39]
cov0130[8:10,7] <- a[40:42]
cov0130[9:10,8] <- a[43:44]
cov0130[10:10,9] <- a[45]

cov0130[1,2:10] <- a[1:9]
cov0130[2,3:10] <- a[10:17]
cov0130[3,4:10] <- a[18:24]
cov0130[4,5:10] <- a[25:30]
cov0130[5,6:10] <- a[31:35]
cov0130[6,7:10] <- a[36:39]
cov0130[7,8:10] <- a[40:42]
cov0130[8,9:10] <- a[43:44]
cov0130[9,10:10] <- a[45]

colnames(cov0130) <- names_0130

a=c()
for (i in 1:ncol(combn(names_0227,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0227,2)[1,i])),eval(parse(text = combn(names_0227,2)[2,i])))
}


cov0227 <- data.frame(row.names =names_0227)

cov0227[1,1] <- var(diff(log(AMZN_0227$V5)))
cov0227[2,2] <- var(diff(log(BRKB_0227$V5)))
cov0227[3,3] <- var(diff(log(FB_0227$V5)))
cov0227[4,4] <- var(diff(log(GOOG_0227$V5)))
cov0227[5,5] <- var(diff(log(MSFT_0227$V5)))
cov0227[6,6] <- var(diff(log(JNJ_0227$V5)))
cov0227[7,7] <- var(diff(log(JPM_0227$V5)))
cov0227[8,8] <- var(diff(log(V_0227$V5)))
cov0227[9,9] <- var(diff(log(WMT_0227$V5)))
cov0227[10,10] <- var(diff(log(XOM_0227$V5)))

cov0227[2:10,1] <- a[1:9]
cov0227[3:10,2] <- a[10:17]
cov0227[4:10,3] <- a[18:24]
cov0227[5:10,4] <- a[25:30]
cov0227[6:10,5] <- a[31:35]
cov0227[7:10,6] <- a[36:39]
cov0227[8:10,7] <- a[40:42]
cov0227[9:10,8] <- a[43:44]
cov0227[10:10,9] <- a[45]

cov0227[1,2:10] <- a[1:9]
cov0227[2,3:10] <- a[10:17]
cov0227[3,4:10] <- a[18:24]
cov0227[4,5:10] <- a[25:30]
cov0227[5,6:10] <- a[31:35]
cov0227[6,7:10] <- a[36:39]
cov0227[7,8:10] <- a[40:42]
cov0227[8,9:10] <- a[43:44]
cov0227[9,10:10] <- a[45]

colnames(cov0227) <- names_0227



a=c()
for (i in 1:ncol(combn(names_0331,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0331,2)[1,i])),eval(parse(text = combn(names_0331,2)[2,i])))
}


cov0331 <- data.frame(row.names =names_0331)

cov0331[1,1] <- var(diff(log(AMZN_0331$V5)))
cov0331[2,2] <- var(diff(log(BRKB_0331$V5)))
cov0331[3,3] <- var(diff(log(FB_0331$V5)))
cov0331[4,4] <- var(diff(log(GOOG_0331$V5)))
cov0331[5,5] <- var(diff(log(MSFT_0331$V5)))
cov0331[6,6] <- var(diff(log(JNJ_0331$V5)))
cov0331[7,7] <- var(diff(log(JPM_0331$V5)))
cov0331[8,8] <- var(diff(log(V_0331$V5)))
cov0331[9,9] <- var(diff(log(WMT_0331$V5)))
cov0331[10,10] <- var(diff(log(XOM_0331$V5)))

cov0331[2:10,1] <- a[1:9]
cov0331[3:10,2] <- a[10:17]
cov0331[4:10,3] <- a[18:24]
cov0331[5:10,4] <- a[25:30]
cov0331[6:10,5] <- a[31:35]
cov0331[7:10,6] <- a[36:39]
cov0331[8:10,7] <- a[40:42]
cov0331[9:10,8] <- a[43:44]
cov0331[10:10,9] <- a[45]

cov0331[1,2:10] <- a[1:9]
cov0331[2,3:10] <- a[10:17]
cov0331[3,4:10] <- a[18:24]
cov0331[4,5:10] <- a[25:30]
cov0331[5,6:10] <- a[31:35]
cov0331[6,7:10] <- a[36:39]
cov0331[7,8:10] <- a[40:42]
cov0331[8,9:10] <- a[43:44]
cov0331[9,10:10] <- a[45]

colnames(cov0331) <- names_0331


a=c()
for (i in 1:ncol(combn(names_0430,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0430,2)[1,i])),eval(parse(text = combn(names_0430,2)[2,i])))
}


cov0430 <- data.frame(row.names =names_0430)

cov0430[1,1] <- var(diff(log(AMZN_0430$V5)))
cov0430[2,2] <- var(diff(log(BRKB_0430$V5)))
cov0430[3,3] <- var(diff(log(FB_0430$V5)))
cov0430[4,4] <- var(diff(log(GOOG_0430$V5)))
cov0430[5,5] <- var(diff(log(MSFT_0430$V5)))
cov0430[6,6] <- var(diff(log(JNJ_0430$V5)))
cov0430[7,7] <- var(diff(log(JPM_0430$V5)))
cov0430[8,8] <- var(diff(log(V_0430$V5)))
cov0430[9,9] <- var(diff(log(WMT_0430$V5)))
cov0430[10,10] <- var(diff(log(XOM_0430$V5)))

cov0430[2:10,1] <- a[1:9]
cov0430[3:10,2] <- a[10:17]
cov0430[4:10,3] <- a[18:24]
cov0430[5:10,4] <- a[25:30]
cov0430[6:10,5] <- a[31:35]
cov0430[7:10,6] <- a[36:39]
cov0430[8:10,7] <- a[40:42]
cov0430[9:10,8] <- a[43:44]
cov0430[10:10,9] <- a[45]

cov0430[1,2:10] <- a[1:9]
cov0430[2,3:10] <- a[10:17]
cov0430[3,4:10] <- a[18:24]
cov0430[4,5:10] <- a[25:30]
cov0430[5,6:10] <- a[31:35]
cov0430[6,7:10] <- a[36:39]
cov0430[7,8:10] <- a[40:42]
cov0430[8,9:10] <- a[43:44]
cov0430[9,10:10] <- a[45]

colnames(cov0430) <- names_0430


a=c()
for (i in 1:ncol(combn(names_0529,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0529,2)[1,i])),eval(parse(text = combn(names_0529,2)[2,i])))
}


cov0529 <- data.frame(row.names =names_0529)

cov0529[1,1] <- var(diff(log(AMZN_0529$V5)))
cov0529[2,2] <- var(diff(log(BRKB_0529$V5)))
cov0529[3,3] <- var(diff(log(FB_0529$V5)))
cov0529[4,4] <- var(diff(log(GOOG_0529$V5)))
cov0529[5,5] <- var(diff(log(MSFT_0529$V5)))
cov0529[6,6] <- var(diff(log(JNJ_0529$V5)))
cov0529[7,7] <- var(diff(log(JPM_0529$V5)))
cov0529[8,8] <- var(diff(log(V_0529$V5)))
cov0529[9,9] <- var(diff(log(WMT_0529$V5)))
cov0529[10,10] <- var(diff(log(XOM_0529$V5)))

cov0529[2:10,1] <- a[1:9]
cov0529[3:10,2] <- a[10:17]
cov0529[4:10,3] <- a[18:24]
cov0529[5:10,4] <- a[25:30]
cov0529[6:10,5] <- a[31:35]
cov0529[7:10,6] <- a[36:39]
cov0529[8:10,7] <- a[40:42]
cov0529[9:10,8] <- a[43:44]
cov0529[10:10,9] <- a[45]

cov0529[1,2:10] <- a[1:9]
cov0529[2,3:10] <- a[10:17]
cov0529[3,4:10] <- a[18:24]
cov0529[4,5:10] <- a[25:30]
cov0529[5,6:10] <- a[31:35]
cov0529[6,7:10] <- a[36:39]
cov0529[7,8:10] <- a[40:42]
cov0529[8,9:10] <- a[43:44]
cov0529[9,10:10] <- a[45]

colnames(cov0529) <- names_0529


a=c()
for (i in 1:ncol(combn(names_0630,2))){
  a[i] <- refreshTime(eval(parse(text = combn(names_0630,2)[1,i])),eval(parse(text = combn(names_0630,2)[2,i])))
}


cov0630 <- data.frame(row.names =names_0630)

cov0630[1,1] <- var(diff(log(AMZN_0630$V5)))
cov0630[2,2] <- var(diff(log(BRKB_0630$V5)))
cov0630[3,3] <- var(diff(log(FB_0630$V5)))
cov0630[4,4] <- var(diff(log(GOOG_0630$V5)))
cov0630[5,5] <- var(diff(log(MSFT_0630$V5)))
cov0630[6,6] <- var(diff(log(JNJ_0630$V5)))
cov0630[7,7] <- var(diff(log(JPM_0630$V5)))
cov0630[8,8] <- var(diff(log(V_0630$V5)))
cov0630[9,9] <- var(diff(log(WMT_0630$V5)))
cov0630[10,10] <- var(diff(log(XOM_0630$V5)))

cov0630[2:10,1] <- a[1:9]
cov0630[3:10,2] <- a[10:17]
cov0630[4:10,3] <- a[18:24]
cov0630[5:10,4] <- a[25:30]
cov0630[6:10,5] <- a[31:35]
cov0630[7:10,6] <- a[36:39]
cov0630[8:10,7] <- a[40:42]
cov0630[9:10,8] <- a[43:44]
cov0630[10:10,9] <- a[45]

cov0630[1,2:10] <- a[1:9]
cov0630[2,3:10] <- a[10:17]
cov0630[3,4:10] <- a[18:24]
cov0630[4,5:10] <- a[25:30]
cov0630[5,6:10] <- a[31:35]
cov0630[6,7:10] <- a[36:39]
cov0630[7,8:10] <- a[40:42]
cov0630[8,9:10] <- a[43:44]
cov0630[9,10:10] <- a[45]

colnames(cov0630) <- names_0630

library(mvtnorm)
library(MASS)
mvrnorm


for (i in length(AMZN_0130$V5)){
  
}