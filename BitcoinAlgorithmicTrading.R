
# -- ---------------------------------------------------------------------------------------- --- #
# -- Initial Developer: IF.Francisco.ME ----------------------------------------------------- --- #
# -- Trading Bitcoin ------------------------------------------------------------------------ --- #
# -- License: GNU License V3 ---------------------------------------------------------------- --- #
# -- Post Web Page -- http://bit.ly/BitcoinAlgorithmicTrading ------------------------------- --- #
# -- ---------------------------------------------------------------------------------------- --- #

# -- Load the R Packages ------------------------------------------------------------------------ #

Pkg <- c("base","digest","downloader","fBasics","forecast","grid",
"gridExtra","ggplot2","httr","jsonlite","lubridate","moments",
"orderbook","openssl","PerformanceAnalytics","plyr","quantmod",
"Quandl","reshape2","RCurl","stats","scales","tseries","TTR","TSA",
"xts","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)

# -- Pre-configuration of the R environment ----------------------------------------------------- #

setwd("~/Documents/ComputationalFinance/GitHub/RSIBTC")
options("scipen"=10000,"getSymbols.warning4.0"=FALSE,concordance=TRUE)
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Load meXBT API from GitHub ----------------------------------------------------------------- #

meXBTAPI <- 'http://bit.ly/meXBTRAPI'
downloader::source_url(meXBTAPI,prompt=FALSE,quiet=TRUE)

# -- Load the R Data Processor from GitHub ------------------------------------------------------ #

PRC <- "http://bit.ly/RDataProcessor"
downloader::source_url(PRC,prompt=FALSE,quiet=TRUE)

# -- Download Bitcoin prices from meXBT --------------------------------------------------------- #

BtcPair  <- "btcmxn"
interval <- "hours"
BtcPair  <- OHLC(BtcPair,1320,interval)

BtcPair <- BtcPair[-which(BtcPair$High.Price == 100000),]
BtcPair <- BtcPair[-which(BtcPair$High.Price == 9999999.00),]

Count    <- length(BtcPair[,1])
limvald  <- length(BtcPair[,1])
limentd  <- round((length(BtcPair[,1]))*.99,0)
TrainPrices <- data.frame(BtcPair[1:limentd,])
ValPrices   <- data.frame(BtcPair[(limentd+1):limvald,])
meXBTMxn <- xts(TrainPrices[,c(2,3,4,5,9)], order.by = TrainPrices[,1])
colnames(meXBTMxn) <- c("Open","High","Low","Close","Volume")

IPC <- OYStockD1("^IPC","yahoo",Sys.Date()-1000,Sys.Date())
IPCRet <- Return.calculate(xts(IPC$Adj.Close, order.by = IPC$TimeStamp))
IPCRet <- IPCRet[-1,]

# -- Partial Auto Correlation Function as trading Parameter ------------------------------------- #

FACP   <- AutoCorrelation(BtcPair[,4], "partial", "Partial Auto Correlation")
MaxLag <- max(which(FACP$data$Sig_nc == max(FACP$data$Sig_nc)))
