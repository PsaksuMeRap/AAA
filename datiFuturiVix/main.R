# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RUnit")
library("fTrading")
home <- "/home/claudio/eclipse/AAA/datiFuturiVix/"
setwd(home)

stringsAsFactors = FALSE

source("./lib/library.R")


importerVix <- create_importer()
repository <- importerVix$createRepository()
rm(importerVix)

importerVixFutures <- create_importerVixFutures()
contracts <- importerVixFutures$extractAllContracts()
rm(importerVixFutures)

# determina le lastTradeDates

settlementDates <- extractLists(contracts,fieldName="settlementDate")
orderSettlementDates <- order(as.Date(settlementDates))
contracts <- contracts[orderSettlementDates]
settlementDates <- settlementDates[orderSettlementDates]

toSelect <- as.Date(settlementDates) >= as.Date("2005-10-19")

contracts <- contracts[toSelect]


lastTradeDates  <- extractLists(contracts,fieldName="lastTradeDate")

settlementPrices <- sapply(contracts,extractPriceAtDate,dateType="settlementDate")
lastTradePrices <- sapply(contracts,extractPriceAtDate,dateType="lastTradeDate")

# compute a moving average of the daily Vix volatility
vixTs <- repository[[1]]$data
dates <- as.Date(rownames(vixTs))
logVixTs <- log(vixTs)
vixLogRetTs <- logVixTs[-1,,drop=FALSE] - logVixTs[-nrow(logVixTs),,drop=FALSE]

# crea la serie storica della volatilita del vix
windowLength = 10
volVix <- sqrt(rollFun(vixLogRetTs[,1], n=windowLength, FUN=var)*252)
volVix <- data.frame(volVix)
rownames(volVix) <- as.character(dates[-(1:windowLength)])

plot(dates[-(1:windowLength)],volVix[[1]],type="l")
plot(as.Date(lastTradeDates),volVix[lastTradeDates,1],type="l")
plot(vixTs[lastTradeDates,1],volVix[lastTradeDates,1],type="l")

contango <- 1

