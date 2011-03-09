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

# seleziona i contratti dopo il settembre 2005
toSelect <- as.Date(settlementDates) >= as.Date("2005-10-19")
contracts <- contracts[toSelect]

# estrai tutte le lastTradeDates ed i lastTradePrices
lastTradeDates  <- extractLists(contracts,fieldName="lastTradeDate")
lastTradePrices <- sapply(contracts,extractPriceAtLastTradeDate)

# estrai tutti i settlementPrices 
settlementDates <- extractLists(contracts,fieldName="settlementDate")
settlementPrices <- sapply(contracts,extractPriceAtSettlementDate)

# estrai tutti i prezzi nbPeriods 3 giorni prima del settlement
nbPeriods <- 3
result3 <- extractPriceAndDatePreviousToSettlement.df(contracts,nbPeriods,
		dateLimit="2011-02-21") 

# estrai tutti i prezzi dei futuri del mese successivo al lastTradeDate
result1 <- data.frame(priceNextContract=vector(mode="numeric",
				length=length(contracts)-1))
rownames(result1) <- lastTradeDates[-length(contracts)]
for (i in 1:(length(contracts)-1)) {
	YM <- substr(lastTradeDates[i],1,7)
	result1[i,1] <- extractPriceOfNextContract(desiredContractYM=YM,
			contracts, desiredDate=lastTradeDates[i])
}

vix <- repository[[1]]$data[lastTradeDates,1,drop=FALSE] 
rownames(vix) <- lastTradeDates

write.csv(result3$data,file="./unitTests/data/futuri_3_giorni.csv")
write.csv(result1$data,file="./unitTests/data/futuri_price_next_contract_lastTradeDate.csv")
write.csv(result1$data,file="./unitTests/data/futuri_vix_at_lastTradeDate.csv")

# determina le date 1 giorno prima del settlement





plot(repository[[1]],from="2004-01-01")
lapply(contracts,addToPlot)


























# compute a moving average of the daily Vix volatility
vixTs <- repository[[1]]$data
dates <- as.Date(rownames(vixTs))
logVixTs <- log(vixTs)
vixLogRetTs <- logVixTs[-1,,drop=FALSE] - logVixTs[-nrow(logVixTs),,drop=FALSE]

# crea la serie storica della volatilita del vix
windowLength = 20
volVix <- sqrt(rollFun(vixLogRetTs[,1], n=windowLength, FUN=var)*252)
volVix <- data.frame(volVix)
rownames(volVix) <- as.character(dates[-(1:windowLength)])

plot(dates[-(1:windowLength)],volVix[[1]],type="l")
plot(as.Date(lastTradeDates),volVix[lastTradeDates,1],type="l")
plot(vixTs[lastTradeDates,1],volVix[lastTradeDates,1],type="l")

contango <- 1

