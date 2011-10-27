# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RUnit")
library("fTrading")
# home <- "/home/claudio/eclipse/AAA/datiFuturiVix/"

setwd(home)

stringsAsFactors = FALSE

source("./lib/library.R")


importerVix <- create_importer(importFrom="./datiVix1.csv")
repository <- importerVix$createRepository()
rm(importerVix)

importerVixFutures <- create_importerVixFutures(file="./serie1.csv",settlementFile="./scadenze.csv")
contracts <- importerVixFutures$extractAllContracts()
rm(importerVixFutures)


# determina le lastTradeDates
settlementDates <- extractFromList(contracts,fieldName="settlementDate")
orderSettlementDates <- order(as.Date(settlementDates))
contracts <- contracts[orderSettlementDates]
settlementDates <- settlementDates[orderSettlementDates]

# seleziona i contratti dopo il settembre 2005 e fino all'ultimo contratto scaduto
toSelect <- (as.Date(settlementDates) >= as.Date("2005-10-19")) & (as.Date(settlementDates) <= as.Date("2011-05-20"))
contracts <- contracts[toSelect]

# estrai tutte le lastTradeDates ed i lastTradePrices
lastTradeDates  <- extractFromList(contracts,fieldName="lastTradeDate")
lastTradePrices <- sapply(contracts,extractPriceAtLastTradeDate)
names(lastTradePrices) <- lastTradeDates
write.csv(lastTradePrices,file="./unitTests/data/lastTradePrices.csv")


# estrai tutti i settlementPrices 
settlementDates <- extractFromList(contracts,fieldName="settlementDate")
settlementPrices <- sapply(contracts,extractPriceAtSettlementDate)
names(settlementPrices) <- settlementDates
write.csv(settlementPrices,file="./unitTests/data/settlementPrices.csv")


# estrai tutti i prezzi nbPeriods 3 giorni prima del settlement
nbPeriods <- 3
result3 <- extractPriceAndDatePreviousToSettlement.df(contracts,nbPeriods,
		dateLimit="2011-04-21") 
write.csv(result3,file="./unitTests/data/futuri_3_giorni.csv")


# estrai tutti i prezzi dei futuri del mese successivo al lastTradeDate
result1 <- data.frame(priceNextContract=vector(mode="numeric",
				length=length(contracts)-1))
rownames(result1) <- lastTradeDates[-length(contracts)]
for (i in 1:(length(contracts)-1)) {
	YM <- substr(lastTradeDates[i],1,7)
	result1[i,1] <- extractPriceOfNextContract(desiredContractYM=YM,
			contracts, desiredDate=lastTradeDates[i])
}
write.csv(result1,file="./unitTests/data/futuri_price_next_contract_lastTradeDate.csv")

# estrai tutti i prezzi dei futuri del mese successivo alla settlementDate
result1 <- data.frame(priceNextContract=vector(mode="numeric",
				length=length(contracts)-1))
rownames(result1) <- settlementDates[-length(contracts)]
for (i in 1:(length(contracts)-1)) {
	YM <- substr(settlementDates[i],1,7)
	result1[i,1] <- extractPriceOfNextContract(desiredContractYM=YM,
			contracts, desiredDate=settlementDates[i])
}
write.csv(result1,file="./unitTests/data/futuri_price_next_contract_settlementDates.csv")


vix <- repository[[1]]$data[lastTradeDates,1,drop=FALSE] 
rownames(vix) <- lastTradeDates


# estrai il vix alle settlement dates
vix <- repository[[1]]$data[settlementDates,1,drop=FALSE] 
rownames(vix) <- settlementDates
write.csv(vix,file="./unitTests/data/futuri_vix_at_lastTradeDate.csv")

















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

