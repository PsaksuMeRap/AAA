# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RUnit")
library("fTrading")

home <- "/home/claudio/workspace/AAA/datiFuturiVix/"
#home <- getwd()
setwd(home)


stringsAsFactors = FALSE

source("./lib/library.R")


importerVix <- create_importer(importFrom="./datiVix2.csv")
repository <- importerVix$createRepository()
rm(importerVix)

importerVixFutures <- create_importerVixFutures(file="./serie2.csv",settlementFile="./scadenze.csv")
future_vix_contracts <- importerVixFutures$extractAllContracts()
rm(importerVixFutures)


# determina le lastTradeDates
settlementDates <- extractFromList(future_vix_contracts,fieldName="settlementDate")
orderSettlementDates <- order(as.Date(settlementDates))
future_vix_contracts <- future_vix_contracts[orderSettlementDates]
settlementDates <- settlementDates[orderSettlementDates]

# seleziona i contratti dopo il settembre 2005 e fino all'ultimo contratto scaduto
#toSelect <- (as.Date(settlementDates) >= as.Date("2005-10-19")) & (as.Date(settlementDates) <= as.Date("2011-10-24"))
toSelect <- (as.Date(settlementDates) >= as.Date("2005-10-19"))
future_vix_contracts <- future_vix_contracts[toSelect]

# estrai tutte le lastTradeDates ed i lastTradePrices
lastTradeDates  <- extractFromList(future_vix_contracts,fieldName="lastTradeDate")
lastTradePrices <- sapply(future_vix_contracts,extractPriceAtLastTradeDate)
names(lastTradePrices) <- lastTradeDates
write.csv(lastTradePrices,file="./output/futuri_vix_at_lastTradePrices.csv")


# estrai tutti i settlementPrices 
settlementDates <- extractFromList(future_vix_contracts,fieldName="settlementDate")
settlementPrices <- sapply(future_vix_contracts,extractPriceAtSettlementDate)
names(settlementPrices) <- settlementDates
write.csv(settlementPrices,file="./output/futuri_vix_at_settlementPrices.csv")


# estrai tutti i prezzi nbPeriods 1 giorni prima del settlement

nbPeriods <- 1
result1 <- extractPriceAndDatePreviousToSettlement.df(future_vix_contracts,nbPeriods,
		dateLimit="2011-10-19") 
write.csv(result1$data,file="./output/futuri_vix_1_giorni.csv")

# estrai tutti i prezzi nbPeriods 2 giorni prima del settlement
nbPeriods <- 2
result2 <- extractPriceAndDatePreviousToSettlement.df(future_vix_contracts,nbPeriods,
		dateLimit="2011-10-19") 
write.csv(result2$data,file="./output/futuri_vix_2_giorni.csv")

# estrai tutti i prezzi nbPeriods 3 giorni prima del settlement
nbPeriods <- 3
result3 <- extractPriceAndDatePreviousToSettlement.df(future_vix_contracts,nbPeriods,
		dateLimit="2011-10-19") 
write.csv(result3$data,file="./output/futuri_vix_3_giorni.csv")


# estrai tutti i prezzi dei futuri del mese successivo al lastTradeDate
result1 <- data.frame(priceNextContract=vector(mode="numeric",
				length=length(future_vix_contracts)-1))
rownames(result1) <- lastTradeDates[-length(future_vix_contracts)]
for (i in 1:(length(future_vix_contracts)-1)) {
	YM <- substr(lastTradeDates[i],1,7)
	result1[i,1] <- extractPriceOfNextContract(desiredContractYM=YM,
			future_vix_contracts, desiredDate=lastTradeDates[i])
}
write.csv(result1,file="./output/futuri_vix_price_next_contract_lastTradeDate.csv")

# estrai tutti i prezzi dei futuri del mese successivo alla settlementDate
result1 <- data.frame(priceNextContract=vector(mode="numeric",
				length=length(future_vix_contracts)-1))
rownames(result1) <- settlementDates[-length(future_vix_contracts)]
for (i in 1:(length(future_vix_contracts)-1)) {
	YM <- substr(settlementDates[i],1,7)
	result1[i,1] <- extractPriceOfNextContract(desiredContractYM=YM,
			future_vix_contracts, desiredDate=settlementDates[i])
}
write.csv(result1,file="./output/futuri_vix_price_next_contract_settlementDates.csv")

# estrai il vix alle lastTrade dates
vix <- repository[[1]]$data[lastTradeDates,1,drop=FALSE] 
rownames(vix) <- lastTradeDates
write.csv(vix,file="./output/vix_at_lastTradeDate.csv")

# estrai il vix alle settlement dates
vix <- repository[[1]]$data[settlementDates,1,drop=FALSE] 
rownames(vix) <- settlementDates
write.csv(vix,file="./output/vix_at_settlementDate.csv")


# estrai tutte le date disponibili
desiredDates <- rownames(repository[[1]]$data)

tmp <- lapply(desiredDates,aligneFutureContractsAtDate,future_vix_contracts,2,0)
serieAllineate.df <- data.frame(Date=character(0),Price1=numeric(0),Price2=numeric(0),Switch=logical(0))
for (i in tmp) serieAllineate.df <- rbind(serieAllineate.df,as.data.frame(i))
write.csv(serieAllineate.df,file="./output/serie_allineate.csv")


# allinea le serie rispetto alla lastTradeDate
tmp <- lapply(desiredDates,aligneFutureContractsAtDate,future_vix_contracts,2,1)
serieAllineate.df <- data.frame(Date=character(0),Price1=numeric(0),Price2=numeric(0),Switch=logical(0))
for (i in tmp) serieAllineate.df <- rbind(serieAllineate.df,as.data.frame(i))
dateNomi <- as.character(serieAllineate.df[[1]])
rownames(serieAllineate.df) <- dateNomi
serieAllineate.df[[1]] <- repository[[1]]$data[dateNomi,1]
colnames(serieAllineate.df)[1] <- "Vix"
write.csv(serieAllineate.df,file="./output/serieAllineate_ltd.csv")

# allinea le serie rispetto alla settlementDate
tmp <- lapply(desiredDates,aligneFutureContractsAtDate,future_vix_contracts,2,1)
serieAllineate.df <- data.frame(Date=character(0),Price1=numeric(0),Price2=numeric(0),Switch=logical(0))
for (i in tmp) serieAllineate.df <- rbind(serieAllineate.df,as.data.frame(i))
dateNomi <- as.character(serieAllineate.df[[1]])
rownames(serieAllineate.df) <- dateNomi
serieAllineate.df[[1]] <- repository[[1]]$data[dateNomi,1]
colnames(serieAllineate.df)[1] <- "Vix"
write.csv(serieAllineate.df,file="./output/serieAllineate_std.csv")






# determina le date 1 giorno prima del settlement

plot(repository[[1]],from="2004-01-01")
lapply(future_vix_contracts,addToPlot)

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

