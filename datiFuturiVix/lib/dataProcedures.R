# TODO: Add comment
# 
# Author: ortellic
###############################################################################


aligneFutureContractsAtDate <- function(date,orderedFutureList,nbDesiredFutures=1,nbBusDaysBefSettlement=1) {
	# questa funzione prende una lista ordinata per data crescente
	# di contratti di classe "Future" e ritorna il prezzo
	# del contratto con scadenza pi� vicina alla 
	# data desiderata. Se nbDesiredFutures>1 viene restituito anche il prezzo
	# del secondo, terzo, ... contratto pi� vicino.
	# date: la data desiderata in formato testo
	# futureList: una lista di elementi di classe "Future"
	# nbDesiredFutures: quante scadenze si desiderano avere
	# nbBusDaysBefSettlement: la data intesa come il numero di giorni lavorativi che 
	# precedono la settlemetDate in cui commutare al successivo contratto
	# future. Di default si cambia scadenza al lastTradeDate
	
	# seleziona i contratti non ancora scaduti, ovvero quelli la cui settlement date
	# viene dopo "date"
	
	if (nbBusDaysBefSettlement>22 | nbBusDaysBefSettlement<0) {
		stop(paste("nbBusDaysBefSettlement is invalid:",nbBusDaysBefSettlement))
	}
	
	settlementDates <- extractFromList(orderedFutureList,fieldName="settlementDate")
	if (nbBusDaysBefSettlement>0) {
		criteriumDate <- determineBusinessDate(date,nbBusDaysBefSettlement)
		toExtract <- as.Date(settlementDates) >= as.Date(criteriumDate)
	} else {
		toExtract <- as.Date(settlementDates) >= as.Date(date)
	}
	
	
	if (any(toExtract)) {
		contracts <- orderedFutureList[toExtract]
		nbAvailableContracts <- length(contracts)
		
		# determina se ci troviamo nell'ultimo giorno prima di cambiare contratto
		criteriumDate <- determineBusinessDate(contracts[[1]]$settlementDate,-nbBusDaysBefSettlement)
		if (criteriumDate==date) switch <- 1 else switch <- 0
		
		# scrivi i risultati
		result <- list(Date=date)
		for (i in 1:min(nbAvailableContracts,nbDesiredFutures)) {
			result[paste("Price",i,sep="")] <- extractPriceAtSpecificDate(contracts[[i]],date)
		}

		# verifica che ci siano contratti a sufficienza
		if (nbDesiredFutures>nbAvailableContracts) {
			for (i in (nbAvailableContracts+1):nbDesiredFutures) {
				result[paste("Price",i,sep="")] <- NA_real_
			}
		}		
	} else {
		result <- list(Date=date)
		for (i in 1:nbDesiredFutures) {
			result[paste("Price",i,sep="")] <- NA_real_
		}
		switch <- 0
	}
	result[["Switch"]] <- switch
	return(result)
}
