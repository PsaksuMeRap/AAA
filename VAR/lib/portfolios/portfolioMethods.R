# TODO: Add comment
# 
# Author: claudio
###############################################################################



explodePortfolioByFund <- function(fundData,fundPortfolios,portfolio) {
	
	# il portfolio è vuoto termina
	nbPositions <- length(portfolio$positions$positions)
	if (nbPositions==0) return(invisible())
	
	# identifica il portafoglio del fondo in questione
	owner <- fundData["owner"]
	fundPortfolio <- filterLists(fundPortfolios,by="owner",value=owner)[[1]]
	
	result <- identifyFundsToExplode(fundData,portfolio$positions)
	
	if (fundData[["nomeFondo"]]=="FIXED INCOME") {
		result_oacc <- identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc(portfolio$positions)
		result <- result & !result_oacc
	} else {
		result_oacc <- rep(FALSE,length(result))
	}
	
	# se non sono state trovati posizioni termina
	if (!any(result)) return()
	
	# salva e poi elimina la posizione dal portafoglio
	position <- portfolio$positions$positions[result]
	if (length(position)==1) {
		position <- position[[1]] 
	} else {
		stop(paste("Errore: portafoglio",portfolio$owner,"con piu' di una posizione in CB Fixed Income."))
	}
	# la precedente operazione andrebbe generalizzata al caso in cui ci siano diverse posizioni
	
	# rimuovi le posizioni relative al fondo in questione
	portfolio$positions$remove(result | result_oacc)
	
	# calcola il peso relativo della posizione del portafoglio sul NAV del fondo
	weight <- position$money$divide(fundPortfolio$value())
	
	positionTmp <- lapply(fundPortfolio$positions$positions,copyPosition)
	
	addToName <- function(position,nameToAdd) {
		position$name <- paste(position$name,"/","From",nameToAdd)
	}
	invisible(lapply(positionTmp,addToName,fundData[["nomeFondo"]]))
	
	# ripesa le posizioni del portafoglio
	invisible(lapply(positionTmp,weightPosition,weight))
	
	# aggiungi le posizioni al portafoglio
	invisible(lapply(positionTmp,portfolio$positions$add))
	
}

explodePortfolioByAllFunds <- function(portfolio,fundsDb,fundPortfolios) {
	apply(fundsDb,1,explodePortfolioByFund,fundPortfolios,portfolio)
}

explodeAllPortfoliosByAllFunds <- function(portfolios) {
	fundsDb <- create_fundsDB()
	invisible(lapply(portfolios,explodePortfolioByAllFunds,fundsDb,fundPortfolios=portfolios))
}


