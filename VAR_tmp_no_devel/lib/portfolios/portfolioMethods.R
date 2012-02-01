# TODO: Add comment
# 
# Author: claudio
###############################################################################



explodePortfolioByFund <- function(fundData,fundPortfolios,portfolio) {
	# fundData: una lista contenente i 4 campi $nomeFondo, $instrumentClass, $id e $owner,
	#           Esempio: "FIXED INCOME", "Fondi_obbligazionari", "2490099", "pippo76"
	# fundPortfolios: una lista con i portafogli dei fondi
	# portfolio: il portafoglio le cui posizioni sono da esplodere
	
	# determina il numero di posizioni in portafoglio
	nbPositions <- length(portfolio$positions$positions)
	
	# se il portfolio è vuoto termina
	if (nbPositions==0) return(invisible())
	
	# identifica il portafoglio del fondo in questione
	owner <- fundData["owner"]
	fundPortfolio <- filterLists(fundPortfolios,by="owner",value=owner)[[1]]

	# seleziona le posizione che corrispondono al fondo in fundData. Per
	# CB_Accent_Lux_sicav_FIXED_INCOME la posizione accruedInterest non è considerata!
	result <- identifyFundsToExplode(fundData,portfolio$positions)
	
	# se non sono state trovati posizioni termina
	if (!any(result)) return()
	
	# salva le posizioni da esplodere
	positions <- portfolio$positions$extract(result)

	# identifica le posizioni accruedInterest di CB_Accent_Lux_sicav_FIXED_INCOME
	# ed aggiungile a quelle gia' da eliminare
	if (fundData[["nomeFondo"]]=="FIXED INCOME") {
		result_oacc <- identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc(portfolio$positions)
		result <- result | result_oacc
	}
	
	# rimuovi le posizioni relative al fondo in questione
	portfolio$positions$remove(result)
	
	# calcola il peso relativo della posizione del portafoglio sul NAV del fondo

	addToPosition <- function(position,nameToAdd) {
		position$explodeString <- paste("From",nameToAdd)
	}
	
	
	for (position in positions$positions) {
		
		weight <- position$money$divide(fundPortfolio$value())
		
		positionTmp <- lapply(fundPortfolio$positions$positions,copyPosition)
		
		invisible(lapply(positionTmp,addToPosition,fundData[["nomeFondo"]]))
		
		# ripesa le posizioni del portafoglio
		invisible(lapply(positionTmp,weightPosition,weight))
		
		# aggiungi le posizioni al portafoglio
		invisible(lapply(positionTmp,portfolio$positions$add))
		rm(positionTmp)
		
	}	
}

explodePortfolioByAllFunds <- function(portfolio,fundsDb,fundPortfolios) {
	apply(fundsDb,1,explodePortfolioByFund,fundPortfolios,portfolio)
}

explodeAllPortfoliosByAllFunds <- function(portfolios) {
	fundsDb <- create_fundsDB()
	invisible(lapply(portfolios,explodePortfolioByAllFunds,fundsDb,fundPortfolios=portfolios))
}

extractUnconsistentPortfolioPositions <- function(portfolio) {
	
	isPositionConsistent <- areConsistent(portfolio$positions)
	
	if (is.null(isPositionConsistent) | all(isPositionConsistent)) {
		inconsistentPositions <- list()
	} else {
		inconsistentPositions <- portfolio$positions$positions[!isPositionConsistent]
	}
	
	output <- list()
	output[[portfolio$owner]] <- inconsistentPositions
	return(output)
}
