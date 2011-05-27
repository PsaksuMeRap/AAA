# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_dsCodeParser <- function() {
	parser <- new.env()
	class(parser) <- "dsCodeParser"
	
	
	parser$extractContractName <- function(dsCode) {
		# legge e parsa la linea di datastream denominata code 
		# (che contiene il codice datastream con il datatype tra parentesi)
		# e ritorna una lista di attributi con nameWithoutType, dataType, year, month

		# Esempio: da "CVX0311(OI)" viene estratto CVX0311, OI, 2011, 03
		
		nameWithoutType <- substr(dsCode,1,7) 
		month <- substr(dsCode,4,5) 
		year  <- substr(dsCode,6,7)
		dataType <- substr(dsCode,9,10)
		
		attributes <- list(nameWithoutType=nameWithoutType,
				dataType=dataType,
				year=paste("20",year,sep=""),
				month=month)
		return(attributes)
	}
	
	parser$extractNameWithoutType <- function(code) {
		# estrae il codice datastream dalla linea di datastream denominata code 
		# (che contiene il codice datastream con il datatype tra parentesi)
		# e ritorna il puro codice datastream 
		
		# Esempio: da "CVX0311(OI)" viene estratto CVX0311
		
		attributes <- parser$extractContractName(code)
		nameWithoutType <- attributes$nameWithoutType
		return(nameWithoutType)
	}
	
	return(parser)
}
