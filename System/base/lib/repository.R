# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_repositoryPoliticaInvestimento <- function(politicaInvestimento.df) {
	
	repository <- list()
	class(repository) <- "repositoryPoliticaInvestimento"
	
	if (missing(politicaInvestimento.df)) {
		connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
		query =paste("SELECT B.ID, A.MonetaInvestimento",
				"FROM [Sistema (prova)].dbo.DBPoliticaInvestimento A ",
				"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B on A.Cliente = B.Cliente")

		repository$politicaInvestimento.df <- sqlQuery(connection,query,as.is=TRUE)
	} else {
		repository$politicaInvestimento.df <- politicaInvestimento.df
	}
	
	return(repository)
}

create_repositoryInstruments <- function(instruments.df) {
	# instruments.df: a data frame with <ID (integer)>, <Instrument (character)>
	
	repository <- list()
	class(repository) <- "repositoryInstruments"
	
	if (missing(instruments.df)) {
		# crea un data.frame con <ID,Strumento>
		connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
		query <- "SELECT ID, Strumento AS Instrument FROM [Sistema (prova)].dbo.DBStrumenti"
		instruments.df <- sqlQuery(connection,query,as.is=TRUE)
	}
	
	if (is.null(nrow(instruments.df))) {
		print(instruments.df)
		isEmpty <- TRUE
	} else {
		isEmpty <- FALSE
		
		# cambia il nome da Azioni a equities
		toChange <- instruments.df[,"Instrument"] == "Azioni"
		if (any(toChange)) instruments.df[toChange,"Instrument"] <- "Equity"

		# cambia il nome da Obbligazioni a bond
		toChange <- instruments.df[,"Instrument"] == "Obbligazioni"
		if (any(toChange)) instruments.df[toChange,"Instrument"] <- "Bond"
		
		# sostituisci tutti gli spazi con "_"
		instruments.df[,"Instrument"] <- gsub(" ","_",instruments.df[,"Instrument"])
		rm(toChange)
	}
	
	repository$instruments.df <- instruments.df
	
	repository$getInstrumentName <- function(id) {
		if (isEmpty) return(NA_character_)
		
		if (is.na(id)) return("Conto_corrente")
		isDesired <- instruments.df[,"ID"] == as.integer(id)
		
		if (any(isDesired)) return(instruments.df[isDesired,"Instrument"])
		
		return(NA_character_)
	}
	
	repository$getId <- function(instrument) {
		if (isEmpty) return(NA_integer_)
		isDesired <- instruments.df[,"Instrument"] == instrument
		if (any(isDesired)) return(instruments.df[isDesired,"ID"])
		return(NA_integer_)
	}
	return(repository)
}



create_repositoryExchangeRates <- function(exchangeRatesDate) {
	
	repository <- new.env()
	class(repository) <- "repositoryExchangeRates"
	
	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	if (missing(exchangeRatesDate)) {
		# crea un data.frame con <Moneta,CHFPar>
		query <- paste("SELECT Moneta, CHFPar",
				"FROM [Sistema (prova)].dbo.Cambi"
		)
		rates.df <- sqlQuery(connection,query,as.is=c(TRUE,FALSE))
		
		if (nrow(rates.df)==0) {
			print("Repository exchange rates empty!")
			stop()
		}
	} else {
		for (i in 1:15) {
			# crea un data.frame con <Moneta,CHFPar>
			query <- paste("SELECT Moneta, ParCHF AS CHFPar ",
					"FROM [Cambi storici].dbo.vista_per_cambi_storici ",
					"WHERE [Date] ='",exchangeRatesDate,"'",
					sep=""
			)
			rates.df <- sqlQuery(connection,query,as.is=c(TRUE,FALSE))
			
			if (nrow(rates.df)>0) {
				break
			}
			exchangeRatesDate <- as.character(as.Date(exchangeRatesDate) - 1)
		}
		if (nrow(rates.df)==0) {
			print("Repository exchange rates empty!")
			stop()
		}
	}
	
	# sostituisci i possibili NA
	if (!is.element("CHr",rates.df[,"Moneta"])) rates.df <- rbind(rates.df,data.frame(Moneta="CHr",CHFPar=1.0))
	
	repository$rates <- rates.df[,"CHFPar"]
	names(repository$rates) <- rates.df[,"Moneta"]
	
	# create the lastUpdate wich is a vector of date-time 
	dateTime <- if(missing(exchangeRatesDate)) Sys.time() else as.POSIXt(exchangeRatesDate)
	repository$lastUpdateDateTime <- rep(dateTime,nrow(rates.df))
	names(repository$lastUpdateDateTime) <- rates.df[,"Moneta"]
	
	rm(rates.df,dateTime)		
	
	repository$exchange <- function(money,toCurrency) {
		# money: an Object of class Money containing the amount to be converted
		# toCurrency: the final currency (of Class "Currency")
	
		amount <- money@amount
		fromCurrency <- money@currency
		
		if (identical(toCurrency,new("Currency","CHF")) | identical(toCurrency,new("Currency","CHr"))) {
			result <- new("Money",amount=amount*repository$rates[[fromCurrency]],currency=toCurrency)
			return(result)
		}

		areAvailable <- is.element(c(fromCurrency,toCurrency),names(repository$rates))
		if (any(!areAvailable)) {
			print(paste("Error",c(fromCurrency,toCurrency)[!areAvailable],"not available."))
		}
		result <- new("Money",amount=amount * repository$rates[[fromCurrency]] / repository$rates[[toCurrency]],
				currency=toCurrency)
		return(result)
	}
	
	repository$update <- function(values,dateTimes) {
		# values is a named vector with exchange rates defined with respect to
		# CHF, i.e. EUR = 1.20 CHF (in the Bloomberg convention EURCHF)
		
		names <- names(values)
	
		areNewer <- repository$lastUpdateDateTime[names] < dateTimes[names]
		if (any(areNewer)) {
			repository$lastUpdateDateTime[names[areNewer]] <- dateTimes[names[areNewer]]
			repository$rates[names[areNewer]] <- values[names[areNewer]]		
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	return(repository)
}


create_testRepositoryExchangeRates <- function(exchangeRates.v) {
	# exchangeRates.v: a named vector of exchangeRates
	
	repository <- new.env()
	class(repository) <- "repositoryExchangeRates"
	
	repository$rates <- exchangeRates.v
	
	# create the lastUpdate wich is a vector of date-time 
	dateTime <-  Sys.time() 
	repository$lastUpdateDateTime <- rep(dateTime,length(repository$rates))
	names(repository$lastUpdateDateTime) <- names(repository$rates)
	
	rm(dateTime)		
	
	repository$exchange <- function(money,toCurrency) {
		# money: an Object of class Money containing the amount to be converted
		# toCurrency: the final currency (of Class "Currency")
		
		amount <- money@amount
		fromCurrency <- money@currency
		
		if (identical(toCurrency,new("Currency","CHF")) | identical(toCurrency,new("Currency","CHr"))) {
			result <- new("Money",amount=amount*repository$rates[[fromCurrency]],currency=toCurrency)
			return(result)
		}
		
		areAvailable <- is.element(c(fromCurrency,toCurrency),names(repository$rates))
		if (any(!areAvailable)) {
			print(paste("Error",c(fromCurrency,toCurrency)[!areAvailable],"not available."))
		}
		result <- new("Money",amount=amount * repository$rates[[fromCurrency]] / repository$rates[[toCurrency]],
				currency=toCurrency)
		return(result)
	}
	
	repository$update <- function(values,dateTimes) {
		# values is a named vector with exchange rates defined with respect to
		# CHF, i.e. EUR = 1.20 CHF (in the Bloomberg convention EURCHF)
		
		names <- names(values)
		
		areNewer <- repository$lastUpdateDateTime[names] < dateTimes[names]
		if (any(areNewer)) {
			repository$lastUpdateDateTime[names[areNewer]] <- dateTimes[names[areNewer]]
			repository$rates[names[areNewer]] <- values[names[areNewer]]		
			return(TRUE)
		} else {
			return(FALSE)
		}
	}
	
	return(repository)
}






