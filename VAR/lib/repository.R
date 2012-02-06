# TODO: Add comment
# 
# Author: claudio
###############################################################################

create_repositoryFixedIncome <- function(fixedIncome=NULL) UseMethod("create_repositoryFixedIncome",fixedIncome)

create_repositoryFixedIncome.default <- function(fixedIncome) {
	
	repository <- list()
	class(repository) <- "repositoryFixedIncome"
	
	
	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	query = paste("Select * ",
			"FROM [Sistema (prova)].dbo.riskman_export_DBRedditoFissoConRatings")
	repository$fixedIncome.df <- sqlQuery(connection,query,as.is=TRUE)
	return(repository)
}


create_repositoryFixedIncome.data.frame <- function(fixedIncome) {
	repository <- list()
	class(repository) <- "repositoryFixedIncome"
	repository$fixedIncome.df <- fixedIncome
	return(repository)
}



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
		if (any(toChange)) instruments.df[toChange,"Instrument"] <- "equity"

		# cambia il nome da Obbligazioni a bond
		toChange <- instruments.df[,"Instrument"] == "Obbligazioni"
		if (any(toChange)) instruments.df[toChange,"Instrument"] <- "bond"
		
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


create_repositoryEquities <- function(equities.df) {
	repository <- list()
	class(repository) <- "repositoryEquityTicker"
	
	if (missing(equities.df)) {
		# crea un data.frame con <ID,Strumento>
		connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
#		query <- paste("SELECT ID as id, Azione AS equity, numeroValore, Ticker AS ticker",
#				"FROM [Sistema (prova)].dbo.DBAzioni",
#				"WHERE ID_strumento=1"
#		)
		query <- paste("SELECT DISTINCT",
		               "EquityDB.ID AS id, EquityDB.Ticker AS ticker,",
					   "CASE WHEN Company IS NULL THEN [Sistema (prova)].dbo.DBAzioni.Azione",
					   "ELSE dbo.EquityDB.Company END AS equity",
		               "FROM dbo.EquityDB LEFT OUTER JOIN",
		               "[Sistema (prova)].dbo.DBAzioni ON dbo.EquityDB.ID = [Sistema (prova)].dbo.DBAzioni.ID",
		               "WHERE (dbo.EquityDB.ID_strumento = 1)"		
		)
		repository$equities.df <- sqlQuery(connection,query,as.is=TRUE)
#verifica che la query sia riuscita (anche data.frame vuoto è ok!)
		
	} else {
		repository$equities.df <- equities.df
	}
	
#	if (is.null(nrow(repository$equities.df))) {
	if (nrow(repository$equities.df)==0) {	
		# print(repository$equities.df)
		isEmpty <- TRUE
	} else {
		isEmpty <- FALSE
	}
	
	repository$tickerFromId <- function(id) {
		
		if (isEmpty) return(NA_character_)
		
		isDesired <- repository$equities.df[,"id"] == as.integer(id)
		
		if (any(isDesired)) return(repository$equities[isDesired,"ticker"])
		
		return(NA_character_)
	}
	
	return(repository)
}



create_repositoryInterestRates <- function(date,interestRates.df) {
	repository <- list()
	class(repository) <- "repositoryInterestRates"
	
	if (missing(interestRates.df)) {
		# crea un data.frame con <currency,date,Scadenza,maturity,rate>	
		if (missing(date)) { # prendi gli ultimi tassi disponibili
			
			query <- paste("SELECT Moneta AS currency, [Date] as date, Scadenza, Scadenza1 AS maturity, Tasso_aggiustato AS rate",
					"FROM [Tassi storici (VAR)].dbo.Curva_tassi_recenti"
			)
		} else {
			query <- paste("SELECT Moneta AS currency, [Date] as date, Scadenza, Scadenza1 AS maturity, Tasso_aggiustato AS rate ",
					"FROM [Tassi storici (VAR)].dbo.TotaleTassiStorico ",
					"WHERE [Date]='",date,"'",sep=""
			)		
		}
		
		connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
		repository$rates.df <- sqlQuery(connection,query)
	} else {
		repository$rates.df <- interestRates.df
	}
	
	rownames(repository$rates.df) <- paste(repository$rates.df[,"currency"],
			repository$rates.df[,"Scadenza"],sep="")
#verifica che la query sia riuscita (anche data.frame vuoto è ok!)
#	if (is.null(nrow(repository$rates.df))) {
	if (nrow(repository$rates.df)==0) {	
		print(repository$rates.df)
		print("Error: interest rates repository is empty!")
		stop()
	}
	
	repository$allowedMonths <- function() {
		# mesi consentiti
		x <- c(0.25,0.5,0.75,1:360)
	}
	
	repository$getMonthTicker <- function(numberMonths) {
		allowedMonths <- repository$allowedMonths()
		if (!any(numberMonths==allowedMonths)) {
			print(paste("Error: invalid number of months:", numberMonths))
			stop()
		}
		if (numberMonths <  1) return(paste(round(numberMonths/0.25),"W",sep=""))
		if (numberMonths < 12) return(paste(round(numberMonths),"M",sep=""))
		return(paste(round(numberMonths)/12,"Y",sep=""))
	}
	
	repository$getRates <- function(currency,maturity) {
		
		# maturity in months, i.e. 0.25 for a week
		missingCurrency <- missing(currency)
		missingMaturity <- missing(maturity)
		if (!missingCurrency & !missingMaturity) {
			ticker <- paste(currency,repository$getMonthTicker(maturity),sep="")
			return(repository$rates.df[ticker,,drop=FALSE])
		}
		if (missingCurrency & missingMaturity) return(repository$rates.df)
		
		if (missingCurrency & !missingMaturity) {
			isDesired <- repository$rates.df[,"maturity"] == maturity
			return(repository$rates.df[isDesired,,drop=FALSE])
		}
		
		if (!missingCurrency & missingMaturity) {
			isDesired <- repository$rates.df[,"currency"] == currency
			return(repository$rates.df[isDesired,,drop=FALSE])
		}		
	}
	
	return(repository)
}

create_repositoryDiscountFactors <- function() {
	repository <- list()
	class(repository) <- "repositoryDiscountFactors"
	
	# crea un data.frame con <currency,maturity,df>
	
	repository$discountFactors.df <- data.frame(currency=character(),maturity=numeric(),df=numeric())
	
	repository$getMonthTicker <- function(numberMonths) {
		allowedMonths <- repository$allowedMonths()
		if (!any(numberMonths==allowedMonths)) {
			print(paste("Error: invalid number of months:", numberMonths))
			stop()
		}
		if (numberMonths <  1) return(paste(round(numberMonths/0.25),"W",sep=""))
		if (numberMonths < 12) return(paste(round(numberMonths),"M",sep=""))
		return(paste(round(numberMonths)/12,"Y",sep=""))
	}
	
	repository$creteEmptyDiscountFactors <- function() {
		return(data.frame(currency=character(),maturity=numeric(),df=numeric()))
	}
	
	repository$createDiscountFactors <- function(oneCurrencyRates.df) {
		# interestRates.df contiene tassi in un'unica moneta!
		
		nbInterestRates <- nrow(oneCurrencyRates.df)
		
		if (nbInterestRates==0) return(repository$creteEmptyDiscountFactors())
		
		# ordina il data.frame in ordine ascendente
		orderAscending <- order(interestRates.df[,"maturity"])
		interestRates.df <- interestRates.df[orderAscending,,drop=FALSE]
		
		# calcola le maturities inferiori o uguali all'anno
		isMoneyMarket <- oneCurrencyRates.df[,"maturity"] <= 12.0
		nbMoneyMarketRates <- sum(isMoneyMarket)
		nbCapitalMarketRates <- nbInterestRates - nbMoneyMarketRates
		
		# se c'e' un unico tasso assumi tassi costanti
		# da correggere per tener conto dei tassi a breve che si calcolano lineari
		if (nbInterestRates==1) {
			rate = oneCurrencyRates.df[,"rate"]
			myFun <- approxfun(x=c(0.25,360),y=c(rate,rate))
			minMaturity <- 0.25
			maxMaturity <- 360			
		} else {
			myFun <- approxfun(x=oneCurrencyRates.df[,"maturity"],y=oneCurrencyRates.df[,"rate"])
			minMaturity <- min(oneCurrencyRates.df[,"maturity"])
			maxMaturity <- max(oneCurrencyRates.df[,"maturity"])
		}
		
		# genera le maturities desiderate per il calcolo 
		maturities <- c(0.25,0.5,0.75,1:360)
		rates <- myFun(maturities)
		
		# elimina i tassi NA
		discard <- is.na(rates)
		maturities <- maturities[!discard]
		rates <- rates[!discard]
		rm (discard)
		
		isMoneyMarket <- maturities <= 12
		if (any(isMoneyMarket)) { 
			moneyMarketDf <- 1.0 + rates[isMoneyMarket] * maturities[isMoneyMarket] / 12
			moneyMarketDf <- 1.0 / moneyMarketDf
		}
		if (maxMaturity == 12) return(list(maturities=maturities[isMoneyMarket]))
		return(interestRates.df)
		
	}
	
	
	repository$getDiscountFactors <- function(currency,maturity) {
		
		# maturity in months, i.e. 0.25 for a week
		missingCurrency <- missing(currency)
		missingMaturity <- missing(maturity)
		if (!missingCurrency & !missingMaturity) {
			ticker <- paste(currency,repository$getMonthTicker(maturity),sep="")
			return(repository$rates.df[ticker,,drop=FALSE])
		}
		
		if (missingCurrency & missingMaturity) return(repository$rates.df)
		
		if (missingCurrency & !missingMaturity) {
			isDesired <- repository$rates.df[,"maturity"] == maturity
			return(repository$rates.df[isDesired,,drop=FALSE])
		}
		
		if (!missingCurrency & missingMaturity) {
			isDesired <- repository$rates.df[,"currency"] == currency
			return(repository$rates.df[isDesired,,drop=FALSE])
		}		
	}
	
	return(repository)
}


create_repositoryExchangeRates <- function(exchangeRates.v,exchangeRatesDate) {
	# exchangeRates.v: a named vector of exchangeRates

	repository <- new.env()
	class(repository) <- "repositoryExchangeRates"
	
	if (missing(exchangeRates.v)) {
		
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
		rm(rates.df)
		
	} else {
		repository$rates <- exchangeRates.v
	}
	
	
	repository$exchange <- function(money,toCurrency) {
		# money: a list <amount,currency> with the amount to be converted
		# toCurrency: the final currency
		
		amount <- money$amount
		fromCurrency <- money$currency
		
		if (toCurrency=="CHF" | toCurrency=="CHr") {
			result <- toMoney(amount*repository$rates[[fromCurrency]],toCurrency)
			return(result)
		}

		areAvailable <- is.element(c(fromCurrency,toCurrency),names(repository$rates))
		if (any(!areAvailable)) {
			print(paste("Error",c(fromCurrency,toCurrency)[!areAvailable],"not available."))
		}
		result <- toMoney(amount * repository$rates[[fromCurrency]] / repository$rates[[toCurrency]],
				toCurrency)
		return(result)
	}
	
	return(repository)
}

create_repositoryAssetClassMapping <- function() {	
	repository <- list()
	class(repository) <- "repositoryAssetClassMapping"
	
	filePath <- "./repositories/assetClassMapping/repositoryAssetClassMapping.csv"
	data <- read.csv(file=filePath,header=TRUE,as.is=TRUE)		
	repository$assetClassMapping.df <- data
	return(repository)
}

create_repositoryVincoliSicav <- function() {
	repository <- new.env()
	class(repository) <- "repositoryVincoli"
	
	return(repository)
}

