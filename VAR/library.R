# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_portfolio <- function() {
	# uses positions
	
	portfolio <- new.env()
	class(portfolio) <- "portfolio"
	
	portfolio$owner <- NA_character_
	portfolio$refCurrency <- NA_character_
	
	portfolio$positions <- create_positions()
	
	return(portfolio)
}


create_positions <- function() {
	positions <- new.env()
	class(positions) <- "positions"
	
	positions$positions <- list()
	
	positions$addPosition <- function(pos) {
		# pos: a position
		positions$positions[[length(positions$positions)+1]] <<- pos
	}
	
	positions$addPositions <- function(pos) {
		# pos: an onbject of class positions
		lapply(pos$positions,positions$addPosition)
	}
	
	positions$remove <- function(index) {
		if (missing(index)) index <- length(positions$positions)
		positions$positions[[index]] <<- NULL
	}
	return(positions)
}


create_position <- function() {
	position <- new.env() # list()
	class(position) <- "position"
	
	position$name = NA_character_
	position$currency = NA_character_
	position$amount = NA_real_
	position$origin = NA
	
	position$create <- function(name=NA_character_,currency="CHF",
			amount=0.0,origin=NA) {
		position$name <<- name
		position$currency <<- currency
		position$amount <<- amount
		position$origin <<- origin
	}
	
	position$print <- function() {
		print(position$name)
		print(position$currency)
		print(position$amount)
	}
	
	position$isMemberOf <- function(myClass) {
		classes <- class(position)
		if (any(classes == myClass)) return(TRUE) else return(FALSE)
	}
	
	position$extendEquities <- function() {
		# create the repository of the equities if not available
		if (!exists("equities",envir=repositories)) {
			eval(expression(equities <- create_repositoryEquities())
					,env=repositories)
		}
		
		# identify the ticker of the equity
		id <- position$origin[["ID_AAA"]]
		if (is.na(id)) {print("Errore: azione senza ID_AAA"); stop()}
		id <- as.numeric(id)
		
		position$ticker <<- repositories$equities$tickerFromId(id)		
		return()		
		
	}
	return(position)
}


create_riskFactors <- function(position) {
	riskFactors <- data.frame(amount=NA_real_,factor=NA_character_)
	class(riskFactors) <- "riskFactors"
	
	add <- function(amount,factor) {
		riskFactors <<- rbind(riskFactors,data.frame(amount,factor))
	}    
	
	if (position$isMemberOf("Conto corrente")) {
		riskFactors <- add(amount=position$amount,factor=position$currency)
	}
	if (position$isMemberOf("equities")) {
		riskFactors <- add(amount=position$amount,factor=position$currency)
		riskFactors <- add(amount=position$amount,factor=position$ticker)
	}
	
	return(riskFactors)
}


DBPortfolioGeneraleLoader <- function() {
	connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
	query = "SELECT * FROM [Sistema (prova)].dbo.DBPortfolioGenerale"
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	return(DBPortfolioGenerale.df)
}


create_parserPortfolio <- function() {
	parser <- new.env()
	class(parser) <- "parserPortfolio"
	
	parser$parse <- function(owner,dati.df) {
		# crea il portafoglio
		portfolio <- create_portfolio()
		portfolio$owner <- owner
		
		# determina la moneta di riferimento del cliente
		query = paste("SELECT MonetaInvestimento ",
				"FROM [Sistema (prova)].dbo.DBPoliticaInvestimento ",
				"WHERE Cliente = '",owner,"'",sep="")
		refCurrency.df <- sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		
		if (nrow(refCurrency.df)==0) {
			print(paste("Il cliente",owner,"non esiste nella banca dati."))
			print("Impossibile determinare la moneta di investimento.")
			return(portfolio)
		} else {
			portfolio$refCurrency <- refCurrency.df[1,"MonetaInvestimento"]
		}
		
		# determina le posizioni dell'owner desiderato
		isDesiredOwner <- dati.df[,"Cliente"] == owner
		desiredDati.df <- dati.df[isDesiredOwner,]
		
		# inizializza e parsa le posizioni
		parserPositions <- create_parserPositions()
		positions <- parserPositions$parse(desiredDati.df)
		
		portfolio$positions$addPositions(positions)
		return(portfolio)
	}
	
	return(parser)
	
}


create_parserPositions <- function() {
	parser <- list()
	class(parser) <- "parserPositions"
	
	parser$parse <- function(dati.df) {
		# inizializza il parser della posizione
		parserPosition <- create_parserPosition()
		
		# crea una lista di posizioni parsate
		positions.list <- apply(X=dati.df,
				MARGIN=1,
				FUN=parserPosition$parse
		)
		
		# crea un oggetto di classe posizioni
		positions <- create_positions()
		
		# inserisci tutte le posizioni parsate
		lapply(positions.list,positions$addPosition)
		return(positions)
	}
	
	return(parser)
}


create_parserPosition <- function() {
	
	parser <- list()
	class(parser) <- "parserPosition"
	
	parser$identifyInstrument <- function(record) {
		# create the repository of the instruments if not available
		if (!exists("instruments",envir=repositories)) {
			eval(expression(instruments <- create_repositoryInstruments())
					,env=repositories)
		}
		
		instrument <- repositories$instruments$getInstrumentName(record["ID_strumento"])
		return(instrument)
	}
	
	parser$parse <- function(record) {
		
		position <- create_position()
		position$create(
				name=record["Nome"],
				currency=record["Moneta"],
				amount=as.real(record["ValorePosizione"]),
				origin=record
		)
		
		instrument <- parser$identifyInstrument(record)	
		class(position) <- c(instrument,class(position))
		
		# extend position if necessary
		if (instrument == "equities") position$extendEquities()
		
		return(position)
	}
	
	return(parser)
}


create_repositoryInstruments <- function() {
	repository <- list()
	class(repository) <- "repositoryInstruments"
	
	# crea un data.frame con <ID,Strumento>
	connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
	query <- "SELECT * FROM [Sistema (prova)].dbo.DBStrumenti"
	instruments.df <- sqlQuery(connection,query,as.is=TRUE)
	
	if (is.null(nrow(instruments.df))) {
		print(instruments.df)
		isEmpty <- TRUE
	} else {
		isEmpty <- FALSE
		colnames(instruments.df) <- c("ID","Instrument")
		
		# cambia il nome da Azioni a equities
		toChange <- instruments.df[,"Instrument"] == "Azioni"
		if (any(toChange)) instruments.df[toChange,"Instrument"] <- "equities"
		rm(toChange)
	}
	
	repository$getInstrumentName <- function(id) {
		if (isEmpty) return(NA_character_)
		
		if (is.na(id)) return("Conto corrente")
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
		connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
		query <- paste("SELECT ID as id, Azione AS equity, numeroValore, Ticker AS ticker",
				"FROM [Sistema (prova)].dbo.DBAzioni",
				"WHERE ID_strumento=1"
		)
		repository$equities.df <- sqlQuery(connection,query,as.is=TRUE)
#verifica che la query sia riuscita (anche data.frame vuoto è ok!)

	} else {
		repository$equities.df <- equities.df
	}
	
#	if (is.null(nrow(repository$equities.df))) {
	if (nrow(repository$equities.df)==0) {	
		print(repository$equities.df)
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
		
		connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
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


create_repositoryExchangeRates <- function() {
	repository <- new.env()
	class(repository) <- "repositoryExchangeRates"
	
	# crea un data.frame con <ID,Strumento>
	connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
	query <- paste("SELECT Moneta, CHFPar",
			"FROM [Sistema (prova)].dbo.Cambi"
	)
	
	rates.df <- sqlQuery(connection,query,as.is=c(TRUE,FALSE))
	
	if (is.null(nrow(rates.df))) {
		print("Repository exchange rates empty!")
		stop()
	}
	
	repository$rates <- rates.df[,"CHFPar"]
	names(repository$rates) <- rates.df[,"Moneta"]
	
	repository$exchange <- function(amount,fromCurrency,toCurrency) {
		if (toCurrency=="CHF" | toCurrency=="CHr") {
			return(amount * repository$rates[fromCurrency])
		}
		
		areAvailable <- is.element(c(fromCurrency,toCurrency),names(repository$rates))
		if (any(!areAvailable)) {
			print(paste("Error",names(repository$rates)[!areAvailable],"not available."))
		}
		
		return(amount * repository$rates[fromCurrency] / repository$rates[toCurrency])
	}
	
	rm(rates.df)
	return(repository)
}


create_repositoryVincoliSicav <- function() {
	repository <- new.env()
	class(repository) <- "repositoryVincoli"
	
	return(repository)
}




create_investmentPolicyAttributes <- function() {
	attributes <- list()
	class(attributes) <- "investmentPolicyAttributes"
	
	# Lo strumento � trattato in una borsa valori
	attributes[["a1.a"]] <- FALSE
	
	# La borsa � riconosciuta valida 
	attributes[["a1.b"]] <- list(exchangeName=NA_character_,isValid=FALSE)
	
	# E' nuova emisione?
	attributes[["a3.a"]] <- FALSE
	
	# Se a3.a � vera, soddisfa la condizione a.3?
	attributes[["a3.b"]] <- FALSE
	
	# E' metallo prezioso o certificato su metallo prezioso?
	attributes[["a.tuttavia.3"]] <- TRUE
	
	# Identificativo emittente
	attributes[["e.emittente"]] <- NA_character_
	
	# E' valore mobiliare?
	attributes[["e.isValoreMobiliare"]] <- TRUE
	
	# E' strumento di mercato monetario?
	attributes[["e.isMoneyMarket"]] <- TRUE
	
	
	return(attributes)
}



