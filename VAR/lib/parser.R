# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/portfolio.R")
source("./lib/position/position.R")


create_parserPortfolio <- function() {
	parser <- new.env()
	class(parser) <- "parserPortfolio"
	
	parser$parse <- function(owner,dati.df) {
		# owner: una stringa col nome del proprietario
		# dati.df: il data.frame proveniente da DBPortfolioGenerale
		
		# crea il portafoglio
		portfolio <- create_portfolio()
		portfolio$owner <- owner
		
		# determina la moneta di riferimento del cliente
		query = paste("SELECT A.MonetaInvestimento ",
				"FROM [Sistema (prova)].dbo.DBPoliticaInvestimento A ",
				"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B on A.Cliente = B.Cliente ",
				"WHERE B.ID = '",owner,"'",sep="")
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
		if (!exists("instruments",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments <- create_repositoryInstruments())
					,env=repositories)
		}
		
		instrument <- repositories$instruments$getInstrumentName(record["ID_strumento"])
		if (is.na(instrument)) {
			print(record["ID_strumento"])
			msg <- paste("Attenzione: lo strumento di ID",
					record["ID_strumento"],"non esite!")
			stop(msg)
		}
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
		
		if (position$origin[1,"Strumento"]=="Oacc") {
			class(position) <- c("accruedInterest",class(position))
		}
		
		# extend position if necessary
		extendPosition(position)
		
		return(position)
	}
	
	return(parser)
}


create_parserSelectionCriteria <- function() {
	
	parser <- new.env()
	class(parser) <- "parserSelectionCriteria"
	# checkString = factorString & factorString & ... 
	#			  + factorString & factorString & ...
	#			  + factorString & factorString & ...
	#			  ; constraintString
	
	parser$splitCheckString <- function(checkString) {
		#checkString: a string like "instrument:equity & currency:CHF + instrument:bond ; <=3%"
		# i.e. the union of a selectionString and constraintString, separated by a semicolon
		result <- unlist(strsplit(checkString,";"))
		
		if (length(result)==0) stop("Error: empty checkString")
		result <- removeStartEndSpaces(result)
		
		if (length(result)==1)  {
			x <- list(selectionString=result[[1]],criteriumCheck=NA)	
		} else {
			result2 <- parser$constructCriteriumCheck(result[[2]])
			x <- list(selectionString=result[[1]],criteriumCheck=result2)		
		}
		
		return(x)
	}
	
	parser$splitSelectionString <- function(selectionString) {
		# selectionString: a string like "instrument:equity & currency:CHF + instrument:bond"
		# where factorsStrings are joined by the "+" character
		result <- unlist(strsplit(selectionString,"\\+"))
		result <- removeStartEndSpaces(result)
		result <- lapply(result,parser$splitFactorsString)
		return(result)
	}
	
	parser$splitFactorsString <- function(factorsString) {
		# factorsString: a string like "instrument:equity & currency:CHF"
		# where one or more factorString are concatenated with the "&" character
	    result <- unlist(strsplit(factorsString,"\\&"))
		result <- removeStartEndSpaces(result)
		
		result <- lapply(result,parser$splitFactorString)
		return(result)
	}
	
	parser$splitFactorString <- function(factorString) {
		# factorString: a string like "instrument:equity,bond" or
		# "amount:<= 50 USD"
		result <- unlist(strsplit(factorString,":"))
		if (length(result)<2) stop(paste("Error: splitFactorString should return 2 blocks.",
							factorString))
		result <- removeStartEndSpaces(result)
		factor <- result[1]
		values <- removeStartEndSpaces(unlist(strsplit(result[2],",")))
		
		if (factor=="amount") {
			criteriumCheck <- parser$constructCriteriumCheck(values)
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values,
					criteriumCheck=criteriumCheck
			)
		} else {
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values
			)		
		}

		return(criteriumSelection)
	}
	
	parser$constructCriteriumCheck <- function(constraintString) {
		# a constraintString with the quantitative constraint to identify
		# "=0USD" or "> 1.77 EUR" or < 5% 
	
		string <- removeStartEndSpaces(constraintString)
		
		# check for operator
		start = 1
		if (grepl("^>=|<=|!=", string)) {
			stop=2
		} else {
			if (grepl("^>|<|=", string)) {
				stop=1
			} else {
				stop(paste("Error: missing operator:",string))
			}
		}
		operator <- substr(string,start,stop)
		
		string <- substr(string,stop+1,nchar(string))
		string <- removeStartEndSpaces(string)
		
		# check for currency
		if (grepl("[A-Z]{3}$", string)) {
			stop  = nchar(string)
			start = stop - 2
			
			kind = "absolute"
			currency <- substr(string,start,stop)
			string <- substr(string,1,start-1)
			amount <- as.numeric(string)
			money <- toMoney(amount,currency)
			
			return(create_criteriumCheck (operator,value=money,kind))
		} else {
			# check for %
			if (grepl("%$", string)) {
				kind = "relative"
				string <- substr(string,1,nchar(string)-1)
				amount <- as.numeric(string)
				return(create_criteriumCheck (operator=operator
								,value=amount,kind=kind))
			} else {
				stop("Error: missing currency or % identifier")
			}
		}
	}
	
	return(parser)	
}

removeStartEndSpaces <- function(string) {
	result <- sub("^[[:blank:]]+", "", string)
	result <- sub("[[:blank:]]+$", "", result)
	return(result)
}

