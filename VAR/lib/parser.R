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

	parser$splitFactorsAndValuesFromTypeAndValue <- function(criteriumString) {
		result <- unlist(strsplit(criteriumString,";"))
		
		if (length(result)==0) stop("Error: empty criteriumString")
		result <- removeStartEndSpaces(result)
		
		if (length(result)==1)  {
			x <- list(stringSelectionCriteria=result[[1]],constraint="")	
		} else {
			result2 <- parser$constructCriteriumCheck(result[[2]])
			x <- list(stringSelectionCriteria=result[[1]],constraint=result2)		
		}
			
		return(x)
	}
	
	parser$splitUnionOfFactorsAndValuesBlocks <- function(string) {
		result <- unlist(strsplit(string,"\\+"))
		result <- removeStartEndSpaces(result)
		result <- lapply(result,parser$splitFactorsAndValuesBlocks)
		return(result)
	}
	
	parser$splitFactorsAndValuesBlocks <- function(string) {
	    result <- unlist(strsplit(string,"\\&"))
		result <- removeStartEndSpaces(result)
		#if (length(result)==1) {
		#	return(list(parser$splitFactorsFromValues(result)))
		#}
		
		result <- lapply(result,parser$splitFactorsFromValues)
		return(result)
	}
	
	parser$splitFactorsFromValues <- function(string) {
		result <- unlist(strsplit(string,":"))
		if (length(result)<2) stop(paste("Error: splitFactorsFromValues should return 2 blocks.",
							string))
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
	
	parser$constructCriteriumCheck <- function(string) {
		# a string with the quantitative constraint to identify
		# "=0USD" or "> 1.77 EUR" or < 5% 
	
		string <- removeStartEndSpaces(string)
		
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

