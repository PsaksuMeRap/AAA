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
		
		# extend position if necessary
		extendPosition(position)
		
		return(position)
	}
	
	return(parser)
}

