# TODO: Add comment
# 
# Author: claudio
###############################################################################





DBPortfolioGeneraleLoader <- function() {
	connection <- odbcConnect("prezzi_storici_azioni_VAR",utente,password)
	query = paste("SELECT B.ID, A.* FROM [Sistema (prova)].dbo.DBPortfolioGenerale A",
	        "INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente")
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	
	DBPortfolioGenerale.df[["Cliente"]] <- NULL
	colnames(DBPortfolioGenerale.df)[1] <- "Cliente"
	return(DBPortfolioGenerale.df)
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



