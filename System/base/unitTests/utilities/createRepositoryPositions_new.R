# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryPositions <- function() {

	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	## create the instrument repository	
	allocateTestRepositories("instruments")
	
	## create the origin
	repository <- createRepositoryAyrtonPositions()
	
	testData <- new.env()

	## create equity1
	Moneta <- "CHF"
	Saldo <- 15
	NumeroValore <- "1203204CH"
	Nome <- "Roche Holding Gs"
	ValoreMercatoMonetaCHF <- 88205
	ID_strumento <- 1

	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Equity",currency=currency,name=name,id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionEquity",
			id=id,
			security=security,
			quantity=quantity,
			value=value)
	A_equity1 <- x
	testData$equity1 <- x

	## create equity2
	x@Moneta <- "EUR"
	x@Saldo <- 1000
	x@NumeroValore <- "1469452EU"
	x@Nome <- "Kontron AG"
	x@ValoreMercatoMonetaCHF <- 7439.7503136
	x@ID_strumento <- 1
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Equity",currency=currency,name=name,id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("PositionEquity",
			id=id,
			security=security,
			quantity=quantity,
			value=value)	

	A_equity2 <- x
	testData$equity2 <- x
	
	## create an indexCertificate
	Moneta <- "USD"
	Saldo <- 100
	NumeroValore <- "US46429B3096"
	Nome <- "ISHARES MSCI Indon"
	ValoreMercatoMonetaCHF <- 283354.88
	ID_strumento <- 15
	
	currency <- new("Currency",Moneta)
	name <- Nome
	id <- new("IdAyrton",new("IdAAA_character",NumeroValore),idStrumento=ID_strumento)
	
	security <- new("Index_certificate",currency=currency,name=name,id)
	quantity <- Saldo
	value <- toMoney(valoreMercatoMonetaCHF,new("Currency","CHF"))
	value <- repositories$exchangeRates$exchange(value,currency)
	x <- new("Position",
			id=id,
			security=security,
			quantity=quantity,
			value=value)	
	
	A_indexCertificate <- x
	testData$indexCertificate <- x
	
}
