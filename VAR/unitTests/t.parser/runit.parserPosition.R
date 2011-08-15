# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldParseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the equity repository and instrument repository
	allocateTestRepositories("equities")	
	allocateTestRepositories("instruments")	

	origin <- list()
	origin$Cliente <- "pippo160"
	origin$Strumento <- "A"
	origin$Moneta <- "CHF"
	origin$Nome <- "Roche Holding Gs"
	origin$ValoreMercatoMonetaCHF <- 88205
	origin$ID_AAA <- 824
	origin$ID_strumento <- 1

	source("./unitTests/utilities/allocateTestRepositories.R")
	allocateTestRepositories("exchangeRates")
	
	equity <- parser$parse(origin)
	deallocateTestRepositories("exchangeRates")	
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"ROG.S")
	
	# restore initial conditions
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")		

}


test.shouldParseBondPosition <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument and exchange rates repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	origin <- list()
	origin$Cliente <- "pippo97"	
	origin$Strumento <- "O      "
	origin$Moneta <- "EUR"
	origin$Nome <- "20101217 - 1.326% Rabobank Nederland 17-12-10"
	origin$ValoreMercatoMonetaCHF <- 66966.8152582
	origin$ID_AAA <- 1568
	origin$ID_strumento <- 2
	

	bond <- parser$parse(origin)
	checkEquals(is.element("bond",class(bond)),TRUE)

	
    # restore initial conditions
	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")		
}

test.shouldParseFondo_obbligazionario <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument and exchange rates repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	origin <- list()
	origin$Cliente <- "pippo61"	
	origin$Strumento <- "O      "
	origin$Moneta <- "EUR"
	origin$Nome <- "20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	origin$ValoreMercatoMonetaCHF <- 306595.4
	origin$ID_AAA <- 825
	origin$ID_strumento <- 2
	
	
	fondo <- parser$parse(origin)
	checkEquals(is.element("Fondi_obbligazionari",class(fondo)),TRUE)
	
	
	# restore initial conditions
	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")		
}

test.shouldParseStrutturatoFixedIncome <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument and exchange rates repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")	
	
	origin <- list()
	origin$Cliente <- "pippo136"
	origin$Strumento <- "PS"
	origin$Moneta <- "EUR"
	origin$Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	origin$ValoreMercatoMonetaCHF <- 399892.3
	origin$ID_AAA <- 98
	origin$ID_strumento <- 49
	
	ps <- parser$parse(origin)
	checkEquals(is.element("Strutturati_FI",class(ps)),TRUE)
	
	# restore initial conditions
	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")		
	
}

test.shouldIdenfyAccruedInterest <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# allocate the repositories instruments and exchangeRates
	allocateTestRepositories("instruments")	
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	origin <- list()
	origin$Cliente <- "pippo185"
	origin$Strumento <- "Oacc"
	origin$Moneta <- "CHF"
	origin$Nome <- "20110527 - 3.25% IBM 27-05-11 Pro-rata"
	origin$ValoreMercatoMonetaCHF <- 368.3333
	origin$ID_AAA <- 1172
	origin$ID_strumento <- 2
	
	bond <- parser$parse(origin)
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	checkEquals(is.element("accruedInterest",class(bond)),TRUE)
	
	# restore initial conditions

	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")	
}

