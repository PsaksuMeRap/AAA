# TODO: Add comment
# 
# Author: claudio
###############################################################################




test.shouldReweightPositions <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	
	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")

	origin1 <- list("pippo16","L","EUR",4477.2698043909,"",
			"EUR-0456-0122993-92-000",NA,NA,4477.2698043909,4477.2698043909,
			NA,"A","Saldi in Cto. Corrente",5975.0597818207,4477.2698043909,
			6250.71637391013,NA,40,NA,NA,NA,NA)
	names(origin1) <- nomi
	
	origin2 <- list("pippo16","Oacc","EUR",0,"2490099",
			"20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata",
			NA,NA,0,0,NA,"D","Obbligazioni e simili          ",0,0,0,825,
			3,NA,NA,NA,NA)
	names(origin2) <- nomi
	
	origin3 <- list("pippo16","O      ","EUR",11000,"2490099",
			"20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20",
			10126.7272727273,11487,1263570,1263570,0.134325008528287,"D ",
			"Obbligazioni e simili          ",1686274.5866043,1263570,
			1764070.077,825,3,NA,NA,NA,NA)
	names(origin3) <- nomi
	
	origin4 <- list("pippo16","A","CHF",30050,"2742261CH",
			"OnCapital Global Equity Fund Cap B",99.9755741239983,73.74,
			2215887,1660422.54258738,-0.262419839584604,"E","Azioni e simili",
			2215887,1660422.54258738,2318115.91170625,1701,14,NA,NA,NA,NA)
	names(origin4) <- nomi
	
	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	
	# create client portfolio
	# create position 1
	parser <- create_parserPosition()
	position1 <- parser$parse(origin1)
	
	# create position 2
	parser <- create_parserPosition()
	position2 <- parser$parse(origin2)
	
	# create position 3
	parser <- create_parserPosition()
	position3 <- parser$parse(origin3)
	
	# create position 4
	parser <- create_parserPosition()
	position4 <- parser$parse(origin4)
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)
	positions$add(position4)
	
	portfolio <- create_portfolio()
	portfolio$owner <- "composizione fondo"
	portfolio$refCurrency <- "EUR"
	portfolio$add(positions)
	
	valorePortafoglio <- portfolio$positions$sum("CHF")
	pesoCliente <- 0.0025
	
	weightPositions(portfolio$positions,pesoCliente)
	
	NuovoValorePortafoglio <- portfolio$positions$sum("CHF")
	
	checkEquals(NuovoValorePortafoglio$amount,valorePortafoglio$amount*pesoCliente)
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	
}


test.shouldExplodePortfolioByFund <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	source("./base/unitTests/utilities/createOriginData.R")
	
	origin <- createOriginData()

	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("fixedIncome")
	
	# initialize the investmentPolicy table used in the portfolio construction in order
	# to determine the reference currency
	politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df	
	
	cliente <- c("pippo16")
	portfParser <- create_parserPortfolio()
	portfolio <- lapply(cliente,portfParser$parse,origin,politicaInvestimento.df)[[1]]
	valorePortafoglioPrimaEsplosione <- portfolio$value()
	numeroPosizioniPrimaEsplosione <- length(portfolio$positions$positions)
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundPortfolios <- lapply(fundsDb[["owner"]],portfParser$parse,origin,politicaInvestimento.df)
	
	# ----- Test 1 --------
	# identify and check CB Fixed Income
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(portfolio$value(),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio$positions$positions),88)
	
	# ----- Test 2 --------
	cliente <- c("pippo16")
	portfParser <- create_parserPortfolio()
	portfolio <- lapply(cliente,portfParser$parse,origin,politicaInvestimento.df)[[1]]
	
	# identify and check CB Global Economy
	fundData <- as.list(fundsDb[2,,drop=FALSE])
	explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(portfolio$value(),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio$positions$positions),numeroPosizioniPrimaEsplosione)

	
	# ----- Test 3 --------
	cliente <- c("pippo16")
	portfParser <- create_parserPortfolio()
	portfolio <- lapply(cliente,portfParser$parse,origin,politicaInvestimento.df)[[1]]
	
	# identify and check CB Global Equity
	fundData <- as.list(fundsDb[1,,drop=FALSE])
	explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(portfolio$value(),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio$positions$positions),63)
	
	# identify and explode w.r.t. CB Global Equity (previous test) & CB Fixed Income
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(portfolio$value(),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio$positions$positions),132)	
	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}

test.shouldExplodePortfolioByAllFunds <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	source("./base/unitTests/utilities/createOriginData.R")
	
	origin <- createOriginData()
	
	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("fixedIncome")
	
	# initialize the investmentPolicy table used in the portfolio construction in order
	# to determine the reference currency
	politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df	
	
	cliente <- c("pippo16")
	portfParser <- create_parserPortfolio()
	portfolio <- lapply(cliente,portfParser$parse,origin,politicaInvestimento.df)[[1]]
	valorePortafoglioPrimaEsplosione <- portfolio$value()
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundPortfolios <- lapply(fundsDb[["owner"]],portfParser$parse,origin,politicaInvestimento.df)
	
	invisible(explodePortfolioByAllFunds(portfolio,fundsDb,fundPortfolios)) 
	
	checkEquals(portfolio$value(),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio$positions$positions),132)

	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}

test.shouldExplodeAllPortfoliosByAllFunds <- function() {
	source("./base//lib/repository.R")
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	source("./base/unitTests/utilities/createOriginData.R")
	
	origin <- createOriginData()
	
	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("fixedIncome")
	
	# initialize the investmentPolicy table used in the portfolio construction in order
	# to determine the reference currency
	politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df	
	
	cliente <- c("pippo16","pippo22","pippo53","pippo210","pippo76")
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(cliente,portfParser$parse,origin,politicaInvestimento.df)
	
	valorePortafoglioPrimaEsplosione <- sapply(portfolios,function(x){x$value()})
	
	
	explodeAllPortfoliosByAllFunds(portfolios)
	
	checkEquals(sapply(portfolios,function(x){x$value()}),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolios[[1]]$positions$positions),132)
	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}

test.shouldAreConsistent <- function() {
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=0.0#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="20101217 - 1.326% Rabobank Nederland 17-12-10",
			currency="EUR",
			amount=401440#,
			#origin=list(ID_AAA=1568)
	)
	class(position2) <- c("bond",class(position2))
	extendPosition(position2,origin=list(ID_AAA=1568))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="20110715 - 3.625% Rabo 15-07-11 Pro-rata",
			currency="EUR",
			amount=178.767120361328#,
			#origin=list(ID_AAA=1161,Strumento="Oacc")
	)
	class(position3) <- c("bond",class(position3))
	class(position3) <- c("accruedInterest",class(position3))	
	extendPosition(position3,origin=list(ID_AAA=1161,Strumento="Oacc"))
	
	# create position 4	(strutturato FI)
	position4 <- create_position()
	position4$create(name="20170924 - >3Y - EUR UBS AG FRN with Floor and Cap",
			currency="EUR",
			amount=197800.00#,
			#origin=list(ID_AAA=114)
	)
	class(position4) <- c("Strutturati_FI",class(position4))	
	extendPosition(position4,origin=list(ID_AAA=114))
	
	# create position 5 (equity)
	position5 <- create_position()
	position5$create(name="bbb",
			currency="CHF",
			amount=1.0#,
			#origin=list(ID_AAA=11)
	)
	class(position5) <- c("equity",class(position5))
	
	# create position 6 (Fondi_obbligazionari)
	position6 <- create_position()
	position6$create(name="20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR",
			currency="EUR",
			amount=1.0#,
			#origin=list(ID_AAA=11)
	)
	class(position6) <- c("Fondi_obbligazionari",class(position6))
	
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	positions$add(position4)
	positions$add(position5)
	positions$add(position6)
	
	result <- areConsistent(positions)
	posCheck <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
	checkEquals(result,posCheck)
	
	# check 2

	# create position 2	
	position2 <- create_position()
	position2$create(name="20101217 - 1.326% Rabobank Nederland 17-12-10",
			currency=NA,
			amount=401440#,
			#origin=list(ID_AAA=1568)
	)
	class(position2) <- c("bond",class(position2))
	extendPosition(position2,origin=list(ID_AAA=1568))
	
	
	# create position 5 (equity)
	position5 <- create_position()
	position5$create(name="bbb",
			currency="CHF",
			amount=NA#,
			#origin=list(ID_AAA=11)
	)
	class(position5) <- c("equity",class(position5))
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	positions$add(position4)
	positions$add(position5)
	positions$add(position6)
	
	result <- areConsistent(positions)
	posCheck <- c(TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)
	checkEquals(result,posCheck)
}

