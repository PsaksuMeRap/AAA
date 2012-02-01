# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExplodePortfolioByFund <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
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

test.shouldExplodeByFundUnionOfPortfolios <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
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
	
	clienti <- c("pippo15","pippo95")
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(clienti,portfParser$parse,origin,politicaInvestimento.df)
	
	# rimuovi le voci non desiderate lasciando CB-Accent Lux Sicav - Fixed Income
	removeFromP1 <- rep(TRUE,19)
	removeFromP1[5]  <- FALSE
	removeFromP1[11] <- FALSE
	removeFromP1[15] <- FALSE
	removeFromP1[16] <- FALSE	
	removeFromP2 <- rep(TRUE,9)
	removeFromP2[3]  <- FALSE
	removeFromP2[5] <- FALSE
	removeFromP2[9] <- FALSE
	portfolios[[1]]$positions$remove(removeFromP1)
	portfolios[[2]]$positions$remove(removeFromP2)
	
	# join portfolio 2 to porfolio 1
	portfolios[[1]]$addPortfolio(portfolios[[2]])
	portfolio <- portfolios[[1]]
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundPortfolios <- lapply(fundsDb[["owner"]],portfParser$parse,origin,politicaInvestimento.df)
	
	
	# ----- Test 1 --------
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(c(clienti,"pippo76"),portfParser$parse,origin,politicaInvestimento.df)
	portfolios[[1]]$positions$remove(removeFromP1)
	portfolios[[2]]$positions$remove(removeFromP2)
	portfolios[[1]]$addPortfolio(portfolios[[2]])
	portfolio <- portfolios[[1]]
	secondaPosizioneInBond <- portfolio$positions$positions[[5]]$money$amount
	
	# identify and check CB Fixed Income
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(portfolio$positions$positions[[143]]$money$amount, secondaPosizioneInBond * 
					portfolios[[3]]$positions$positions[[69]]$money$amount / 
					portfolios[[3]]$value("EUR")$amount
	)
	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}


test.shouldExplodePortfolioByAllFunds <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
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
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
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


test.shouldVerifyPortfolioPositionsConsistency <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
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

	output <- extractUnconsistentPortfolioPositions(portfolio)
	# ----- Test 1 --------
	# all positions should be ok
	checkEquals(output[[1]],list())
	
	
	portfolio$positions$positions[[1]]$money$amount <- NA_real_
	portfolio$positions$positions[[3]]$money$currency <- NA_character_
	
	output <- extractUnconsistentPortfolioPositions(portfolio)
	
	# ----- Test 2 --------
	# two positions should be incomplete
	checkEquals(length(output[[1]]),2)
	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}



