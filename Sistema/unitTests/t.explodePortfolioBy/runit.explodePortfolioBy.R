# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.shouldExplodePortfolioByFund <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	origin <- new("AyrtonPositions",createOriginData())

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
	originPippo <- filterS4List(origin,by="Cliente",cliente)

	portfolio <- portfoliosFactory(originPippo,politicaInvestimento.df)[[1]]
	
	valorePortafoglioPrimaEsplosione <- sum(portfolio)
	numeroPosizioniPrimaEsplosione <- length(portfolio)
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	originFunds <- filterS4List(origin,by="Cliente",fundsDb[["owner"]])
	fundPortfolios <- portfoliosFactory(originFunds,politicaInvestimento.df)
	
	# ----- Test 1 --------
	# identify and check CB Fixed Income
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	portfolioExploded <- explodePortfolioByFund(fundData,fundPortfolios,portfolio)
	checkEquals(sum(portfolioExploded),valorePortafoglioPrimaEsplosione)
	checkEquals(length(portfolio),88)
	
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

