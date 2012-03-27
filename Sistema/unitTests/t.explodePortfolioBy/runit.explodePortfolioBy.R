# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.shouldExplodePositionsByFund <- function() {
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
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundsDbOwners <- sapply(fundsDb,slot,"owner")	
	originFunds <- filterS4List(origin,by="Cliente",fundsDbOwners)
	fundPortfolios <- portfoliosFactory(originFunds,politicaInvestimento.df)
	
	# ----- Test 1 --------
	# identify and check CB Fixed Income (third portfolio in fundPortfolios)
	fundData <- fundsDb[[3]]
	portfolioPositionsExploded <- explodePositionsByFund(fundData,fundPortfolios,portfolio)
	checkEquals(is(portfolioPositionsExploded,"Positions"),TRUE)
	checkEquals(length(fundPortfolios[[3]]),length(portfolioPositionsExploded))
	checkEquals(sum(portfolio[11]),sum(portfolioPositionsExploded))

	
	# ----- Test 2 --------
	cliente <- c("pippo16") # pippo16 has no CB-Accent Global Economy Fund
	
	# identify and check CB Global Economy (second portfolio in fundPortfolios)
	fundData <- fundsDb[[2]]
	portfolioPositionsExploded <- explodePositionsByFund(fundData,fundPortfolios,portfolio)
	checkEquals(0,length(portfolioPositionsExploded))
	
	
	# ----- Test 3 --------
	cliente <- c("pippo16")
	
	# identify and check CB Global Equity
	fundData <- fundsDb[[1]]
	portfolioPositionsExploded <- explodePositionsByFund(fundData,fundPortfolios,portfolio)
	checkEquals(length(fundPortfolios[[2]]),length(portfolioPositionsExploded))
	checkEquals(sum(portfolio[12]),sum(portfolioPositionsExploded))
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}

test.shouldExplodePositionsByFunds <- function() {
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
	
	# valorePortafoglioPrimaEsplosione <- sum(portfolio)
	# numeroPosizioniPrimaEsplosione <- length(portfolio)
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundsDbOwners <- sapply(fundsDb,slot,"owner")
	originFunds <- filterS4List(origin,by="Cliente",fundsDbOwners)
	fundPortfolios <- portfoliosFactory(originFunds,politicaInvestimento.df)
		
	# ----- Test 1 --------
	# identify and explode w.r.t. CB Global Equity (previous test) & CB Fixed Income
	portfolioPositionsExploded <- explodePositionsByFunds(portfolio,fundsDb,fundPortfolios)
	checkEquals(sum(portfolio[11])+sum(portfolio[12]),sum(portfolioPositionsExploded))
	checkEquals(length(portfolioPositionsExploded),length(fundPortfolios[[2]])+length(fundPortfolios[[3]]))	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}


test.shouldexplodePortfolioByFunds <- function() {
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
	
	# valorePortafoglioPrimaEsplosione <- sum(portfolio)
	# numeroPosizioniPrimaEsplosione <- length(portfolio)
	
	# crea il data.frame dei fondi BAC&P e la lista di fundPortfolios
	fundsDb <- create_fundsDB()
	fundsDbOwners <- sapply(fundsDb,slot,"owner")
	originFunds <- filterS4List(origin,by="Cliente",fundsDbOwners)
	fundPortfolios <- portfoliosFactory(originFunds,politicaInvestimento.df)
	
	# ----- Test 1 --------
	# identify and explode w.r.t. CB Global Equity (previous test) & CB Fixed Income
	portfolioExploded <- explodePortfolioByFunds(portfolio,fundsDb,fundPortfolios)
	checkEquals(sum(portfolioExploded),sum(portfolio))
	totalNumberOfPositions <- length(portfolio)-2+length(fundPortfolios[[3]])+length(fundPortfolios[[2]])
	checkEquals(length(portfolioExploded),totalNumberOfPositions)	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	deallocateTestRepositories("fixedIncome")
}