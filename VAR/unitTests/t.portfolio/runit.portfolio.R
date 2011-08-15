# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_character_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(ls(portfolio),c("add","addPortfolio","owner","positions","print","refCurrency","value"))
	
}


test.should_addPositionToPortfolio <- function() {
	source("./lib/portfolio/portfolio.R")
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- create_equityTestPositions()
	
    p <- positions.l[["equityCHF2"]]
	
	
	portfolio <- create_portfolio()
	portfolio$add(p)
	
	checkEquals(portfolio$positions$positions[[1]]$name,"Nestle Na")
	checkEquals(portfolio$positions$positions[[1]]$money$currency,"CHF")
	checkEquals(portfolio$positions$positions[[1]]$money$amount,13.0)
	checkEquals(portfolio$positions$positions[[1]]$id,697)
}

test.should_addPositionsToPortfolio <- function() {
	source("./lib/portfolio/portfolio.R")
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- c(create_equityTestPositions(),
			create_Conto_correnteTestPositions())
	
	p1 <- positions.l[["equityUSD1"]]
	p2 <- positions.l[["equityEUR1"]]
	p3 <- positions.l[["Conto_correnteEUR1"]]
	
	# crea le posizioni
	positions <- create_positions()
	positions$add(p1);positions$add(p2);positions$add(p3)
	
	portfolio <- create_portfolio()
	portfolio$add(positions)
	
	checkEquals(portfolio$positions$positions[[2]]$name,"DT Telekom N")
	checkIdentical(portfolio$positions$positions,positions$positions)
}

test.shouldParsePortfolio <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	source("./unitTests/utilities/createOriginData.R")
	
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	allocateTestRepositories("politicaInvestimento")

	parser <- create_parserPortfolio()
	owner = "pippo76"

	origin <- createOriginData()
	
	portfolio <- parser$parse(owner,origin,
			repositories$politicaInvestimento$politicaInvestimento.df)

	nome1 <- portfolio$positions$positions[[1]][["name"]]
	nome2 <- portfolio$positions$positions[[3]][["name"]]
	
	checkEquals(nome1,"20121008 - 3.75% BASF 08-10-12")
	checkEquals(nome2,"20101025 - 5.625% BNG 25-10-10")
	
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("exchangeRates")	
	deallocateTestRepositories("politicaInvestimento")
}

test.should_returnValueOfPortfolio <- function() {
	source("./lib/portfolio/portfolio.R")
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- c(create_equityTestPositions(),
			create_Conto_correnteTestPositions())
	
	p1 <- positions.l[["equityUSD1"]]
	p2 <- positions.l[["equityEUR1"]]
	p3 <- positions.l[["Conto_correnteEUR1"]]
	
	
	# crea le posizioni
	positions <- create_positions()
	positions$add(p1);positions$add(p2);positions$add(p3)
	
	portfolio <- create_portfolio()
	portfolio$add(positions)
	
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	repository <- repositories$exchangeRates
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# check 1
	amount <- 124.0*0.9627+98+10*1.33853808
	checkEquals(portfolio$value("CHF"),toMoney(amount,"CHF"))

	# check 2
	amount <- 124.0+98/0.9627+10*1.33853808/0.9627
	checkEquals(portfolio$value("USD"),toMoney(amount,"USD"))
	
	# ripristina la situazione iniziale
	repositories$exchangeRates <- repository
}

test.should_addPortfolio <- function() {
	source("./lib/portfolio/portfolio.R")
	source("./unitTests/utilities/createInstrumentsForTests.R")
	
	positions.l <- c(create_equityTestPositions(),
			create_Conto_correnteTestPositions())
	
	p1 <- positions.l[["equityUSD1"]]
	p2 <- positions.l[["equityEUR1"]]
	p3 <- positions.l[["Conto_correnteEUR1"]]
	
	# crea le posizioni
	positions1 <- create_positions()
	positions1$add(p1);positions1$add(p2)
	
	positions2 <- create_positions()
	positions2$add(p3)
	
	portfolio1 <- create_portfolio()
	portfolio1$owner <- "pippo1"
	portfolio1$refCurrency <- "CHF"
	portfolio1$add(positions1)
	
	portfolio2 <- create_portfolio()
	portfolio2$owner <- "pippo2"
	portfolio2$refCurrency <- "CHF"
	portfolio2$add(positions2)
	
	positionsCheck <- create_positions()
	positionsCheck$add(p1);positionsCheck$add(p2);positionsCheck$add(p3)
	
	portfolio1$addPortfolio(portfolio2)
	
	checkIdentical(portfolio1$positions$positions,positionsCheck$positions)
}
