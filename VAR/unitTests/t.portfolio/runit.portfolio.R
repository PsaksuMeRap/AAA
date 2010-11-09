# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_character_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(ls(portfolio),c("add","owner","positions","print","refCurrency","value"))
	
}


test.should_addPositionToPortfolio <- function() {
	source("./lib/portfolio.R")
	
    p <- create_position()
	p$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	portfolio <- create_portfolio()
	portfolio$add(p)
    
	checkEquals(portfolio$positions$positions[[1]]$name,"pippo")
}

test.should_addPositionsToPortfolio <- function() {
	source("./lib/portfolio.R")
	
	p1 <- create_position()
	p1$create(name="SUNCOR ENERGY  INC. CO",
			currency="USD",
			amount=243250,
			origin=list(ID_AAA=1984)
	)	
	p2 <- create_position()
	p2$create(name="Philips",
			currency="EUR",
			amount=22065.28,
			origin=list(ID_AAA=766)
	)
	p3 <- create_position()
	p3$create(name="E.ON",
			currency="EUR",
			amount=25356.00,
			origin=list(ID_AAA=445)
	)
	
	# crea le posizioni
	positions <- create_positions()
	positions$add(p1);positions$add(p2);positions$add(p3)
	
	portfolio <- create_portfolio()
	portfolio$add(positions)
	
	checkEquals(portfolio$positions$positions[[2]]$name,"Philips")

	checkIdentical(portfolio$positions$positions,positions$positions)
}

test.shouldParsePortfolio <- function() {
	source("./unitTests/utilities/createPositionsData.R")
	
	parser <- create_parserPortfolio()
	owner = "pippo76"
	dati.df <- createPositionsData()
	portfolio <- parser$parse(owner,dati.df)
			
}

test.should_returnValueOfPortfolio <- function() {
	source("./lib/portfolio.R")
	
	p1 <- create_position()
	p1$create(name="SUNCOR ENERGY  INC. CO",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=1984)
	)	
	p2 <- create_position()
	p2$create(name="Philips",
			currency="EUR",
			amount=10.50,
			origin=list(ID_AAA=766)
	)
	p3 <- create_position()
	p3$create(name="E.ON",
			currency="EUR",
			amount=200,
			origin=list(ID_AAA=445)
	)
	
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
	amount <- 100*0.9627+10.50+200*1.33853808
	checkEquals(portfolio$value("CHF"),toMoney(amount,"CHF"))

	# check 2
	amount <- 100+10.50/0.9627+200*1.33853808/0.9627
	checkEquals(portfolio$value("USD"),toMoney(amount,"USD"))
	
	repositories$exchangeRates <- repository
}
