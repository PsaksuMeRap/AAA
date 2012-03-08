# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldSelectPositionsBySecurities <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$unclassified1	
	positions <- new("Positions",list(p1,p2,p3))
	
	# one position
	criterium <- new("SecuritySelectionCriterium",values="Bond",negation=FALSE)
	result <- selector(criterium,positions)
	should <- c(FALSE,TRUE,FALSE)
	checkEquals(result,should)
	
	# two positions
	should <- c(FALSE,TRUE,TRUE)
	criterium <- new("SecuritySelectionCriterium",values=c("Unclassified","Bond"),negation=FALSE)
	result <- selector(criterium,positions)
	checkEquals(result,should)

	# two positions with negation
	should <- !c(FALSE,TRUE,TRUE)
	criterium <- new("SecuritySelectionCriterium",values=c("Unclassified","Bond"),negation=TRUE)
	result <- selector(criterium,positions)
	checkEquals(result,should)
	
	# zero positions
	should <- c(FALSE,FALSE,FALSE)
	criterium <- new("SecuritySelectionCriterium",values=c("U","B"),negation=FALSE)
	result <- selector(criterium,positions)
	checkEquals(result,should)
	
	# return NULL
	should <- NULL
	positions <- new("Positions")
	criterium <- new("SecuritySelectionCriterium",values="U",negation=FALSE)
	result <- selector(criterium,positions)
	checkEquals(result,should)

}


test.shouldSelectPositionsByMaturityHorizon <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$strutturati_FI
	p4 <- repo$equity2
	p5 <- repo$fondiObbligazionari
	positions <- new("Positions",list(p1,p2,p3,p4,p5))

	# check 1
	criterium <- new("MaturityHorizonSelectionCriterium",values=c(">3Y"),negation=FALSE)
	baseDate <- "2010-12-10"
	result <- selector(criterium,positions,baseDate)
	
	should <- c(FALSE,FALSE,FALSE,FALSE,FALSE)
	checkEquals(result,should)
	
	# check 2 same as before but with negation
	criterium <- new("MaturityHorizonSelectionCriterium",values=c(">3Y"),negation=TRUE)
	baseDate <- "2010-12-10"
	result <- selector(criterium,positions,baseDate)
	
	should <- c(FALSE,TRUE,TRUE,FALSE,TRUE)
	checkEquals(result,should)

	# check 3
	criterium <- new("MaturityHorizonSelectionCriterium",values=c("<3Y"),negation=FALSE)
	result <- selector(criterium,positions)
	should <- c(FALSE,TRUE,TRUE,FALSE,TRUE)
	checkEquals(result,should)
}



test.shouldSelectPositionsByCurrency <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1 # chf
	p2 <- repo$bond1 # eur
	p3 <- repo$strutturati_FI # eur
	p4 <- repo$equity2 # eur
	p5 <- repo$fondiObbligazionari # eur
	positions <- new("Positions",list(p1,p2,p3,p4,p5))	
	
	# check 1
	criterium <- new("CurrencySelectionCriterium",values=c("USD","GBP","EUR"),negation=FALSE)
	result <- selector(criterium,positions)
	
	should <- c(FALSE,TRUE,TRUE,TRUE,TRUE)
	checkEquals(result,should)
	
	# check 2 (negation)
	criterium <- new("CurrencySelectionCriterium",values=c("USD","GBP","EUR"),negation=TRUE)
	result <- selector(criterium,positions)
	
	should <- !c(FALSE,TRUE,TRUE,TRUE,TRUE)
	checkEquals(result,should)
	
	
	# all false
	criterium <- new("CurrencySelectionCriterium",values=c("JPY","USD"),negation=FALSE)
	result <- selector(criterium,positions)
	
	should <- c(FALSE,FALSE,FALSE,FALSE,FALSE)
	checkEquals(result,should)
	
	# empty positions as argument
	positions <- new("Positions")
	criterium <- new("CurrencySelectionCriterium",values=c("JPY","USD"),negation=FALSE)
	result <- selector(criterium,positions)
	should <- NULL
	checkEquals(result,should)

	# empty positions as argument with negation
	positions <- new("Positions")
	criterium <- new("CurrencySelectionCriterium",values=c("JPY","USD"),negation=TRUE)
	result <- selector(criterium,positions)
	should <- NULL
	checkEquals(result,should)
}



test.shouldSelectPositionsByAmountAbsolute <- function() {
	
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1 # 88'205 chf / chf
	p2 <- repo$bond1 # 124'345.632268 chf / 92'896.5967 eur
	p3 <- repo$indexCertificate # 283'354.88 chf / usd
	positions <- new("Positions",list(p1,p2,p3))

	# check1: create the absolute constraint of type > 88205 CHF
	constraint <- new("AbsoluteConstraint",operator=">",value=toMoney(88205,"CHF"))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(FALSE,TRUE,TRUE)
	checkEquals(result,should)
	
	# check 2: create the aboslute checkCriterium of type >= 88205 CHF
	constraint <- new("AbsoluteConstraint",operator=">=",value=toMoney(88205,"CHF"))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,TRUE,TRUE)
	checkEquals(result,should)
	
	# check 3: create the absolute checkCriterium of type = 88205 CHF
	constraint <- new("AbsoluteConstraint",operator="=",value=toMoney(88205,"CHF"))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,FALSE,FALSE)
	checkEquals(result,should)
	
	# check 4: create the absolute checkCriterium of type < 124'345.632268 EUR
	constraint <- new("AbsoluteConstraint",operator="<",value=toMoney(124345.7,"CHF"))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,TRUE,FALSE)
	checkEquals(result,should)
	
	# check 5: create the absolute checkCriterium of type < 124'345.632268 EUR
	constraint <- new("AbsoluteConstraint",operator="<=",value=toMoney(124345.632267,"CHF"))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,FALSE,FALSE)
	checkEquals(result,should)
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}

test.shouldSelectPositionsByAmountRelative <- function() {

	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1 # 88'205 chf / chf
	p2 <- repo$bond1 # 124'345.632268 chf / 92'896.5967 eur
	p3 <- repo$indexCertificate # 283'354.88 chf / usd
	positions <- new("Positions",list(p1,p2,p3))
	
	total <- sum(positions)
	#sapply(positions, "/",total) ( the result is 0.1778665 0.2507446 0.5713888 )
	

	# check1: create a relative checkCriterium of type > 
	constraint <- new("RelativeConstraint",operator=">",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(FALSE,FALSE,TRUE)
	checkEquals(result,should)
	
	# check 2: create a relative checkCriterium of type =
	percentage <- 100*100.1/0.9627/(97.1+100.1/0.9627+80*1.33853808/0.9627)
	checkCriterium <- create_criteriumCheck(operator="=",
			value=percentage,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck2 <- c(FALSE,TRUE,FALSE)
	checkEquals(result,posCheck2)
	
	# check 3: create the relative checkCriterium of type >=
	checkCriterium <- create_criteriumCheck(operator=">=",
			value=33.2932864433126,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck3 <- c(FALSE,TRUE,TRUE)
	checkEquals(result,posCheck3)
	
	# check 4: create the relative checkCriterium of type < 
	checkCriterium <- create_criteriumCheck(operator="<",
			value=33.293286443312,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck4 <- c(TRUE,FALSE,FALSE)
	checkEquals(result,posCheck4)
	
	# check 5: create the relative checkCriterium of type <= 
	checkCriterium <- create_criteriumCheck(operator="<=",
			value=33.2932864433127,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck5 <- c(TRUE,TRUE,FALSE)
	checkEquals(result,posCheck5)
	
	# check6: create the relative checkCriterium of type !=
	checkCriterium <- create_criteriumCheck(operator="!=",
			value=34,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck5 <- c(TRUE,TRUE,TRUE)
	checkEquals(result,posCheck5)
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}
