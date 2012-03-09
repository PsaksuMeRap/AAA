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

	if (!is.null(repository)) repositories$exchangeRates <- repository
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
	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
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
	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
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
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
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
	# sapply(positions,"/",total) #( the result is 0.1778665 0.2507446 0.5713888 )
	

	# check1: create a relative checkCriterium of type > 
	constraint <- new("RelativeConstraint",operator=">",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(FALSE,FALSE,TRUE)
	checkEquals(result,should)
	
	# check 2: create a relative checkCriterium of type =
	constraint <- new("RelativeConstraint",operator="=",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(FALSE,TRUE,FALSE)
	checkEquals(result,should)
	
	# check 3: create the relative checkCriterium of type >=
	constraint <- new("RelativeConstraint",operator=">=",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(FALSE,TRUE,TRUE)
	checkEquals(result,should)
	
	# check 4: create the relative checkCriterium of type < 
	constraint <- new("RelativeConstraint",operator="<",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,FALSE,FALSE)
	checkEquals(result,should)
	
	# check 5: create the relative checkCriterium of type <= 
	constraint <- new("RelativeConstraint",operator="<=",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,TRUE,FALSE)
	checkEquals(result,should)
	
	# check6: create the relative checkCriterium of type !=
	constraint <- new("RelativeConstraint",operator="!=",value=100 * (positions[[2]] / total))
	criterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
	
	result <- selector(criterium,positions)
	should <- c(TRUE,FALSE,TRUE)
	checkEquals(result,should)
	
	# reset the repository in the original state
	if (!is.null(repository)) repositories$exchangeRates <- repository
	
}


test.shouldExtractPositions <- function() {
	
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
	
	p1 <- repo$equity1 # CHF
	p2 <- repo$bond1 # EUR
	p3 <- repo$strutturati_FI # EUR
	p4 <- repo$equity2 # EUR
	p5 <- repo$fondiObbligazionari # EUR
	p6 <- repo$indexCertificate # USD
	positions <- new("Positions",list(p1,p2,p3,p4,p5,p6))
	# "Equity / CHF / 88'205.00 / Roche Holding Gs"
	# "Bond / EUR / 92'896.60 / 20130603 - 3.625% Pfizer 03-06-13"
	# "Strutturati_FI / EUR / 133'951.68 / 20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	# "Equity / EUR / 5'558.12 / Kontron AG"
	# "Fondi_obbligazionari / EUR / 7'694.96 / 20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	# "Index_certificate / USD / 294'333.52 / ISHARES MSCI Indon"
	
	# test1
	selectionString <- new("SelectionString","security:Equity,Bond & currency!:GBP + security:Index_certificate & amount:>=294333.50USD")
	should <- new("Positions",list(p1,p2,p4,p6))
	
	result <- selector(selectionString,positions)
	checkEquals(result,should)
	
	# test2: with an empty positions
	should <- new("Positions")

	result <- selector(selectionString,new("Positions"))
	checkEquals(result,should)
	
	
	# reset the repository in the original state
	if (!is.null(repository)) repositories$exchangeRates <- repository
}
