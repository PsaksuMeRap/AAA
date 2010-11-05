# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCheckPositionByAmount <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=100.0,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
		
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
		
	
	# check >
	checkCriterium <- create_criteriumCheck(operator=">",
			value=toMoney(100.8623,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,FALSE)

	
	# check <
	checkCriterium <- create_criteriumCheck(operator="<",
			value=toMoney(100.8623,"EUR"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,TRUE)
	
	# check =
	checkCriterium <- create_criteriumCheck(operator="=",
			value=toMoney(96.27,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,TRUE)
	
	# check >=
	checkCriterium <- create_criteriumCheck(operator=">=",
			value=toMoney(96.2701,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,FALSE)
	
	# check <=
	checkCriterium <- create_criteriumCheck(operator="<=",
			value=toMoney(1000.258,"EUR"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,TRUE)
	
	# check !=
	checkCriterium <- create_criteriumCheck(operator="!=",
			value=toMoney(96.27,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			,criteriumCheck=checkCriterium)
	result <- check(position1,criterium)
	checkEquals(result,FALSE)	
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}

test.shouldCheckPositionByInstrument <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	
	# check 1: should recognize equity
	criterium <- create_criteriumSelection(factor="instrument",
			values=c("equity","bond"))
	result <- check(position1,criterium)
	checkEquals(result,TRUE)
	
	# check 2: should not recognize equity
	criterium <- create_criteriumSelection(factor="instrument",
			values=c("option on equity","bond"))
	result <- check(position1,criterium)
	checkEquals(result,FALSE)	
	
}

test.shouldCheckPositionByCurrency <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	
	# check 1: should recognize equity
	criterium <- create_criteriumSelection(factor="currency",
			values=c("EUR","USD"))
	result <- check(position1,criterium)
	checkEquals(result,TRUE)
	
	# check 2: should not recognize equity
	criterium <- create_criteriumSelection(factor="instrument",
			values=c("CHF","EUR","JPY"))
	result <- check(position1,criterium)
	checkEquals(result,FALSE)	
	
}

test.shouldCheckFailWithNonImplementedFactor <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# check 1: should recognize equity
	criterium <- create_criteriumSelection(factor="test",
			values=c("EUR","USD"))
	checkException(check(position1,criterium))
	
}


test.shouldExtractPositionsByCurrency <- function() {
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=1000.5,
			origin=list(ID_AAA=10)
	)
	class(position3) <- c("ABC",class(position3))	
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	
	criterium <- create_criteriumSelection(factor="currency",values=c("EUR","USD"))
	
	result <- positionsSelector(criterium,positions)
	
	posCheck <- c(TRUE,FALSE,TRUE)
	
	checkEquals(result,posCheck)
	
	# all false
	posCheck <- create_positions()
	criterium <- create_criteriumSelection(factor="currency",values="abc")
	result <- positionsSelector(criterium,positions)
	posCheck <- rep(FALSE,3)
	checkEquals(result,posCheck)
	
	# empty positions as argument
	positions <- create_positions()
	criterium <- create_criteriumSelection(factor="currency",values="abc")
	result <- positionsSelector(criterium,positions)
	checkEquals(result,NULL)
}


test.shouldExtractPositionsByInstrument <- function() {
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=1000.5,
			origin=list(ID_AAA=10)
	)
	class(position3) <- c("ABC",class(position3))	
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	
	# one position
	criterium <- create_criteriumSelection(factor="instrument",values=c("bond"))
	
	result <- positionsSelector(criterium,positions)
	
	posCheck <- c(FALSE,TRUE,FALSE)
	
	checkEquals(result,posCheck)
	
	# two positions
	posCheck <- c(FALSE,TRUE,TRUE)
	
	criterium <- create_criteriumSelection(factor="instrument",values=c("ABC","bond"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
	
	
	# zero positions
	posCheck <- c(FALSE,FALSE,FALSE)
	
	criterium <- create_criteriumSelection(factor="instrument",values=c("abc"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
	
	# return NULL
	posCheck <- NULL
	positions <- create_positions()
	criterium <- create_criteriumSelection(factor="instrument",values=c("abc"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
}

test.shouldExtractPositionsByAmountAbsolute <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1/0.9627,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1,
			origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80/1.33853808,
			origin=list(ID_AAA=10)
	)
	class(position3) <- c("ABC",class(position3))	
	
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	
	# check1: create the absolute checkCriterium of type > 12.1 CHF
	checkCriterium <- create_criteriumCheck(operator=">",
			value=toMoney(12.1,"CHF"),kind="absolute")
	
	criterium <- create_criteriumSelection(factor="amount",
			values="",criteriumCheck=checkCriterium)

	result <- positionsSelector(criterium,positions)
	posCheck1 <- c(TRUE,TRUE,TRUE)
	checkEquals(result,posCheck1)
	
	# check 2: create the aboslute checkCriterium of type >= 100.1 CHF
	checkCriterium <- create_criteriumCheck(operator=">=",
			value=toMoney(100.1,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck2 <- c(FALSE,TRUE,FALSE)
	checkEquals(result,posCheck2)
	
	# check 3: create the absolute checkCriterium of type = 97.1 CHF
	checkCriterium <- create_criteriumCheck(operator="=",
			value=toMoney(97.1,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck3 <- c(TRUE,FALSE,FALSE)
	checkEquals(result,posCheck3)
	
	# check 4: create the absolute checkCriterium of type < 80.1/1.33853808 EUR
	checkCriterium <- create_criteriumCheck(operator="<",
			value=toMoney(80.1/1.33853808,"EUR"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck4 <- c(FALSE,FALSE,TRUE)
	checkEquals(result,posCheck4)
	
	# check 5: create the absolute checkCriterium of type <= 97.1 CHF
	checkCriterium <- create_criteriumCheck(operator="<=",
			value=toMoney(97.1,"CHF"),kind="absolute")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck5 <- c(TRUE,FALSE,TRUE)
	checkEquals(result,posCheck5)
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}

test.shouldExtractPositionsByAmountRelative <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1,
			origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80,
			origin=list(ID_AAA=10)
	)
	class(position3) <- c("ABC",class(position3))	
	
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	# valore portafogio: 97.1             ~ 0.3109086 %
    #          + 100.1/0.9627             ~ 0.3329329 %
    #             + 80*1.33853808/0.9627  ~ 0.3561585 %
    #               = 312.3104


	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	
	# check1: create a relative checkCriterium of type > 
	checkCriterium <- create_criteriumCheck(operator=">",
			value=0.3416872,kind="relative")
	
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)

	posCheck1 <- c(FALSE,FALSE,TRUE)
	checkEquals(result,posCheck1)
	
	# check 2: create a relative checkCriterium of type =
    percentage <- 100.1/0.9627/(97.1+100.1/0.9627+80*1.33853808/0.9627)
	checkCriterium <- create_criteriumCheck(operator="=",
			value=percentage,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck2 <- c(FALSE,TRUE,FALSE)
	checkEquals(result,posCheck2)
	
	# check 3: create the relative checkCriterium of type >=
	checkCriterium <- create_criteriumCheck(operator=">=",
			value=0.332932864433126,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck3 <- c(FALSE,TRUE,TRUE)
	checkEquals(result,posCheck3)
	
	# check 4: create the relative checkCriterium of type < 
	checkCriterium <- create_criteriumCheck(operator="<",
			value=0.33293286443312,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck4 <- c(TRUE,FALSE,FALSE)
	checkEquals(result,posCheck4)
	
	# check 5: create the relative checkCriterium of type <= 
	checkCriterium <- create_criteriumCheck(operator="<=",
			value=0.332932864433127,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck5 <- c(TRUE,TRUE,FALSE)
	checkEquals(result,posCheck5)

	# check6: create the relative checkCriterium of type !=
	checkCriterium <- create_criteriumCheck(operator="!=",
			value=0.34,kind="relative")
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)
	posCheck5 <- c(TRUE,TRUE,TRUE)
	checkEquals(result,posCheck5)
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}
