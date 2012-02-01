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
			amount=100.0 # ,
	# origin=list(ID_AAA=10)
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
			amount=97.1# ,
			# origin=list(ID_AAA=10)
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
			amount=97.1#,
			#origin=list(ID_AAA=10)
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
			amount=97.1#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# check 1: should recognize equity
	criterium <- create_criteriumSelection(factor="test",
			values=c("EUR","USD"))
	checkException(check(position1,criterium),silent=TRUE)
	
}

test.shouldExtractPositionsByMaturityHorizon <- function() {
	
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
	
	criterium <- create_criteriumSelection(factor="maturityHorizon",values=c(">3Y"))
	baseDate <- "2010-12-10"
	result <- positionsSelector(criterium,positions,baseDate)
	
	# result <- positionsSelector(criterium,positions)
	posCheck <- c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)
	checkEquals(result,posCheck)

	# check 2
	criterium <- create_criteriumSelection(factor="maturityHorizon",values=c("<3Y"))
	baseDate <- "2010-12-10"
	result <- positionsSelector(criterium,positions,baseDate)
	
    # result <- positionsSelector(criterium,positions)
	posCheck <- c(FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)

	checkEquals(result,posCheck)
}



test.shouldExtractPositionsByCurrency <- function() {
	
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
	position2$create(name="AAA",
			currency="CHF",
			amount=0.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=1000.5#,
			#origin=list(ID_AAA=10)
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

test.shouldExtractPositionsByCurrencyWithNegation <- function() {
	
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
	position2$create(name="AAA",
			currency="CHF",
			amount=0.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=1000.5#,
			#origin=list(ID_AAA=10)
	)
	class(position3) <- c("ABC",class(position3))	
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)	
	
	criterium <- create_criteriumSelection(factor="currency",values=c("EUR","USD"),negation=TRUE)
	
	result <- positionsSelector(criterium,positions)
	
	posCheck <- ! c(TRUE,FALSE,TRUE)
	
	checkEquals(result,posCheck)
	
	# all true
	posCheck <- create_positions()
	criterium <- create_criteriumSelection(factor="currency",values="abc",negation=TRUE)
	result <- positionsSelector(criterium,positions)
	posCheck <- rep(TRUE,3)
	checkEquals(result,posCheck)
	
	# empty positions as argument
	positions <- create_positions()
	criterium <- create_criteriumSelection(factor="currency",values="abc",negation=TRUE)
	result <- positionsSelector(criterium,positions)
	checkEquals(result,NULL)
}

test.shouldExtractPositionsByInstrument <- function() {
	
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
	position2$create(name="AAA",
			currency="CHF",
			amount=0.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=1000.5#,
			#origin=list(ID_AAA=10)
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
			amount=97.1/0.9627#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80/1.33853808#,
			#origin=list(ID_AAA=10)
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
			amount=97.1#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80#,
			#origin=list(ID_AAA=10)
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
			value=34.16872,kind="relative")
	
	criterium <- create_criteriumSelection(factor="amount",
			criteriumCheck=checkCriterium)
	
	result <- positionsSelector(criterium,positions)

	posCheck1 <- c(FALSE,FALSE,TRUE)
	checkEquals(result,posCheck1)
	
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


test.shouldApplyLogicalAnd <- function() {

	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80#,
			#origin=list(ID_AAA=10)
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
	
	# initialize parser selectionCriteria
	parser <- create_parserSelectionCriteria()
	string = "instrument:bond,equity & currency:CHF"
	criteria <- parser$splitFactorsString(string)
	
	result <- filterByCriteriaLogicalAnd(criteria,positions)
	
	posCheck1 <- c(FALSE,TRUE,FALSE)
	checkEquals(result,posCheck1)
	
	# check that returns an empty logical when applying to empty positions
	positions <- create_positions()
	result <- filterByCriteriaLogicalAnd(criteria,positions)
	checkEquals(result,vector(mode="logical"))
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository

}

test.shouldApplyLogicalOr <- function() {
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80#,
			#origin=list(ID_AAA=10)
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
	
	# initialize parser selectionCriteria
	parser <- create_parserSelectionCriteria()
	string = "instrument:bond,equity & currency:CHF + currency:EUR & amount:>79EUR"
	unionOfBlocksOfCriteria <- parser$splitSelectionString(string)
	result <- filterByCriteriaLogicalOr(unionOfBlocksOfCriteria,positions)

	posCheck1 <- c(FALSE,TRUE,TRUE)
	checkEquals(result,posCheck1)
	
	# check that returns an empty logical when applying to empty positions
	positions <- create_positions()
	result <- filterByCriteriaLogicalOr(unionOfBlocksOfCriteria,positions)
	checkEquals(result,vector(mode="logical"))

	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}

test.shouldFilterPositionsFromselectionString <- function() {
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=97.1#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100.1#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=80#,
			#origin=list(ID_AAA=10)
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
	
	selectionString = "instrument:bond,equity & currency:CHF + currency:EUR & amount:>79EUR"
	result <- extractPositionsFromSelectionString(selectionString,positions)
	positions$remove(1)
	
	checkEquals(result,positions)
	checkEquals(length(result$positions),2)

	# reset the repository in the original state
	repositories$exchangeRates <- repository
}


test.shouldCheckOneCheckStringOnPositions <- function() {
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=100/0.9627#,
			#origin=list(ID_AAA=10)
	)
	class(position1) <- c("equity",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="AAA",
			currency="CHF",
			amount=100#,
			#origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create position 3	
	position3 <- create_position()
	position3$create(name="AAA",
			currency="EUR",
			amount=100/1.33853808#,
			#origin=list(ID_AAA=10)
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
	
	# Test 1
	checkString <- scan(file="./unitTests/data/criteriSelezionePerPortafoglioTest.txt",
			what="character",sep="\n",quiet = TRUE)
	
	checkEquals(checkString,"instrument:bond & currency:CHF ; > 5%")
	
	# Test 2
	# redefine the checkString
	checkString="instrument:bond & currency:CHF ; > 30%"

	result <- checkCheckStringOnPositions(checkString,positions)
	checkEquals(result$checkResult,TRUE)
	checkEquals(result$percentageValue,"30.00%")
	
	# Test 3
	# use an absolute constraint
	checkString="instrument:bond & currency:CHF ; >= 30CHF"
	checkEquals(result$checkResult,TRUE)
	checkEquals(result$percentageValue,"30.00%")
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
}


# questa parte di test riguarda i fondi nostri da esplodere nei portafogli cliente
test.shouldIdentifyFundsToExplode <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")

	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")
	
	origin1 <- list("pippo123","A","CHF",2000,"2742261CH",
			"CB-Accent Global Equity Fund Cap B",77.3339998779297,73.74,147480,
			110510.651752904,-0.0464737357902443,"E","Azioni e simili",
			147480,110510.651752904,154283.920912229,1701,14,NA,NA,NA,NA)
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
	
	origin4 <- list("pippo51","A","CHF",100,"11995588CH",
			"CB-Accent Global Economy",100,92.63,9263,8935.55,-0.07,"E",
			"Azioni e simili",9263,8935.55,12826.09,2256,14,NA,NA,NA,NA)
	names(origin4) <- nomi
	
	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	
	
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
	
	fundsDb <- create_fundsDB()
	
	# identify GLOBAL EQUITY
	fundData <- as.list(fundsDb[1,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(TRUE,FALSE,FALSE,FALSE))
	
	# identify FIXED INCOME fund (without accruedInterests)
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,TRUE,FALSE))
	
	# identify GLOBAL ECONOMY
	fundData <- as.list(fundsDb[2,,drop=FALSE])
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,FALSE,TRUE))
	
	# identify nothing
	fundData <- as.list(fundsDb[1,,drop=FALSE])
	fundData["id"] <- -13949
	result <- identifyFundsToExplode(fundData,positions)
	checkEquals(result,c(FALSE,FALSE,FALSE,FALSE))
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	
}


test.shouldIdentifyCB_Fixed_Income_AccruedInterest <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")
	
	origin1 <- list("pippo123","A","CHF",2000,"2742261CH",
			"CB-Accent Global Equity Fund Cap B",77.3339998779297,73.74,147480,
			110510.651752904,-0.0464737357902443,"E","Azioni e simili",
			147480,110510.651752904,154283.920912229,1701,14,NA,NA,NA,NA)
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
	
	origin4 <- list("pippo51","A","CHF",100,"11995588CH",
			"CB-Accent Global Economy",100,92.63,9263,8935.55,-0.07,"E",
			"Azioni e simili",9263,8935.55,12826.09,2256,14,NA,NA,NA,NA)
	names(origin4) <- nomi
		
	# initialize the different repositories
	allocateTestRepositories("equities")
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	allocateTestRepositories("exchangeRates")
	
	
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
	
	fundsDb <- create_fundsDB()
	fundData <- as.list(fundsDb[3,,drop=FALSE])
	result <- identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc(positions)

	checkEquals(result,c(FALSE,TRUE,FALSE,FALSE))
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("equities")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
	
}


test.shouldFailToApplyDirectiveString <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# crea position di tipo accrued interest e Fondo_misto
	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")
	
	origin1 <- list("pippo16","A","EUR",4477.2698043909,"",
			"25.3-74.7 Fondo misto di test",NA,NA,4477.2698043909,4477.2698043909,
			NA,"A","Fondo misto",5975.0597818207,4477.2698043909,
			6250.71637391013,12,26,NA,NA,NA,NA)
	names(origin1) <- nomi
	
	
	origin2 <- list("pippo16","Oacc","EUR",0,"2490099",
			"20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata",
			NA,NA,0,0,NA,"D","Obbligazioni e simili          ",0,0,0,825,
			2,NA,NA,NA,NA)
	names(origin2) <- nomi
	
	# initialize the different repositories
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
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	
	# Test 1: positions argument not of class positions
	checkException(applyDirectiveString(directiveString=NA,positions="pippo"))
		
	
	# Test 2: directiveString="dadd" -> stop
	checkException(applyDirectiveString(directiveString="dadd",positions))
	
		
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
}


test.shouldApplyDirectiveString <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# crea position di tipo accrued interest e Fondo_misto
	nomi <- c("Cliente","Strumento","Moneta","Saldo","NumeroValore",
			"Nome","PrezzoMedio","PrezzoMercato","ValorePosizione",
			"ValoreMonetaRiferimento","Evoluzione","Ordine","Categoria",
			"ValoreMercatoMonetaCHF","ValoreMercatoMonetaEUR",
			"ValoreMercatoMonetaUSD","ID_AAA","ID_strumento",
			"ID_transazione","Ticker_indice_per_BETA","Valuta",
			"VariazioneFX")

	origin1 <- list("pippo16","A","EUR",4477.2698043909,"",
			"25.3-74.7 Fondo misto di test",NA,NA,4477.2698043909,4477.2698043909,
			NA,"A","Fondo misto",5975.0597818207,4477.2698043909,
			6250.71637391013,12,26,NA,NA,NA,NA)
	names(origin1) <- nomi
	
	
	origin2 <- list("pippo16","Oacc","EUR",0,"2490099",
			"20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata",
			NA,NA,0,0,NA,"D","Obbligazioni e simili          ",0,0,0,825,
			2,NA,NA,NA,NA)
	names(origin2) <- nomi

	# initialize the different repositories
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
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)

	
	# Test 1: directiveString=NA -> return identical positions
	directiveString <- NA
	results <- applyDirectiveString(directiveString,positions)
	
	checkEquals(results,positions)
	

	# Test 2: esplodi le posizioni
	directiveString <- "explode:Fondi_misti"
	newPositions <- applyDirectiveString(directiveString,positions)

	checkEquals(class(newPositions$positions[[1]]),c("Fondi_azionari","position"))
	checkEquals(class(newPositions$positions[[2]]),c("Fondi_obbligazionari","position"))	
	total <- newPositions$positions[[1]]$money$sum(newPositions$positions[[2]]$money)
	checkEquals(total,position1$money$amount)
	checkEquals(newPositions$positions[[3]],positions$positions[[2]])
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
}



test.shouldReweightPositions <- function() {
	source("./lib/repository.R")
	source("./unitTests/utilities/allocateTestRepositories.R")
	
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
			"CB-Accent Global Equity Fund Cap B",99.9755741239983,73.74,
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

