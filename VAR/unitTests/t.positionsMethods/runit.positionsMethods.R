# TODO: Add comment
# 
# Author: claudio
###############################################################################


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
		
	criterium <- create_selectionCriterium(factor="currency",values=c("EUR","USD"))
	
	result <- positionsSelector(criterium,positions)
	
	posCheck <- c(TRUE,FALSE,TRUE)

	checkEquals(result,posCheck)
	
	# all false
	posCheck <- create_positions()
	criterium <- create_selectionCriterium(factor="currency",values="abc")
	result <- positionsSelector(criterium,positions)
	posCheck <- rep(FALSE,3)
	checkEquals(result,posCheck)
	
	# empty positions as argument
	positions <- create_positions()
	criterium <- create_selectionCriterium(factor="currency",values="abc")
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
	criterium <- create_selectionCriterium(factor="instrument",values=c("bond"))
	
	result <- positionsSelector(criterium,positions)
	
	posCheck <- c(FALSE,TRUE,FALSE)
	
	checkEquals(result,posCheck)
	
	# two positions
	posCheck <- c(FALSE,TRUE,TRUE)
	
	criterium <- create_selectionCriterium(factor="instrument",values=c("ABC","bond"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)

	
	# zero positions
	posCheck <- c(FALSE,FALSE,FALSE)
	
	criterium <- create_selectionCriterium(factor="instrument",values=c("abc"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
	
	# return NULL
	posCheck <- NULL
	positions <- create_positions()
	criterium <- create_selectionCriterium(factor="instrument",values=c("abc"))
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
}

test.shouldExtractPositionsByAmount <- function() {
	source("./lib/money.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=100.1*0.9627,
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
			amount=100.1*1.33853808,
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
	
	
	# one position
	criterium <- create_selectionCriterium(factor="amount",values=toMoney(100.1,"CHF"),type=">")
	
	result <- positionsSelector(criterium,positions)
	print(result)
	posCheck <- c(FALSE,FALSE,FALSE)
	
	checkEquals(result,posCheck)
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
	
}


