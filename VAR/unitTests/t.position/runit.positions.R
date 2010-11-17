# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldAddPositionToPositions <- function() {
	source("./lib/position/position.R")
	
	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	positions <- create_positions()
	positions$add(position)
	
	checkEquals(positions$positions[[1]],position)
}

test.shouldAdd2PositionsToPositions <- function() {
	source("./lib/position/position.R")
	# crea la posizione
	
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	
	checkEquals(length(positions$positions),2)
	checkEquals(positions$positions[[1]],position1)
	checkEquals(positions$positions[[2]],position2)
}


test.shouldAddPositionsToPositions <- function() {
	source("./lib/position/position.R")
	# crea la posizione
	
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	
	positions_new <- create_positions()
	positions_new$add(positions)
	
	checkEquals(length(positions_new$positions),2)
	checkEquals(positions_new$positions,positions$positions)	
	checkEquals(positions_new$positions[[1]],position1)
	checkEquals(positions_new$positions[[2]],position2)
}

test.shouldExtractReturnEmptypositions <- function() {
	source("./lib/position/position.R")
	# crea la posizione
	
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	# with a non empty positions
	positions <- create_positions()
	positions$add(position1)
	
	result <- positions$extract(NULL)
	check <- create_positions()
	checkEquals(result,check)
	
	# with an empty positions
	positions <- create_positions()
	
	result <- positions$extract(NULL)
	check <- create_positions()
	checkEquals(result,check)
	
}

test.shouldExtractPositions <- function() {
	source("./lib/position/position.R")
	# crea la posizione
	
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	
	result <- positions$extract(c(TRUE,TRUE))

	# extract all positions
	checkEquals(length(result$positions),2)
	checkEquals(result$positions,positions$positions)	
	checkEquals(result$positions[[1]],position1)
	checkEquals(result$positions[[2]],position2)
	
	# extract one position
	result <- positions$extract(c(FALSE,TRUE))
	positions$remove(1)
	checkEquals(result,positions)

}

test.shouldFailExtracting <- function() {
	source("./lib/position/position.R")
	# crea la posizione
	
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	positions <- create_positions()
	positions$add(position1)
	
	# generate error because of differing length
	checkException(positions$extract(c(TRUE,TRUE)),silent=TRUE)
}

test.shouldRemoveOnePosition <- function() {
	source("./lib/position/position.R")
	# crea la posizione

	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	

	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$remove(1)
	
	positions_new <- create_positions()
	positions_new$add(position2)
		
	checkEquals(positions_new,positions)


	
	
}

test.shouldRemoveTwoPositions <- function() {
	source("./lib/position/position.R")
	# crea la posizione

	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	position3 <- create_position()
	position3$create(name="pippo2",
			currency="CHF",
			amount=0.8,
			origin=list(ID_AAA=11)
	)
	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)
	positions$remove(c(1,3))	
	
	positions_new <- create_positions()
	positions_new$add(position2)
	
	checkEquals(positions_new,positions)

	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)
	positions$remove(c(TRUE,FALSE,TRUE))	
	
	checkEquals(positions_new,positions)	
	
}

test.isCurrency <- function() {
	source("./lib/position/position.R")
	
	# crea la posizione
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	
	# check 1
	checkEquals(positions$isCurrency("CHF"),list(FALSE,TRUE))

	# check 2
	positions <- create_positions()
	checkEquals(positions$isCurrency("CHF"),list())
}


test.shouldSumPositions <- function() {
	source("./lib/position/position.R")
	
	# crea la posizione
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=10)
	)
	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=200,
			origin=list(ID_AAA=10)
	)	

	position3 <- create_position()
	position3$create(name="pop",
			currency="EUR",
			amount=300,
			origin=list(ID_AAA=10)
	)	
	
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)
	positions$add(position3)
	
	# select the testExchangeRate repository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	
	
	# check 0 (no argument should return sum in "USD"
	# because first position in USD)
	shouldBe <- 100+200/0.9627+300*1.33853808/0.9627
	checkEquals(positions$sum(),toMoney(shouldBe,"USD"))
	
	# check 1
	shouldBe <- 100*0.9627+200+300*1.33853808
	checkEquals(positions$sum("CHF"),toMoney(shouldBe,"CHF"))
	
	# check 2
	shouldBe <- 100*0.9627/1.33853808+200/1.3385380+300
	checkEquals(positions$sum("EUR"),toMoney(shouldBe,"EUR"))
	
	# check 3 
	positions <- create_positions()
	checkEquals(positions$sum("EUR"),toMoney(0,"EUR"))
	
	# check 4 (0 positions should return 0 "CHF")
	positions <- create_positions()
	checkEquals(positions$sum(),toMoney(0,"CHF"))
	
	repositories$exchangeRates <- repository
}


test.shouldFailWithNonPosition <- function() {
	source("./lib/position/position.R")
	
	positions <- create_positions()
	
	checkException(positions$add("pippo"),silent=TRUE)

}


test.shouldCreateDataFrameFromPositions <- function() {
	
	source("./lib/position/position.R")
	# create position 1
	position1 <- create_position()
	position1$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equities",class(position1))
	
	# create position 2	
	position2 <- create_position()
	position2$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)
	class(position2) <- c("bond",class(position2))
	
	# create positions
	positions <- create_positions()
	positions$add(position1)
	positions$add(position2)

	df <- data.frame(instrument=c("equities","bond"),
			name=c("test","pippo"),
			currency=c("USD","CHF"),
			amount=c(0.0,0.1),
			stringsAsFactors=FALSE)
	
	checkEquals(positions$toDataFrame(),df)
}

test.shouldOrderPositions <- function() {
	
	source("./lib/position/position.R")
	
	# create position 1
	position1 <- create_position()
	position1$create(name="xxx",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position1) <- c("equities",class(position1))
	
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
	
	posSorted <- create_positions()
	posSorted$add(position2)
	posSorted$add(position3)
	posSorted$add(position1)		

	checkEquals(positions$sortBy(c("name","currency","amount")),posSorted$positions)
}

 