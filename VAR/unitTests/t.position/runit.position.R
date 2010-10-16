# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_position <- function() {
	source("./lib/position.R")
	source("./lib/repository.R")
	
	# crea l'equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	repositories <<- new.env()
	repositories$equities <- create_repositoryEquities(equities.df)
	rm(equities.df)
	
	# crea la posizione
	position <- create_position()
	
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	checkEquals(class(position),"position")
	checkEquals(position$isMemberOf("position"),TRUE)
	checkEquals(position$isMemberOf("positions"),FALSE)
	
	position$extendEquities()
	checkEquals(position$ticker,"ROG")
	rm(repositories)
}

test.shouldAddPositionToPositions <- function() {
	source("./lib/position.R")
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
	source("./lib/position.R")
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
	source("./lib/position.R")
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


test.shouldFailWithNonPosition <- function() {
	source("./lib/position.R")
	
	positions <- create_positions()
	
	checkException(positions$add("pippo"))

}

test.shouldCreateDataFrameFromPosition <- function() {
	source("./lib/position.R")
	
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position) <- c("equities",class(position))
	
	df <- data.frame(instrument="equities",name="test",currency="USD",amount=0.0,
			stringsAsFactors=FALSE)
	
	checkEquals(position$toDataFrame(),df)
}


test.shouldCreateDataFrameFromPositions <- function() {
	
	source("./lib/position.R")
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
	
	source("./lib/position.R")
	
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