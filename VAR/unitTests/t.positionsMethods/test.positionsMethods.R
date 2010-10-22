# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldExtractPositionsByCurrency <- function() {
	
	load("./lib/positionsMethods.R")
	
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
	
	necessito della variabile di classe criterium!
	
	
	positionSelector <- function(criterium,positions) {
		if (class(criterium=="currency")) return()
	}
	
	positionsSelector.currency <- function(currency,positions) {
		
		trueFalse <- lapply(positions,
				function(position,currency) return(position$currency==currency),
				currency)
		trueFalse <- unlist(trueFalse)
		
		return(positions[trueFalse])
	}
	
	checkEquals()
}
