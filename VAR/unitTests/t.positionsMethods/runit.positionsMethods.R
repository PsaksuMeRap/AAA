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
	
	posCheck <- create_positions()
	posCheck$add(position1)
	posCheck$add(position3)
	
	checkEquals(result,posCheck)
	checkEquals(identical(result,positions),FALSE)
	
	# empty positions
	posCheck <- create_positions()
	criterium <- create_selectionCriterium(factor="currency",values="abc")
	result <- positionsSelector(criterium,positions)
	checkEquals(result,posCheck)
}
