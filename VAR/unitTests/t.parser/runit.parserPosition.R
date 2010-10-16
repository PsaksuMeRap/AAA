# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.parseEquityPosition <- function() {
	
	parser <- create_parserPosition()
	
<<<<<<< HEAD:VAR/unitTests/parser/runit.parserPosition.R
	source("./unitTests/testUtilities/createPositionsData.R")
	record.df <- createPositionsData()
=======
	source("./unitTests/utilities/createEquityDataFrame.R")
	record.df <- createEquityDataFrame()
>>>>>>> cf01df5687545e21475ec33191e9f2d7985cecfa:VAR/unitTests/t.parser/runit.parserPosition.R
	
	equity <- parser$parse(record.df[2,])
	
	checkEquals(is.element("equities",class(equity)),TRUE) 
}

test.parseBondPosition <- function() {
	
	parser <- create_parserPosition()
	
	source("./unitTests/testUtilities/createPositionsData.R")
	record.df <- createPositionsData()
	
	bond <- parser$parse(record.df[700,])
	
	checkEquals(is.element("???",class(bond)),TRUE) 
}
