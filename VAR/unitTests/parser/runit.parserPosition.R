# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.parseEquityPosition <- function() {
	
	parser <- create_parserPosition()
	
	source("./unitTests/testUtilities/createPositionsData.R")
	record.df <- createPositionsData()
	
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
