# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.parseEquityPosition <- function() {
	
	parser <- create_parserPosition()
	
	source("./unitTests/testUtilities/createEquityDataFrame.R")
	record.df <- createEquityDataFrame()
	
	equity <- parser$parse(record.df[1,])
	
	checkEquals(is.element("equities",class(equity)),TRUE) 
}
