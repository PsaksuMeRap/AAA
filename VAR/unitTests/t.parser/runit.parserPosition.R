# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.parseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	# create the equity repository
	source("./lib/repository.R")
	repository <- create_repositoryEquities(equities.df)
    
	# create the instrument repository
	# to be done!!!!!!
	
	load("./unitTests/data/dati.df_RData")
	
	equity <- parser$parse(dati.df[2,])
	
	checkEquals(is.element("equities",class(equity)),TRUE) 
}


#test.parseBondPosition <- function() {
	
#	parser <- create_parserPosition()
	
#	source("./unitTests/testUtilities/createPositionsData.R")
#	record.df <- createPositionsData()
	
#	bond <- parser$parse(record.df[700,])
	
#	checkEquals(is.element("???",class(bond)),TRUE) 
#}
