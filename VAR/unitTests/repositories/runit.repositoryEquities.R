# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryEquities <- function() {
	
	source("./unitTests/testUtilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	source("./unitTests/testUtilities/createEmptyEquityDataFrame.R")
	emptyEquities.df <- createEmptyEquityDataFrame()
			
	repository <- create_repositoryEquities(equities.df)
	
	checkEquals(class(repository),"repositoryEquityTicker")
	
	checkEquals(repository$tickerFromId(1),"NESN")
	checkEquals(repository$tickerFromId(10),"ROG")
	checkEquals(repository$tickerFromId(3),NA_character_)
	
	# with an empty repository
	equities.df <- createEmptyEquityDataFrame()
	
	repository <- create_repositoryEquities(equities.df)	
	checkEquals(repository$tickerFromId(1),NA_character_)
	
	emptyRepository <- create_repositoryEquities(emptyEquities.df)
	checkEquals(emptyRepository$tickerFromId(1),NA_character_)

}
