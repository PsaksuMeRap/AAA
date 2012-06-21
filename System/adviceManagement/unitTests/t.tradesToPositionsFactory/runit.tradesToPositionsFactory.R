# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreatePositionsFromTrades <- function() {
	# create the BloombergData
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	blData <- createRepositoryBloombergData()
	
	# set the fileName from which to import trades
	fileName <- "mixedTrades.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradesToPositionsFactory") 
	
	positions <- tradesToPositionsFactory(file.path(directory,fileName))
	checkEquals(length(positions),6)
	checkEquals(as.character(positions[[3]]@id),"UBSN VX Equity")
}


