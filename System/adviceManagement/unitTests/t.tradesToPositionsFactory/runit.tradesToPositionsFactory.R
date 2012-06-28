# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCreatePositionsFromTrades <- function() {
	# create the BloombergData

	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","utilities")
	source(file.path(directory,"createRepositoryBloombergData.R"))
	assign("bloombergData",createRepositoryBloombergData(),pos=repositories)
	
	# set the fileName from which to import trades
	fileName <- "2012-06-19_14-27-47_Ortelli_globalEconomy_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradesToPositionsFactory") 
	
	positions <- tradesToPositionsFactory(fileName,directory)
	
	checkEquals(length(positions),6)
	checkEquals(as.character(positions[[3]]@id),"UBSN VX Equity")
	
	rm("bloombergData",pos=repositories)
}


