# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldImportTradesFromCsv <- function() {
	
	fileName <- "2012-05-09_14-22-24_Ortelli_globalEconomy_newAdvice.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeFactory") 
	
	trades <- tradesFactory(fileName,directory)
	
	checkEquals(trades[[1]]$Portfolio,"GLOBAL ECONOMY")
	checkEquals(trades[[2]]$Buy.Sell,"Sell")
	checkEquals(trades[[1]]$Currency,"EUR")
}


test.shouldImportEmptyTradeCsv <- function() {
	
	fileName <- "2012-05-09_14-22-29_Ortelli_globalEquity_confirmation.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.tradeFactory") 

	trades <- tradesFactory(fileName,directory)
	
	checkEquals(length(trades),0)
}
