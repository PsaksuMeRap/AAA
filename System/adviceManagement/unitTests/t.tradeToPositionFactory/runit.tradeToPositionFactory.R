# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertTradeToPosition <- function() {
	# set the fileName from which to import trades
	fileName <- "equityTradeSimulation.csv"
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","files") 
	
	# import trades
	trades <- tradesFactory(fileName,directory)
	securityTrade <- trades[[1]]
	
	# determine the security type
	securityType <- securityTrade$Security.Type
	
	if (securityType=="Equity") {
		currency <- new("Currency",securityTrade$Currency)
		name <- securityTrade$Name
		id=bloombergID!!
				
		newSecurity <- new("Equity",currency=currency,name=name,id=id) 
		
		
	}
	checkequals(FALSE,TRUE)
}
