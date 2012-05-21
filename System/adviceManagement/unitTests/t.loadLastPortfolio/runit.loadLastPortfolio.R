# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldLoadLastPortfolio <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadLastPortfolio")
	loadLastPortfolio(directory)
	
	checkEquals(portfolio,3)
	
	directory <- file.path(systemOptions[["sourceCodeDir"]])
	checkException(loadLastPortfolio(directory))
	
}
