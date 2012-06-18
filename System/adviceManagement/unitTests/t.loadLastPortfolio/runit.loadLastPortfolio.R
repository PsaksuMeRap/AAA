# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldLoadLastPortfolio <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadLastPortfolio")
	portfolio <- loadLastPortfolio(directory)
	
	checkEquals(portfolio,3)
	
}


test.shouldFailToLoadLastPortfolio <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]])
	checkException(loadLastPortfolio(directory))
}



test.shouldLoadPortfolio <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadLastPortfolio")
	portfolio <- loadPortfolio(directory=directory)
	
	checkEquals(portfolio,3)
	
}



test.shouldLoadPortfolioWithPortfolioId <- function() {
	
	directory <- file.path(systemOptions[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadLastPortfolio")
	portfolio <- loadPortfolio(directory=directory,portfolioId="globalEconomy")
	
	checkEquals(portfolio,3)
	
}