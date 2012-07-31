# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldLoadPortfolio <- function() {
	
	portfolioName <- "2012-05-21_17-40-03_portfolio.RData"

	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadPortfolio")
	portfolio <- loadPortfolio(portfolioName,directory)
	
	checkEquals(portfolio,2)
	
}


test.shouldLoadPortfolioWithPortfolioId <- function() {
	
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","t.loadPortfolio")
	portfolio <- loadPortfolio(directory=directory,portfolioId="globalEconomy")

	checkEquals(portfolio,3)
	
}