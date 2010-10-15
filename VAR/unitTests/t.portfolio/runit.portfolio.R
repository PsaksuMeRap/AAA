# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_charter_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(names(portfolio),c("owner","refCurrency","positions"))
	
}