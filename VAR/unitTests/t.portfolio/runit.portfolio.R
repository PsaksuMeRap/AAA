# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_character_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(ls(portfolio),c("owner","positions","refCurrency"))
	
}