# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_character_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(ls(portfolio),c("add","owner","positions","refCurrency"))
	
}


test.should_addPositionToPortfolio <- function() {
	source("./lib/portfolio.R")
	
    p <- create_position()
	p$create(name="pippo",
			currency="CHF",
			amount=0.1,
			origin=list(ID_AAA=10)
	)	
	
	portfolio <- create_portfolio()
	portfolio$add(p)
    
	checkEquals(portfolio$positions$positions[[1]]$name,"pippo")
	
}