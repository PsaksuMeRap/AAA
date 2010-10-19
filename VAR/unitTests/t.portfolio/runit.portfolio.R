# TODO: Add comment
# 
# Author: claudio
###############################################################################



test.should_createPortfolio <- function() {
	source("./lib/portfolio.R")
	
	portfolio <- create_portfolio()
	
	checkEquals(portfolio$owner,NA_character_)
	checkEquals(portfolio$refCurrency,NA_character_)
	checkEquals(ls(portfolio),c("add","owner","positions","print","refCurrency"))
	
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

test.should_addPositionsToPortfolio <- function() {
	source("./lib/portfolio.R")
	
	p1 <- create_position()
	p1$create(name="SUNCOR ENERGY  INC. CO",
			currency="USD",
			amount=243250,
			origin=list(ID_AAA=1984)
	)	
	p2 <- create_position()
	p2$create(name="Philips",
			currency="EUR",
			amount=22065.28,
			origin=list(ID_AAA=766)
	)
	p3 <- create_position()
	p3$create(name="E.ON",
			currency="EUR",
			amount=25356.00,
			origin=list(ID_AAA=445)
	)
	
	# crea le posizioni
	positions <- create_positions()
	positions$add(p1);positions$add(p2);positions$add(p3)
	
	portfolio <- create_portfolio()
	portfolio$add(positions)
	
	checkEquals(portfolio$positions$positions[[2]]$name,"Philips")

	checkIdentical(portfolio$positions$positions,positions$positions)
}