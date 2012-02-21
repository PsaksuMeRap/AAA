test.shouldCreatePortfolio <- function() {

  	positions <- new("AyrtonPositions",list())
	owner <- "Pinowitch"
	refCurrency <- new("Currency","USD")

	result <- portfolioFactory(positions,owner,refCurrency)
	checkEquals(result@owner,owner)
	checkEquals(result@referenceCurrency,refCurrency)
	checkEquals(as(result@.Data,"Positions"),positionsFactory(positions))
	
}
