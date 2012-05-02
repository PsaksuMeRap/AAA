# TODO: Add comment
# 
# Author: claudio
###############################################################################


explodePositionsByFund <- function(fundData,fundPortfolios,positions) {
	# fundData: a list containing 4 fields: $nomeFondo, $instrumentClass, $id e $owner,
	#           Example: "FIXED INCOME", "Fondi_obbligazionari", "2490099", "pippo76"
	# fundPortfolios: a variable of class portfolios with the funds
	# positions: the positions to be exploded
	# This procedure returns the positions invested in the fund described in fundData
	# exploded by the fundPositions.
	
	# determine the number of positions
	nbPositions <- length(positions)
	
	# se positions è vuoto termina
	if (nbPositions==0) return(invisible())

	# select the positions invested into the fund
	result <- identifyPositionsToExplode(fundData,positions)
	
	# exit if no positions
	if (!any(result)) return()
	
	# save the positions to explode
	positionsToExplode <- positions[result]	
	
	# identify the fund to use
	owner <- fundData@owner
	fundPortfolio <- filterLists(fundPortfolios,by="owner",value=owner)[[1]]
	
	# compute the relative weight of every position w.r.t. the fund NAV
	fundNav <- sum(fundPortfolio)
	finalPositions <- list()
	
	for (position in positionsToExplode) {
		weight <- position@value / fundNav
		
		# reweight the fundPositions
		newPositions <- lapply(fundPortfolio,reweight,weight)
		
		# add the positions
		finalPositions <- c(finalPositions,newPositions)
	}
	
	return(new("Positions",finalPositions))
}

explodePositionsByFunds <- function(positions,fundsDb,fundPortfolios) {
	# create the weighted positions of the funds
	positions <- unlist(
			lapply(fundsDb,explodePositionsByFund,fundPortfolios,positions),
			recursive = FALSE)

	return(new("Positions",positions))
	
}

explodePortfolioByFunds <- function(portfolio,fundsDb,fundPortfolios) {	

	lengthPortfolio <- length(portfolio)
	if (lengthPortfolio==0) return(portfolio)
	
	
	# determine which portfolio's positions must be exploded
	# Step1: create the list of vectors of logical values indicating if a portfolio position
	# must be exploded
	toRemove.l <- lapply(fundsDb,identifyPositionsToExplode,portfolio)

	toRemove <- rep(FALSE,lengthPortfolio)
	for (remove in toRemove.l) toRemove <- toRemove | remove

	# add the exploded positions to the portfolio
	if (any(toRemove)) {
		explodedPositions <- explodePositionsByFunds(portfolio,fundsDb,fundPortfolios) 
		portfolio@.Data <- new("Positions",c(portfolio[!toRemove],explodedPositions))
	}
	return(portfolio)
}


explodeAllPortfoliosByAllFunds <- function(portfolios) {
	fundsDb <- create_fundsDB()
	fundPortfolios <- filterLists(portfolios,by="owner",value=extractFromList(fundsDb,"owner"))
	
	invisible(new("Portfolios",lapply(portfolios,explodePortfolioByFunds,fundsDb,fundPortfolios=fundPortfolios)))
}

