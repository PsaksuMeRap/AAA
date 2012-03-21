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
	owner <- fundData["owner"]
	fundPortfolio <- filterS4List(fundPortfolios,by="owner",value=owner)[[1]]
	
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
			apply(fundsDb,1,explodePositionsByFund,fundPortfolios,positions),
			recursive = FALSE)

	return(new("Positions",positions))
	
}

explodePortfolioByFunds <- function(portfolio,fundsDb,fundPortfolios) {	
	
	explodedPositions <- explodePositionsByFunds(portfolio,fundsDb,fundPortfolios) 
browser()	
	# create the list of vectors of logical values indicating if a portfolio position
	# must be exploded
	toRemove.list <- apply(fundsDb,1,identifyPositionsToExplode,portfolio)
	
	# determine which portfolio positions must be exploded
	toRemove <- rep(length(portfolio),FALSE)
	for (i in toRemove.list) toRemove <- toRemove | i
	
	# remove the original portfolio positions which have been exploded
	portfolio[toRemove] <- NULL
	
	# add the exploded positions to the portfolio
	portfolio@.Data <- new("Positions",c(portfolio@.Data,positions))
	return(portfolio)
}


explodeAllPortfoliosByAllFunds <- function(portfolios) {
	fundsDb <- create_fundsDB()
	invisible(lapply(portfolios,explodePortfolioByAllFunds,fundsDb,fundPortfolios=portfolios))
}
