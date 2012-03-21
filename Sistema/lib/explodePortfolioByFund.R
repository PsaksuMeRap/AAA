# TODO: Add comment
# 
# Author: claudio
###############################################################################


explodePortfolioByFund <- function(fundData,fundPortfolios,portfolio) {
	# fundData: a list containing 4 fields: $nomeFondo, $instrumentClass, $id e $owner,
	#           Example: "FIXED INCOME", "Fondi_obbligazionari", "2490099", "pippo76"
	# fundPortfolios: a variable of class portfolios with the funds
	# portfolio: a portfolio the positions of which are to be exploded
	# This procedure returns the positions invested in the fund described in fundData
	# exploded by the fundPositions.
	
	# determine the number of positions in the portfolio
	nbPositions <- length(portfolio)
	
	# se il portfolio è vuoto termina
	if (nbPositions==0) return(invisible())

	# select the positions corresponding to the fund
	result <- identifyPositionsToExplode(fundData,portfolio)
	
	# exit if no positions
	if (!any(result)) return()
	
	# save the positions to explode
	positionsToExplode <- portfolio[result]	
	
	# identify the fund to insert in the portfolio
	owner <- fundData["owner"]
	fundPortfolio <- filterS4List(fundPortfolios,by="owner",value=owner)[[1]]
	
	# compute the relative weight of every position w.r.t. the fund NAV
	fundNav <- sum(fundPortfolio)
	finalPositions <- list()
	
	for (position in positionsToExplode) {
		weight <- position@value / fundNav
		
		# reweight the fundPositions
		newPositions <- lapply(fundPortfolio,reweight,weight)
		
		# add the positions to the portfolio
		finalPositions <- c(finalPositions,newPositions)
	}
	
	return(new("Positions",finalPositions))
}

explodePortfolioByAllFunds <- function(portfolio,fundsDb,fundPortfolios) {
	# create the weighted positions of the funds
	positions <- unlist(
			apply(fundsDb,1,explodePortfolioByFund,fundPortfolios,portfolio),
			recursive = FALSE)
	
	# create the list of vectors of logical values indicating if a portfolio position
	# must be exploded
	toRemove.list <- apply(fundsDb,1,identifyPositionsToExplode,fundData,portfolio)
	
	# determine which portfolio positions must be exploded
	toRemove <- rep(lenght(portfolio),FALSE)
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
