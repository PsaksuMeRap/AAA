# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/position/position.R")

create_portfolio <- function() {
	# uses positions
	
	portfolio <- new.env()
	class(portfolio) <- "portfolio"
	
	portfolio$owner <- NA_character_
	portfolio$refCurrency <- NA_character_
	
	portfolio$positions <- create_positions()
	
	portfolio$add <- function(p) {
		# p: a position or an object of class positions
		if (is.element("position",class(p))) {
			portfolio$positions$addPosition(p)
			return()
		}
		
		if (is.element("positions",class(p))) {
			portfolio$positions$addPositions(p)
			return()
		}
		stop("Impossible to add p to portfolio: p is not of class position or positions")
		
	}
	
	portfolio$print <- function() {
		print(paste("Owner:",portfolio$owner))
		print(paste("Reference currency:",portfolio$refCurrency))
		print(paste("Positions:"))
		portfolio$positions$print()
	}
	
	portfolio$value <- function(currency) {
		return(portfolio$positions$sum(currency))
	}
	
	portfolio$addPortfolio <- function(portf) {
		if (portfolio$refCurrency!=portf$refCurrency) {
			stop(paste("The portfolios of",portfolio$owner,"and",
							portf$owner,"have different reference currency")
			)
		}
		portfolio$add(portf$positions)
	}
	return(portfolio)
}


