# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/position.R")

create_portfolio <- function() {
	# uses positions
	
	portfolio <- new.env()
	class(portfolio) <- "portfolio"
	
	portfolio$owner <- NA_character_
	portfolio$refCurrency <- NA_character_
	
	portfolio$positions <- create_positions()
	
	return(portfolio)
}


