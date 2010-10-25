# TODO: Add comment
# 
# Author: claudio
###############################################################################



positionsSelector <- function(criterium,positions) UseMethod("positionsSelector",criterium)

positionsSelector.currency <- function(criterium,positions) {
	if (!is.element("positions",class(positions))) stop("The argument is not of class positions")

	FUNC <- function(position,criterium) {
		is.element(position$currency,criterium$values)	
	}
	
	# apply the function FUNC
	extract <- lapply(positions$positions,FUNC,criterium)
	extract <- unlist(extract)
	
	positions_new <- create_positions()

	positions_new$positions <- positions$positions[extract]

	return(positions_new)
}

positionsSelector.default <- function(criterium) {
	print(criterium)
}

# analizza i nomi e guarda se funzionano correttamente. crea un repository per le
# criteriumClass con i rispettivi valori? Esempio
# instruments: equity, bond, ...
# currency: usd, chf, eur
# geograficArea: EU, USA, Canada, Asia, Japan



