# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_selectionCriterium <- function(factor=NA_character_,
		values=NA_character_,type=NA_character_) {
	# factor: the class of the criterium, i.e. instrument, currency, money, ...
	# values: the values to verify, i.e. equity, USD, ...
	# type: the type of the selection criterium for money values, i.e. > or ==
	
	selectionCriterium <- list()
	if (!is.na(factor)) {
		classes <- c(factor,"selectionCriterium") 
	} else {
		classes <- "selectionCriterium" 
	} 
	class(selectionCriterium) <- classes
	selectionCriterium$factor <- factor
	selectionCriterium$values <- values
	selectionCriterium$type <- type
	
	return(selectionCriterium)	
}

