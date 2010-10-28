# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_criteriumSelection <- function(factor=NA_character_,
		values=NA,checkCriterium=NA) {
	# factor: the class of the criterium, i.e. instrument, currency, amount, ...
	# values: the values to verify, i.e. equity, USD, a currency (for factor amount)...
	# checkCriterium: an object of type checkCriterium, i.e. "< 5 %"
	
	criteriumSelection <- list()
	if (!is.na(factor)) {
		classes <- c(factor,"criteriumSelection") 
	} else {
		classes <- "criteriumSelection" 
	} 
	class(criteriumSelection) <- classes
	criteriumSelection$factor <- factor
	criteriumSelection$values <- values
	criteriumSelection$checkCriterium <- checkCriterium
	
	return(criteriumSelection)	
}

