# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_selectionCriterium <- function(factor=NA_character_,
		values=NA_character_) {
	selectionCriterium <- list()
	if (!is.na(factor)) {
		classes <- c(factor,"selectionCriterium") 
	} else {
		classes <- "selectionCriterium" 
	} 
	class(selectionCriterium) <- classes
	selectionCriterium$factor <- factor
	selectionCriterium$values <- values
	
	return(selectionCriterium)	
}

