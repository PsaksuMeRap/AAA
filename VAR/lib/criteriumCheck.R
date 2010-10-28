# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_criteriumCheck <- function(operator=NA_character_,
		value=NA,kind=NA_character_) {
	# - the operator, i.e. > or ==
	# - the value, i.e. 1450.75
	# - the kind, i.e. absolute or relative
	
	criteriumCheck <- list()
	class(criteriumCheck) <- "criteriumCheck"
	
	criteriumCheck$operator <- operator
	criteriumCheck$value <- value
	criteriumCheck$kind <- kind
	
	return(criteriumCheck)	
}


