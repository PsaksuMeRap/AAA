# TODO: Add comment
# 
# Author: ortellic
###############################################################################

create_symbol <- function(name="",power=1) {
	symbol <- list()
	class(symbol) <- "symbol"
	
	symbol$name <- name
	symbol$power <- power
	return(symbol)
}

"*.symbol" <- function(a,b) {
	
	if (a$name==b$name) {
		a$power=a$power+b$power
		return(create_symbols(a))
	}	
	c <- create_symbols(a)
	c[[2]] <- b
	return(c)
	
	#stop("Error in function '*.symbol': entered symbols are not valid symbols")
}

"==.symbol" <- function(a,b) {
	if (a$name != b$name) return(FALSE)
	if (a$power != b$power) return(FALSE)
	return(TRUE)
}

