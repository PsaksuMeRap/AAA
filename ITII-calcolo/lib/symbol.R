# TODO: Add comment
# 
# Author: ortellic
###############################################################################

create_symbol <- function(name="a",power=1) {
	symbol <- list()
	class(symbol) <- "symbol"
	
	symbol$name <- name
	symbol$power <- as.integer(power)
	return(symbol)
}

"*.symbol" <- function(a,b) {
	
	if (a$name==b$name) {
		a$power=as.integer(a$power+b$power)
		return(create_symbols(a))
	}	
	c <- create_symbols(a)
	c[[2]] <- b
	return(sort(c))
}

"==.symbol" <- function(a,b) {
	return (identical(a,b))
}

toString <- function(x) UseMethod("toString",x)

toString.symbol <- function(symbol) {
	if (symbol$power==1) return(symbol$name)
	return(paste(symbol$name,"^",symbol$power,sep=""))
}
