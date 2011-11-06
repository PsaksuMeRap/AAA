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

create_symbols <- function(symbol) {
		
	symbols <- list()
	class(symbols) <- "symbols"
	
	if (!missing(symbol)) symbols[[1]] <- symbol
	return(symbols)
}

sort.symbols <- function(symbols) {
	
	if (length(symbols)==0) return(symbols)
	
	names <- extractFromList(symbols,"name")
	order <- order(names)
	result <- symbols[order]
	class(result) <- "symbols"
	return(result)
}


"*.symbol" <- function(a,b) {
	
	if (class(b)=="symbol") {
		if (a$name==b$name) {
			a$power=a$power+b$power
			return(create_symbols(a))
		}	
		c <- create_symbols(a)
		c[[2]] <- b
		return(c)
	}
	
	stop("Error in function '*': entered symbols are not valid symbols")
}

"*.symbols" <- function(a,b) {
	
	if (length(b)==0) return(a)
	if (length(a)==0) return(b)
	
	tmp <- list()
	for(sym_b in b) {
		copy <- TRUE
		for (i in 1:length(a)) {
			if (sym_b$name==a[[i]]$name) {
				a[[i]]$power=a[[i]]$power+sym_b$power
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- sym_b
	}
	
	if (length(tmp)>0) { a <- c(a,tmp); class(a) <- "symbols" }
	return(a)
	stop("Error in function '*': entered symbols are not valid symbols")
}
