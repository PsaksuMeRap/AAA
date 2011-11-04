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
	
	if ((class(b)=="symbols" )) {
		if  (lenght(b)>0) {
			for(sym in b) {
				if (a$name==sym$name) {
					a$power=a$power+sym$power
					return(create_symbols(a))
				}	
			}
			b[[length(b)]] <- a
			return(b)
		} else {
			return(create_symbols(a))
		}
	}
	stop("Error in function '*': entered symbols are not a valid symbol/symbols")
}

"*.symbols" <- function(a,b) {
	if (class(b)=="symbol") {
		return(b*a)
	}
	
	if (class(b)=="symbols") {
		if (length(b)==0) return(a)
		if (length(a)==0) return(b)

		for(sym1 in b) {
			for (sym2 in a) {
				if (sym2$name==sym1$name) {
					sym2$power=sym2$power+sym1$power
					break
				} else {
					
				}
			}
		}
		b[[length(b)]] <- a
		return(b)
	}
	stop("Error in function '*': entered symbols are not a valid symbol/symbols")
}
