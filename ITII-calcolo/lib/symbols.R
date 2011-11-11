# TODO: Add comment
# 
# Author: ortellic
###############################################################################


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

}


"==.symbols" <- function(a,b) {
	la <- length(a)
	lb <- length(b)
	if (la != lb) return(FALSE)
	if (la + lb == 0) return(TRUE)
	
	result <- mapply(FUN="==",sort(a),sort(b))
	if (all(result)) return(TRUE) else return(FALSE)
}


toString.symbols <- function(symbols){
		
	result <- sapply(symbols,toString)
	result <- paste(result,collapse="*")
	return(result)
}
