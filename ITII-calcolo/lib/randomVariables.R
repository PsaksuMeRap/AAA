# TODO: Add comment
# 
# Author: ortellic
###############################################################################



create_randomVariables <- function(randomVariable) {
	
	randomVariables <- list()
	class(randomVariables) <- "randomVariables"
	
	if (!missing(randomVariable)) randomVariables[[1]] <- randomVariable
	return(randomVariables)
}



"==.randomVariables" <- function(a,b) {
	la <- length(a)
	lb <- length(b)
	if (la != lb) return(FALSE)
	if (la + lb == 0) return(TRUE)
	
	result <- mapply(FUN="==",sort(a),sort(b))
	if (all(result)) return(TRUE) else return(FALSE)
}



sort.randomVariables <- function(randomVariables) {
	
	if (length(randomVariables)==0) return(randomVariables)
	
	names <- extractFromList(randomVariables,"name")
	lags <- extractFromList(randomVariables,"lag")
	
	order <- order(names,lags)
	result <- randomVariables[order]
	class(result) <- "randomVariables"
	return(result)
}


"*.randomVariables" <- function(a,b) {
	
	if (length(b)==0) return(a)
	if (length(a)==0) return(b)
	
	tmp <- list()
	for(rv_b in b) {
		copy <- TRUE
		for (i in 1:length(a)) {
			if (rv_b$name==a[[i]]$name & rv_b$lag==a[[i]]$lag) {
				a[[i]]$power=a[[i]]$power+rv_b$power
				copy <- FALSE
				break
			}
		}
		if (copy) tmp[[length(tmp)+1]] <- rv_b
	}
	
	if (length(tmp)>0) { a <- c(a,tmp); class(a) <- "randomVariables" }
	return(a)
	stop("Error in function '*.randomVariables': entered randomVariables are not valid randomVariables")
}



toString.randomVariables <- function(x) {
	
	result <- sapply(x,toString)
	result <- paste(result,collapse="*")
	return(result)
}
