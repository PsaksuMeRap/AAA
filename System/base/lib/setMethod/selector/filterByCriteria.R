
filterByCriteriaLogicalAnd <- function(selectionCriteria,positions) {
	# selectionCriteria: a variable of class selectionCriteria, i.e. a list of selectionCriterium
	# positions: a variable of class positions
	
	if (length(positions)==0) return(vector(mode = "logical"))
	result <- lapply(selectionCriteria,selector,positions)
	
	if (length(result)==0) return(result)
	if (length(result)==1) return(result[[1]])
	
	x <- result[[1]]
	for (r in result[-1]) x <- x & r
	return(x)
}


filterByCriteriaLogicalOr <- function(listOfselectionCriteria,positions) {
	# listOfselectionCriteria: a list of selectionCriteria
	# positions: a variable of class positions
	
	if (length(positions)==0) return(vector(mode = "logical"))
	result <- lapply(listOfselectionCriteria,filterByCriteriaLogicalAnd,positions)
	
	if (length(result)==0) return(result)
	if (length(result)==1) return(result[[1]])
	
	x <- result[[1]]
	for (r in result[-1]) x <- x | r
	return(x)
}

