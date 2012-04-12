# TODO: Add comment
# 
# Author: claudio
###############################################################################


extractFromList <- function(x,fieldName) {
	# x: a list
	# fieldName: the name of the field or slot whose values have to extracted
	
	l <- length(x)
	if (l==0) return(list())
	
	if (isS4(x[[1]])) {
		return(sapply(x,slot,fieldName))
	} else {
		extract <- function(x,fieldName) return(x[[fieldName]])
		return(sapply(x,extract,fieldName))
	}
}


filterLists <- function(x,by,values) {
	# x: a list the element of which, denoted by y,
	# are lists or objects of class S4 having the field y[[by]] or
	# the slot y@by, respectively
	# by: the slot name on which apply the filter
	# values: the values of the field/slot by on which apply the filter
	# output: a list, i.e. the subset of x with match "value"
	
	l <- length(x)
	
	if (l==0) return(list())
	
	areOkFinal <- rep(FALSE,length(l))	
	
	listValues <- extractFromList(x,by)
	
	for (value in values) {
		areOk <- sapply(listValues,identical,value)
		areOkFinal <- areOkFinal | areOk
	}
	return(x[areOkFinal])
	
}
