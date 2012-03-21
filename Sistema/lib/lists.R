# TODO: Add comment
# 
# Author: claudio
###############################################################################




isListFieldEqual <- function(origin,by,value) {
	# origin: una lista di tipo origin o una lista i cui elementi,
	# notati x, hanno il campo x[[by]]
	# by: il nome del campo su cui applicare il filtro
	# value: il valore del campo da filtrare
	# output: un vettore di vero/falso
	
	filter <- function(x,by,value) {
		return(x[[by]] == value)
	}	

	areOk <- sapply(origin,filter,by,value=value)
	
	return(areOk)
}


filterLists <- function(origin,by,value) {
	# origin: una lista di tipo origin o una lista i cui elementi,
	# notati x, hanno il campo x[[by]]
	# by: il nome del campo su cui applicare il filtro
	# value: il valore del campo da filtrare
	# output: the subset of origin with match the value
	
	l <- length(origin)
	
	if (l==0) return(list())
	
	areOkFinal <- rep(FALSE,length(l))
	
	filter <- function(x,by,value) {
		return(x[[by]] == value)
	}	
	
	for (v in value) {
		areOk <- sapply(origin,filter,by,value=v)
		areOkFinal <- areOkFinal | areOk
	}
	
	return(origin[areOkFinal])
	
}

extractFromList <- function(origin,fieldName) {
	# origin: una lista
	# fieldName: il nome del campo i cui valori sono da estrarre
	# output: il vettore dei valori
	
	extract <- function(x,fieldName) return(x[[fieldName]])
	return(sapply(origin,extract,fieldName))

}


extractSlotFromList <- function(x,fieldName) {
	# x: una lista
	# fieldName: il nome del campo i cui valori sono da estrarre
	
	return(sapply(x,slot,fieldName))
}


filterS4List <- function(x,by,value) {
	# x: a list the element of which are objects of class S4,
	# denoted by y, having the slot y@by
	# by: the slot name on which apply the filter
	# value: the value/s of the slot by on which apply the filter
	# output: a list, the subset of x with match the value
	
	l <- length(x)
	
	if (l==0) return(list())
	
	areOkFinal <- rep(FALSE,length(l))	
	
	for (v in value) {
		areOk <- sapply(sapply(x,slot,by),identical,v)
		areOkFinal <- areOkFinal | areOk
	}
	
	return(x[areOkFinal])
	
}


filterClassLists <- function(origin,by,value) {
	# origin: una lista i cui elementi,
	# notati x, sono una classe con il campo x@by
	# by: il nome del campo su cui applicare il filtro
	# value: il/i valore/i del campo da filtrare per cui eseguire la selezione
	# output: a list, the subset of origin with match the value
	
	l <- length(origin)
	
	if (l==0) return(list())
	
	areOkFinal <- rep(FALSE,length(l))
	
	filter <- function(x,by,value) {
		return(x[[by]] == value)
	}	
	
	for (v in value) {
		areOk <- sapply(origin,slot,by) == v
		areOkFinal <- areOkFinal | areOk
	}
	
	return(origin[areOkFinal])
	
}
