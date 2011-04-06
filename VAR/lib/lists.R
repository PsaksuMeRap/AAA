# TODO: Add comment
# 
# Author: claudio
###############################################################################


filterLists <- function(origin,by,value) {
	# origin: una lista di tipo origin o una lista i cui elementi,
	# notati x, hanno il campo x[[by]]
	# by: il nome del campo su cui applicare il filtro
	# value: il valore del campo da filtrare
	# output: the subset of origin with match the value
	
	areOkFinal <- rep(FALSE,length(origin))
	
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
	# origin: una lista di tipo origin
	# fieldName: il nome del campo i cui valori sono da estrarre
	# output: il vettore dei valori
	
	extract <- function(x,fieldName) return(x[[fieldName]])
	return(sapply(origin,extract,fieldName))

}
