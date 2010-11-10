# TODO: Add comment
# 
# Author: claudio
###############################################################################


filterLists <- function(origin,by,value) {
	# origin: una lista di tipo origin
	# by: il nome del campo su cui applicare il filtro
	# value: il valore del campo da filtrare
	# output: the subset of origin with match the value
	
	filter <- function(x,by,value) {
		return(x[[by]] == value)
	}
	areOk <- sapply(origin,filter,by,value)
	
	return(origin[areOk])
}

extractLists <- function(origin,fieldName) {
	# origin: una lista di tipo origin
	# fieldName: il nome del campo i cui valori sono da estrarre
	# output: una lista vettore dei valori
	
	extract <- function(x,fieldName) return(x[[fieldName]])
	return(sapply(origin,extract,fieldName))

}
