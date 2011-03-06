# TODO: Add comment
# 
# Author: claudio
###############################################################################


filterLists <- function(origin,by,value) {
	# origin: una lista i cui elementi,
	# notati x, hanno il campo x[[by]]
	# by: il nome del campo su cui applicare il filtro
	# value: il/i valore/i del campo da estrarre
	# output: the subset of origin with match the value
	
	filter <- function(x,by,value) {
		return(x[[by]] == value)
	}	

	areOk <- rep(FALSE,length(origin))
	for (v in value) {
		areOk <- areOk | sapply(origin,filter,by,value=v)
		
	}
	
	return(origin[areOk])
	
}

extractLists <- function(origin,fieldName) {
	# origin: una lista di tipo origin
	# fieldName: il nome del campo i cui valori sono da estrarre
	# output: il vettore dei valori
	
	extract <- function(x,fieldName) return(x[[fieldName]])
	return(sapply(origin,extract,fieldName))

}
