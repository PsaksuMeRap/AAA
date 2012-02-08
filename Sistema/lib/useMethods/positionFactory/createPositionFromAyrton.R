# TODO: Add comment
# 
# Author: claudio
###############################################################################


createPositionFromAyrton <- function(security,origin) UseMethod("createPositionFromAyrton")

createPositionFromAyrton.default <- function(security,origin) {
	
	id <- 10.2
	owner <- origin[["Cliente"]]
	quantity <- origin[["Saldo"]]
	value <- toMoney(origin[["ValoreMercatoMonetaCHF"]],origin[["Moneta"]])
	position <- new("Position",id=id,owner=owner,security=security,
			quantity=quantity,value=value)
	
	return(position)
}

createPositionFromAyrton.NULL <- function(security,origin) {
	
	# if security is of class NULL, i.e. from an AccruedInterest
	# return NULL
	return(NULL)
}