# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("createPosition",
		useAsDefault=function(security,origin) {
			id <- 10.2
			quantity <- origin[["Saldo"]]
			value <- toMoney(origin[["ValoreMercatoMonetaCHF"]],origin[["Moneta"]])
			position <- new("Position",id=id,security=security,
					quantity=quantity,value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="NULL",origin="AyrtonPosition"),
		function(security,origin) {
			# if security is of class NULL, i.e. from an AccruedInterest
			# return NULL
			return(NULL)
		}
)

