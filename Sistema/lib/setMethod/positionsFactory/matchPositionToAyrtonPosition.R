# TODO: Add comment
# 
# Author: claudio
###############################################################################


matchPositionToAyrtonPosition <- function(ayrtonPosition,position){
	# this function match an ayrtonPosition to a position having a quantity 
	# of class money with respect to the fields ID_AAA, ID_strumento and 
	# Saldo slots

	return(
		identical(position@security@id@idAAA,ayrtonPosition@ID_AAA) &
		identical(position@security@id@idStrumento,ayrtonPosition@ID_strumento) &
		matchPositionQuantityToAyrtonPosition(position@quantity,ayrtonPosition)
	)
}

setGeneric("matchPositionQuantityToAyrtonPosition",def=function(positionQuantity,ayrtonPosition) standardGeneric("matchPositionQuantityToAyrtonPosition"))

setMethod("matchPositionQuantityToAyrtonPosition",signature(positionQuantity="numeric",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(positionQuantity,ayrtonPosition@Saldo))	
		}
)

setMethod("matchPositionQuantityToAyrtonPosition",signature(positionQuantity="NominalValue",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(as(positionQuantity,"Money"),toMoney(ayrtonPosition@Saldo,ayrtonPosition@Moneta)))
		}
)