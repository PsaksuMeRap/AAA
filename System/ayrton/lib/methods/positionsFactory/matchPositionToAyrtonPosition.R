# TODO: Add comment
# 
# Author: claudio
###############################################################################


matchPositionToAyrtonPosition <- function(ayrtonPosition,position){
	# this function match an ayrtonPosition to a position having a quantity 
	# of class money with respect to the fields ID_AAA, ID_strumento and 
	# Saldo slots

	if (is.element(ayrtonPosition@ID_strumento,c(6,7))) {
		# The instrument is Anticipi_fissi
		# identify the instrument by name
		lenghtName <- nchar(ayrtonPosition@Nome)
		nameWithoutProRata <- substr(ayrtonPosition@Nome,1,lengthName-9)
		if (ayrtonPosition@Nome == nameWithoutProRata) return(TRUE) else return(FALSE)
	}	
	
	return(identical(position@security@id@idAAA,ayrtonPosition) &
			identical(position@security@id@idStrumento,ayrtonPosition@ID_strumento) &
			matchPositionQuantityToAyrtonPosition(position@quantity,ayrtonPosition))
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