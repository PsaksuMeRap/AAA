# TODO: Add comment
# 
# Author: claudio
###############################################################################


matchPositionToAyrtonPosition <- function(accPosition,positionWithAcc){
	# this function match an accPosition to a positionWithAcc having a quantity 
	# of class money with respect to the fields ID_AAA, ID_strumento and 
	# Saldo slots

	if (is.element(accPosition@ID_strumento,c(6,7))) {
		# The instrument is Anticipi_fissi
		# identify the instrument by name
		lengthName <- nchar(accPosition@Nome)
		nameWithoutProRata <- substr(accPosition@Nome,1,lengthName-9)
		if (positionWithAcc@security@name == nameWithoutProRata) return(TRUE) else return(FALSE)
	}	

	return(identical(positionWithAcc@security@id@idAAA,accPosition) &
			identical(positionWithAcc@security@id@idStrumento,accPosition@ID_strumento) &
			matchPositionQuantityToAccPosition(positionWithAcc@quantity,accPosition))
}

setGeneric("matchPositionQuantityToAccPosition",def=function(positionQuantity,ayrtonPosition) standardGeneric("matchPositionQuantityToAccPosition"))

setMethod("matchPositionQuantityToAccPosition",signature(positionQuantity="numeric",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(positionQuantity,ayrtonPosition@Saldo))	
		}
)

setMethod("matchPositionQuantityToAccPosition",signature(positionQuantity="NominalValue",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(as(positionQuantity,"Money"),toMoney(ayrtonPosition@Saldo,ayrtonPosition@Moneta)))
		}
)