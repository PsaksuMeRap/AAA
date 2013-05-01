# TODO: Add comment
# 
# Author: claudio
###############################################################################


matchPositionToAyrtonPosition <- function(oaccPosition,positionWithAcc){
	# this function match an oaccPosition to a positionWithAcc having a quantity 
	# of class money with respect to the fields ID_AAA, ID_strumento and 
	# Saldo slots

	if (is.element(oaccPosition@ID_strumento,c(6,7))) {
		# The instrument is Anticipi_fissi or Depositi_a_termine
		# identify the instrument by name
		lengthName <- nchar(oaccPosition@Nome)
		nameWithoutProRata <- substr(oaccPosition@Nome,1,lengthName-9)
		if (positionWithAcc@security@name == nameWithoutProRata) return(TRUE) else return(FALSE)
	}	

	return(identical(positionWithAcc@security@id,idFactory(oaccPosition)) &
			matchPositionQuantityToOaccPosition(positionWithAcc@quantity,oaccPosition))
}

setGeneric("matchPositionQuantityToOaccPosition",def=function(positionQuantity,ayrtonPosition) standardGeneric("matchPositionQuantityToOaccPosition"))

setMethod("matchPositionQuantityToOaccPosition",signature(positionQuantity="numeric",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(positionQuantity,ayrtonPosition@Saldo))	
		}
)

setMethod("matchPositionQuantityToOaccPosition",signature(positionQuantity="NominalValue",ayrtonPosition="AyrtonPosition"),
		function(positionQuantity,ayrtonPosition) {
			return(identical(as(positionQuantity,"Money"),toMoney(ayrtonPosition@Saldo,ayrtonPosition@Moneta)))
		}
)