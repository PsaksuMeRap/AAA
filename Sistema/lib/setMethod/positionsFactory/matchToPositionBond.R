# TODO: Add comment
# 
# Author: claudio
###############################################################################


matchToPositionBond <- function(ayrtonPosition,position){
	# this function match an ayrtonPosition to a position having a quantity 
	# of class money with respect to the fields ID_AAA, ID_strumento and 
	# Saldo slots
	
	# accruedInterestPosition: an AyrtonPosition of type accruedInterest
	# positionBond: a variable of class PositionBond
	
	return(
		identical(position@security@id@idAAA,ayrtonPosition@ID_AAA) &
		identical(position@security@id@idStrumento,ayrtonPosition@ID_strumento) &
		identical(as.numeric(position@quantity@amount),ayrtonPosition@Saldo)
	)
}
