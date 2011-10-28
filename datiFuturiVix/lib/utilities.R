# TODO: Add comment
# 
# Author: ortellic
###############################################################################



determineBusinessDate <- function(Date,nbDays) {
	# Date: a business date
	# a positive or negative integer indicating the number of days to move
	# questa funzione calcola la data lavorativa che segue o precede Date
	# di nbDays giorni

	if (nbDays==0) return(Date)

	dateSeq <- as.POSIXlt(seq(as.Date(Date),by=sign(nbDays),length.out=(2+abs(nbDays)/5)*7))
	isBusDay <- !is.element(dateSeq$wday,c(0,6))
	dateSeq <- dateSeq[isBusDay]
	result <- as.character(as.Date(dateSeq[abs(nbDays)+1]))
	
	return(result)
	
}

