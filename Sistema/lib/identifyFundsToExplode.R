# TODO: Add comment
# 
# Author: claudio
###############################################################################


identifyFundsToExplode <- function(fundData,positions) {
	# fundData: a list with 4 fields: nomeFondo, instrumentClass, id, owner
	# a variable of class Positions
	
	# return a vector with the positions matching the id field
	
	nbPositions <- length(positions)
	if (nbPositions==0) return (logical(0))
	
	isFundToExplode <- function(position,fundData) {
	
		id <- position@security@id@idAAA
		if (is.null(id)) return(FALSE)
		if (is.na(id))   return(FALSE)
		
		return(fundData[["id"]]==id & is(position@security,fundData[["securityClass"]]))
	}

	result <- sapply(positions,isFundToExplode,fundData)
	return(result)		
}
