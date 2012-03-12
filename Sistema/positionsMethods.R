# TODO: Add comment
# 
# Author: claudio
###############################################################################





identifyFundsToExplode <- function(fundData,positions) {
	# fundData: una lista con 4 campi: nomeFondo, instrumentClass, id, owner
	# una variabile di classe positions
	
	# return a vector with the positions matching the id field

	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))
	
	isFundToExplode <- function(position,fundData) {
		
		id <- position$id
		if (is.null(id)) return(FALSE)
		if (is.na(id))   return(FALSE)

		return(fundData[["id"]]==id & fundData[["instrumentClass"]]==class(position)[1])
	}
	
	result <- sapply(positions$positions,isFundToExplode,fundData)
	return(result)		
}


identifyCB_Accent_Lux_sicav_FIXED_INCOME_oacc <- function(positions) {

	nbPositions <- length(positions$positions)
	if (nbPositions==0) return (logical(0))

	# identify accrued interest
	identifyOacc <- function(position) {
		id <- position$id
		if (is.null(id)) return(FALSE)
		if (is.na(id))   return(FALSE)
		isOk <- is.element("accruedInterest",class(position)) & id==825
		return(isOk)
	}
	result <- sapply(positions$positions,identifyOacc)
	
	return(result)
}


weightPositions <- function(positions,weight) {
	return(invisible(lapply(positions$positions,weightPosition,weight)))
}

areConsistent <- function(positions) {
	isConsistent <- function(position) {
		return(position$isConsistent())
	}
	
	areConsistent <- sapply(positions$positions,isConsistent)
	return(areConsistent)
}

