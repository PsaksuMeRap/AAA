# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("positionsFactory",def=function(positions) standardGeneric("positionsFactory"))

setMethod("positionsFactory",signature(positions="AyrtonPositions"),
		function(positions) {

			# create the positions
			result <- lapply(positions,positionFactory)
			
			# return if empty
			if (length(result)==0) return(new("Positions",result))
			
			# remove the NULL positions coming from the AccruedInterest
			remove <- sapply(result,is.null)
			if (any(remove)) result <- result[!remove]
			
			return(new("Positions",result))
		}
)

