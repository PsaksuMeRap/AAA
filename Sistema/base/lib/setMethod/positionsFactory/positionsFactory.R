# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("positionsFactory",def=function(positions) standardGeneric("positionsFactory"))

setMethod("positionsFactory",signature(positions="AyrtonPositions"),
		function(positions) {

			# create the positions
			result <- new("Positions",lapply(positions,positionFactory))
			
			# return if empty
			if (length(result)==0) return(result)
			
			# remove the NULL positions coming from the AccruedInterest
			remove <- sapply(result,is.null)
			
			if (any(remove)) {
				# extract the accruedInterest				
				accruedInterestPositions <- positions[remove]
				# extract the pure positions
				result <- result[!remove]
				# adjust PositionBond for missing accruedInterest
				result <- adjustForAccruedInterest(result,accruedInterestPositions)
			}
			
			return(new("Positions",result))
		}
)

