# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("portfolioFactory",def=function(positions,...) standardGeneric("portfolioFactory"))

#setMethod("portfolioFactory",signature(positions="AyrtonPositions"),
#		function(positions,owner,referenceCurrency) {
#			
#			positions <- positionsFactory(positions)
#			
#			return(new("Portfolio",owner=owner,referenceCurrency=referenceCurrency,positions))
#			
#		}
#)

