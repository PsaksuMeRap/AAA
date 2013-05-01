# TODO: Add comment
# 
# Author: claudio
###############################################################################


setMethod("portfolioFactory",signature(positions="AyrtonPositions"),
		function(positions,owner,referenceCurrency) {
			
			positions <- positionsFactory(positions)
			
			return(new("Portfolio",owner=owner,referenceCurrency=referenceCurrency,positions))
			
		}
)

