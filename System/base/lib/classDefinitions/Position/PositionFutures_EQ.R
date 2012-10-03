# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionFutures_EQ"
setClass("PositionFutures_EQ",representation(indexLevel="numeric",valueOnePoint="Money"),contains="Position")

setMethod("groupBySecurityId",signature(x="PositionFutures_EQ",y="PositionFutures_EQ"),
		function(x,y) {
			
			z <- new("PositionFutures_EQ",
					indexLevel=x@indexLevel,
					valueOnePoint=x@valueOnePoint,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
