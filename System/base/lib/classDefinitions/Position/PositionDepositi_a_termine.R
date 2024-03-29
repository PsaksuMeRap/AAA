# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/classDefinitions/Position/PositionBond/AccruedInterest.R")

# crea la classe virtuale "Position"
setClass("PositionDepositi_a_termine",representation(accruedInterest="AccruedInterest"),contains="Position")

setMethod("reweight",signature(x="PositionDepositi_a_termine"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)

setMethod("groupBySecurityId",signature(x="PositionDepositi_a_termine",y="PositionDepositi_a_termine"),
		function(x,y) {
			
			z <- new("PositionDepositi_a_termine",
					accruedInterest=x@accruedInterest+y@accruedInterest,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
