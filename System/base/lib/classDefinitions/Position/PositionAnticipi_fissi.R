# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/classDefinitions/Position/PositionBond/AccruedInterest.R")

# crea la classe virtuale "Position"
setClass("PositionAnticipi_fissi",representation(accruedInterest="AccruedInterest"),contains="Position")


setMethod("reweight",signature(x="PositionAnticipi_fissi"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)

setMethod("groupBySecurityId",signature(x="PositionAnticipi_fissi",y="PositionAnticipi_fissi"),
		function(x,y) {

			z <- new("PositionAnticipi_fissi",
					accruedInterest=x@accruedInterest+y@accruedInterest,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
