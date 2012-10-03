# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/classDefinitions/Position/PositionBond/AccruedInterest.R")

# crea la classe virtuale "Position"
setClass("PositionBond",representation(accruedInterest="AccruedInterest",spRating="character"),contains="Position")


setMethod("reweight",signature(x="PositionBond"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)

setMethod("groupBySecurityId",signature(x="PositionBond",y="PositionBond"),
		function(x,y) {
			# the next line is necessary because PositionFondiObbligazionari
			# extend PositionBond but does not have a groupBySecurityId
			classOfx <- class(x)[[1]]
			
			z <- new(classOfx,
					accruedInterest=x@accruedInterest+y@accruedInterest,
					spRating=x@spRating,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)

			return(z)
		}
)


