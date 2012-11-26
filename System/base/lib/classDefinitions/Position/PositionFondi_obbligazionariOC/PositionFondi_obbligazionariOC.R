# TODO: Add comment
# 
# Author: claudio
###############################################################################

# crea la classe virtuale "Position"
setClass("PositionFondi_obbligazionariOC",representation(accruedInterest="AccruedInterest",rating="character"),contains="Position")


setMethod("reweight",signature(x="PositionFondi_obbligazionariOC"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)

setMethod("groupBySecurityId",signature(x="PositionFondi_obbligazionariOC",y="PositionFondi_obbligazionariOC"),
		function(x,y) {
			z <- new("PositionFondi_obbligazionariOC",
					accruedInterest=x@accruedInterest+y@accruedInterest,
					rating=x@rating,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)

