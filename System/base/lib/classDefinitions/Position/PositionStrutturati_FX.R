# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionFX_Forward"
setClass("PositionStrutturati_FX",representation(otherLeg="Money",premium="Money"),contains="Position")



setMethod("reweight",signature(x="PositionStrutturati_FX"),
		function(x,weight) {
		
			position <- new("PositionStrutturati_FX",
					otherLeg=weight*x@otherLeg,
					premium=x@premium,
					id=x@id,
					security=x@security,
					quantity=weight*x@quantity,
					value=weight*x@value
			
			)
			return(position)
		}
)


