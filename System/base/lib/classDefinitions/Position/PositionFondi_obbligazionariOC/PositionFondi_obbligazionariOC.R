# TODO: Add comment
# 
# Author: claudio
###############################################################################

# crea la classe virtuale "Position"
setClass("PositionFondi_obbligazionariOC",representation(accruedInterest="AccruedInterest"),contains="Position")


setMethod("reweight",signature(x="PositionFondi_obbligazionariOC"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)
