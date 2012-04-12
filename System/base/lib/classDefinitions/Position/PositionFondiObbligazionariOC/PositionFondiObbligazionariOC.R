# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/classDefinitions/Position/PositionBond/AccruedInterest.R")

# crea la classe virtuale "Position"
setClass("PositionFondiObbligazionariOC",representation(accruedInterest="AccruedInterest"),contains="Position")


setMethod("reweight",signature(x="PositionFondiObbligazionariOC"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			return(position)
		}
)
