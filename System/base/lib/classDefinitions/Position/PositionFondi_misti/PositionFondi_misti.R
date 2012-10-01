# TODO: Add comment
# 
# Author: claudio
###############################################################################


# crea la classe virtuale "Position"
setClass("PositionFondi_misti",representation(equityPart="numeric",bondPart="numeric"),contains="Position")


setMethod("groupBySecurityId",signature(x="PositionFondi_misti",y="PositionFondi_misti"),
		function(x,y) {
			z <- new("PositionFondi_misti",
					equityPart=x@equityPart,
					bondPart=x@bondPart,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)