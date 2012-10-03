# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionOpzioni_su_azioni"
setClass("PositionOpzioni_su_azioni",representation(numberEquities="numeric",contractSize="numeric"),contains="Position")

setMethod("reweight",signature(x="PositionOpzioni_su_azioni"),
		function(x,weight) {

			position <- new("PositionOpzioni_su_azioni",
					numberEquities=weight*x@numberEquities,
					contractSize=x@contractSize,
					id=x@id,
					security=x@security,
					quantity=weight*x@quantity,
					value=weight*x@value
			
			)
			return(position)
		}
)


setMethod("groupBySecurityId",signature(x="PositionOpzioni_su_azioni",y="PositionOpzioni_su_azioni"),
		function(x,y) {
			
			z <- new("PositionOpzioni_su_azioni",
					numberEquities=x@numberEquities+y@numberEquities,
					contractSize = x@contractSize,
					id=x@id,
					security=x@security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
