# TODO: Add comment
# 
# Author: claudio
###############################################################################

# create the class PositionObbligazioni_convertibili
setClass("PositionObbligazioni_convertibili",representation(positionCall="PositionOpzioni_su_azioni"),contains="PositionBond")


setMethod("reweight",signature(x="PositionObbligazioni_convertibili"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			
			position@positionCall <- reweight(x@positionCall,weight)
		
			return(position)
		}
)

setMethod("groupBySecurityId",signature(x="PositionObbligazioni_convertibili",y="PositionObbligazioni_convertibili"),
		function(x,y) {

			z <- new("PositionObbligazioni_convertibili",
					positionCall=groupBySecurityId(x@positionCall,y@positionCall),
					accruedInterest=x@accruedInterest + y@accruedInterest,
					id=x@id,
					security=x@security,
					quantity=x@quantity + y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
