# TODO: Add comment
# 
# Author: claudio
###############################################################################

# create the class PositionObbligazioni_convertibili
setClass("PositionObbligazioni_convertibili",representation(PositionCall="PositionOpzioni_su_azioni"),contains="PositionBond")


setMethod("reweight",signature(x="PositionObbligazioni_convertibili"),
		function(x,weight) {
			position <- x
			position@value <- x@value * weight
			position@quantity <- weight * x@quantity
			position@accruedInterest <- weight * x@accruedInterest 
			
			position@PositionCall <- reweight(x@PositionCall,weight)
		
			return(position)
		}
)

