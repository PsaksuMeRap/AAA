# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("replaceDirective",def=function(position,...) standardGeneric("replaceDirective"))

setMethod("replaceDirective",
		signature(position="PositionFutures_EQ"),
		function(position) {
			
			positionValue <- position@quantity * position@indexLevel * position@valueOnePoint
			
			position@value <- positionValue
			
			# crate the corresponding money flow
			currency <- position@security@currency
			securityConto_corrente <- new("Conto_corrente",currency=position@security@currency,
					name=paste(currency,"-",position@currency@name,sep=""))
			positionConto_corrente <- new("PositionConto_corrente",security=securityConto_corrente,
					value=-position@value,quantity=1,id=position@security@name)
			
			return(new("Positions",list(position,positionConto_corrente)))
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_azioni"),
		function(position) {
			
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
			
		}
)