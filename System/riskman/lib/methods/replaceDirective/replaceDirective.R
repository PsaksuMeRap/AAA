# TODO: Add comment
# 
# Author: claudio
###############################################################################



setGeneric("replaceDirective",def=function(position,...) standardGeneric("replaceDirective"))

setMethod("replaceDirective",
		signature(position="PositionFutures_EQ"),
		function(position) {
			
			positionValue <- (position@quantity * position@indexLevel) * position@valueOnePoint
			
			position@value <- positionValue
			
			# crate the corresponding money flow
			currency <- position@security@currency
			nameAndId <- paste(currency,"-",position@security@name,sep="")
			securityConto_corrente <- new("Conto_corrente",currency=currency,
					name=nameAndId,id=new("IdCharacter",nameAndId))
			positionConto_corrente <- new("PositionConto_corrente",security=securityConto_corrente,
					value=-1 *position@value,quantity=1,id=new("IdCharacter",nameAndId))
			
			return(new("Positions",list(position,positionConto_corrente)))
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_azioni"),
		function(position) {
			positionValue <- (position@quantity * position@indexLevel) * position@valueOnePoint
			
			position@value <- positionValue
			
			# crate the corresponding money flow
			currency <- position@security@currency
			nameAndId <- paste(currency,"-",position@security@name,sep="")
			securityConto_corrente <- new("Conto_corrente",currency=currency,
					name=nameAndId,id=new("IdCharacter",nameAndId))
			positionConto_corrente <- new("PositionConto_corrente",security=securityConto_corrente,
					value=-1 *position@value,quantity=1,id=new("IdCharacter",nameAndId))
			
			return(new("Positions",list(position,positionConto_corrente)))
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
			# construct the first lag based on the underlying currency
			nameAndId <- paste(position@security@underlying,"-",position@security@name,sep="")
			securityConto_corrente1 <- new("Conto_corrente",
					currency=position@security@underlying,
					name=nameAndId,
					id=new("IdCharacter",nameAndId))
			
			positionConto_corrente1 <- new("PositionConto_corrente",security=securityConto_corrente1,
					id=securityConto_corrente1@id,
					value=is.call_put(position)*position@quantity,
					quantity=1.0)
			
			positionValue <- (position@quantity * position@indexLevel) * position@valueOnePoint
			
			position@value <- positionValue
			
			# crate the corresponding money flow
			currency <- position@security@currency
			nameAndId <- paste(currency,"-",position@security@name,sep="")
			securityConto_corrente <- new("Conto_corrente",currency=currency,
					name=nameAndId,id=new("IdCharacter",nameAndId))
			positionConto_corrente <- new("PositionConto_corrente",security=securityConto_corrente,
					value=-1 *position@value,quantity=1,id=new("IdCharacter",nameAndId))
			
			return(new("Positions",list(position,positionConto_corrente)))		
			
		}
)

