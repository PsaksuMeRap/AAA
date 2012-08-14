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
			
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
			
		}
)

test.shouldReplacePositionFutures_EQ <- function() {
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$Futures_EQ1,repository$equity2,repository$bond1)
	
	result <- replaceDirective(positions[[2]])
	
	checkEquals(length(result),2)
	checkEquals(is(result[[1]],"PositionFutures_EQ"),TRUE)
	checkEquals(is(result[[2]],"PositionConto_corrente"),TRUE)
	checkEquals(result[[1]]@value,-1 * result[[2]]@value)
	
}