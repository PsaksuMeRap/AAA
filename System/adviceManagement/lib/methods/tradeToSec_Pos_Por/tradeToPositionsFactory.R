# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPositionsFactory",def=function(position,trade) standardGeneric("tradeToPositionsFactory"))

setMethod("tradeToPositionsFactory",signature(position="PositionEquity"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from equity trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
			}
)


setMethod("tradeToPositionsFactory",signature(position="PositionFutures_EQ"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from future on equity trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= position@security@deliveryPrice * (-1 * as.numeric(position@quantity) * position@valueOnePoint))
			
			return(positions)
			
		}
)


setMethod("tradeToPositionsFactory",signature(position="PositionBond"),
		function(position,trade) {
			
			positions <- new("Positions")
			positions[1] <- position
			
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from bond trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
		}
)



setMethod("tradeToPositionsFactory",signature(position="Positions"),
		function(position,trade) {
			# when the position is of class Positions (i.e. for fxForward) no
			# change is required
			return(position)
		}
)

setMethod("tradeToPositionsFactory",signature(position="PositionOpzioni_su_azioni"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
						
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from options on equity trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
		}
)

setMethod("tradeToPositionsFactory",signature(position="PositionOpzioni_su_divise"),
		function(position,trade) {
			
			positions <- new("Positions")
			positions[1] <- position			
	
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from options on fx trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
		}
)

