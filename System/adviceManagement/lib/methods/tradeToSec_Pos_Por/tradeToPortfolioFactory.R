# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPortfolioFactory",def=function(position,trade) standardGeneric("tradeToPortfolioFactory"))

setMethod("tradeToPortfolioFactory",signature(position="PositionEquity"),
		function(position) {
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


setMethod("tradeToPortfolioFactory",signature(position="PositionFutures_EQ"),
		function(position) {
			positions <- new("Positions")
			positions[1] <- position
			
			# create the cash flow associated with this trade
			id <- paste(position@security@currency,"-",tolower(position@security@currency),sep="")
			name <- paste(id," from future on equity trade ", position@id,sep="")
			
			security <- new("Conto_corrente",currency=position@security@currency,name=name,id=new("IdCharacter",id))
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
		}
)


setMethod("tradeToPortfolioFactory",signature(position="PositionBond"),
		function(position) {
			
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



setMethod("tradeToPortfolioFactory",signature(position="Positions"),
		function(position,trade) {
			
			#securityType <- trade$Security_type
		
			#if (securityType=="FX Spot") {
				return(position)
			#}
		}
)

setMethod("tradeToPortfolioFactory",signature(position="PositionOpzioni_su_azioni"),
		function(position) {
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

setMethod("tradeToPortfolioFactory",signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
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

