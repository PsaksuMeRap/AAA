# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPositionsFactory",def=function(position,trade) standardGeneric("tradeToPositionsFactory"))

tradeToPositionsFactoryCreateCashFlow <- function(position,fromString) {
	# create the cash flow associated with this trade
	currency <- as.character(position@security@currency)
	idCharacter <- paste(currency,tolower(currency),sep="-")
	id <- new("IdAyrton",idAAA=new("IdAAA_character",idCharacter),idStrumento=40)
	
	name <- paste(idCharacter, position@security@name,sep="")
	
	security <- new("Conto_corrente",currency=position@security@currency,name=name,id=id)
	return(security)
}



setMethod("tradeToPositionsFactory",signature(position="PositionEquity"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from equity trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
			}
)


setMethod("tradeToPositionsFactory",signature(position="PositionFondi_azionari"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from equity fund trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
		}
)

setMethod("tradeToPositionsFactory",signature(position="PositionFondi_obbligazionari"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from bond fund trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
		}
)

setMethod("tradeToPositionsFactory",signature(position="PositionFutures_EQ"),
		function(position,trade) {
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from future on equity trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
			positions[2] <- new("PositionConto_corrente_fittizio",id=security@id,security=security,
					quantity=1,value= position@security@deliveryPrice * (-1 * as.numeric(position@quantity) * position@valueOnePoint))
			
			return(positions)
			
		}
)


setMethod("tradeToPositionsFactory",signature(position="PositionBond"),
		function(position,trade) {
			
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from bond trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
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
			
			## create the security Conto_corrente
			fromString <- " from options on equity trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			## create the corresponding position
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
			
		}
)


setMethod("tradeToPositionsFactory",signature(position="PositionOpzioni_su_divise"),
		function(position,trade) {
			
			positions <- new("Positions")
			positions[1] <- position
			
			## create the security Conto_corrente
			fromString <- " from options on fx trade "
			security <- tradeToPositionsFactoryCreateCashFlow(position,fromString)
			
			positions[2] <- new("PositionConto_corrente",id=security@id,security=security,
					quantity=1,value= -1 * position@value)
			
			return(positions)
		}
)

