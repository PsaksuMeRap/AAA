# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPositionFactory",def=function(newSecurity,trade,blData) standardGeneric("tradeToPositionFactory"))

setMethod("tradeToPositionFactory",signature(newSecurity="Equity"),
		function(newSecurity,trade,blData) {
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value
					
			quantity <- trade$Quantity

			# crea la classe virtuale "PositionEquity"
			equityPositions <- new("PositionEquity",id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
							quantity=quantity,value=toMoney(quantity*price,newSecurity@currency))
			
			return(equityPositions)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Futures_EQ"),
		function(newSecurity,trade,blData) {
		
			# get the last price
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value
		
			# get the value of 1 pt
			ptId <- paste(trade$Id_Bloomberg,"FUT_VAL_PT",sep="__")
			valueOnePoint <- blData[[ptId]]@value
			
			# get the underlying ticker
			underlyingId <- paste(trade$Id_Bloomberg,"UNDL_SPOT_TICKER",sep="__")
			underlying <- blData[[underlyingId]]@value
			
			# get the delivery date
			deliveryDateId <- paste(trade$Id_Bloomberg,"FUT_DLV_DT_FIRST",sep="__")
			deliveryDate <- blData[[deliveryDateId]]@value	
			
			quantity <- trade$Quantity
			
			# update the newSecurity
			newSecurity@deliveryDate <- deliveryDate
			newSecurity@underlying <- new("IndexEquity",name=underlying,id=new("IdBloomberg",underlying))
	
			# crea la classe virtuale "PositionFutures_EQ"
			futureEquityIndexPosition <- new("PositionFutures_EQ",valueOnePoint=valueOnePoint,id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					quantity=quantity,value=toMoney(quantity*price*valueOnePoint,newSecurity@currency))
			
			return(futureEquityIndexPosition)
		}
)

setMethod("tradeToPositionFactory",signature(newSecurity="Conto_corrente"),
		function(newSecurity,trade,blData) {
			
			securityType <- trade$Security_type
			
			if (securityType=="FX Spot") {
				
				blFxCorrectionFactor <- function(IdBloomberg) {
					return(1.0)
				}
				
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value * blFxCorrectionFactor(trade$Id_Bloomberg)
				
				quantity <- 1.0
				
				# crea la classe virtuale "PositionConto_corrente"
				conto_correntePosition <- new("PositionConto_corrente",id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
						quantity=quantity,value=toMoney(trade$Quantity,newSecurity@currency))
				
				return(conto_correntePosition)
			}
		}
)
