# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPositionFactory",def=function(newSecurity,trade,blData) standardGeneric("tradeToPositionFactory"))

setMethod("tradeToPositionFactory",signature(newSecurity="Equity"),
		function(newSecurity,trade,blData) {
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value
					
			quantity <- sign(trade)*trade$Quantity

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
			
			quantity <- sign(trade)*trade$Quantity
			
			# update the newSecurity
			newSecurity@deliveryDate <- deliveryDate
			newSecurity@underlying <- new("IndexEquity",name=underlying,id=new("IdBloomberg",underlying))
	
			# crea la classe virtuale "PositionFutures_EQ"
			futureEquityIndexPosition <- new("PositionFutures_EQ",valueOnePoint=valueOnePoint,id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					quantity=quantity,value=toMoney(quantity*price*valueOnePoint,newSecurity@currency))
			
			return(futureEquityIndexPosition)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Bond"),
		function(newSecurity,trade,blData) {
			
			# get the last price
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value
			
			# get the value of the accrued interest
			accInterestId <- paste(trade$Id_Bloomberg,"INT_ACC",sep="__")
			accInterest <- new("AccruedInterest",toMoney(trade$Quantity*blData[[accInterestId]]@value/100,newSecurity@currency))
			accInterestPercentage <- blData[[accInterestId]]@value
			
			# get the Standard and Poors rating
			spRatingId <- paste(trade$Id_Bloomberg,"RTG_SP",sep="__")
			spRating <- blData[[spRatingId]]@value
			
			# get the maturity date
			maturityId <- paste(trade$Id_Bloomberg,"MATURITY",sep="__")
			maturity <- blData[[maturityId]]@value
			
			quantity <- new("NominalValue",amount=new("Amount",sign(trade)*trade$Quantity),currency=newSecurity@currency)
			
			# update the newSecurity
			newSecurity@maturity <- maturity
					
			# crea la classe virtuale "PositionBond"
			value <- (0.01*(price+accInterestPercentage))*quantity
			value <- as(value,"Money")
			bondPosition <- new("PositionBond",spRating=spRating,accruedInterest=accInterest,id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					quantity=quantity,value=value)
			
			return(bondPosition)
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
				
				quantity <- sign(trade)*1.0
				
				# crea la classe virtuale "PositionConto_corrente"
				conto_correntePosition <- new("PositionConto_corrente",id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
						quantity=quantity,value=toMoney(trade$Quantity,newSecurity@currency))
				
				return(conto_correntePosition)
			}
		}
)

setMethod("tradeToPositionFactory",signature(newSecurity="Opzioni_su_azioni"),
		function(newSecurity,trade,blData) {

			# get the underlying ticker
			underlyingId <- paste(trade$Id_Bloomberg,"OPT_UNDL_TICKER",sep="__")
			underlying <- blData[[underlyingId]]@value
			
			# get the expiry date
			expiryDateId <- paste(trade$Id_Bloomberg,"OPT_EXPIRE_DT",sep="__")
			expiryDate <- blData[[expiryDateId]]@value
			
			# get the last price
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value
			
			# get the strike price
			strikeId <- paste(trade$Id_Bloomberg,"OPT_STRIKE_PX",sep="__")
			strike <- blData[[strikeId]]@value	
			
			# get the option type
			optionTypeId <- paste(trade$Id_Bloomberg,"OPT_TYPE",sep="__")
			optionType <- blData[[optionTypeId]]@value
			
			# get the option contract size
			contractSizeId <- paste(trade$Id_Bloomberg,"OPT_CONT_SIZE",sep="__")
			contractSize <- blData[[contractSizeId]]@value
			
			quantity <- sign(trade)*trade$Quantity
		
			# update the newSecurity
			newSecurity@expiryDate <- expiryDate
			newSecurity@underlying <- new("IndexEquity",name=underlying,id=new("IdBloomberg",underlying))
			newSecurity@strike <- strike
			newSecurity@optionType <- optionType
			
			# crea la classe virtuale "PositionOpzioni_su_azioni"
			OptionOnEquityPosition <- new("PositionOpzioni_su_azioni",id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					contractSize=contractSize,quantity=quantity,value=toMoney(quantity*contractSize*price,newSecurity@currency))
			
			return(OptionOnEquityPosition)
		}
)

