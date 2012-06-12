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

			# create the class "PositionEquity"
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
	
			# create the class "PositionFutures_EQ"
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
					
			# create the class "PositionBond"
			value <- (0.01*(price+accInterestPercentage))*quantity
			value <- as(value,"Money")
			bondPosition <- new("PositionBond",spRating=spRating,accruedInterest=accInterest,id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					quantity=quantity,value=value)
			
			return(bondPosition)
		}
)


parseFxSpotId_Bloomberg <- function(IdBloomberg) {
	tmp <- strsplit(IdBloomberg,"\\s+")[[1]]
	
	# identify the two currencies codes, the underlying and the 
	# numeraire
	
	currencyCodes <- toupper(tmp[1])
	underlying <- substr(currencyCodes,1,3)
	numeraire <- substr(currencyCodes,4,6)
	
	return(list(underlying=underlying,numeraire=numeraire))
}


setMethod("tradeToPositionFactory",signature(newSecurity="Conto_corrente"),
		function(newSecurity,trade,blData) {
			
			securityType <- trade$Security_type
		
			if (securityType=="FX Spot") {
				
				blFxCorrectionFactor <- function(IdBloomberg) {
					return(1.0)
				}
			
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value * blFxCorrectionFactor(trade$Id_Bloomberg)
				
				# we create two conto_corrente positions with respect the "xxxyyy curncy" spot trade.
				# the first conto_corrente contains the amount in currency "xxx" while the second the 
				# corresponding amount in "yyy" currency.
				# the amount in currency "xxx" can be extracted directly from the trade$quantity field
				
				# create the class "PositionConto_corrente"
				info <- parseFxSpotId_Bloomberg(trade$Id_Bloomberg)
				
				# the underlying currency first
				currency <- new("Currency",info[["underlying"]])
				id <- new("IdBloomberg",paste(currency,tolower(currency),sep="-"))
				name <- paste(id,trade$Id_Bloomberg)
				newSecurity <- new("Conto_corrente",currency=currency,name=name,id=id) 
				conto_corrente_xxx <- new("PositionConto_corrente",id=new("IdBloomberg",id),security=newSecurity,
						quantity=1.0,value=toMoney(trade$Quantity,newSecurity@currency))
				
				# the numeraire currency then
				currency <- new("Currency",info[["numeraire"]])
				id <- new("IdBloomberg",paste(currency,tolower(currency),sep="-"))
				name <- paste(id,trade$Id_Bloomberg)
				newSecurity <- new("Conto_corrente",currency=currency,name=name,id=id) 
				conto_corrente_yyy <- new("PositionConto_corrente",id=new("IdBloomberg",id),security=newSecurity,
						quantity=1.0,value=toMoney(trade$Quantity*price,currency))
				
				# create a list of positions
				positions <- new("Positions")
				positions[1] <- conto_corrente_xxx
				positions[2] <- conto_corrente_yyy
				return(positions)
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
			optionTypeId <- paste(trade$Id_Bloomberg,"OPT_PUT_CALL",sep="__")
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
			
			# create the class "PositionOpzioni_su_azioni"
			OptionOnEquityPosition <- new("PositionOpzioni_su_azioni",id=new("IdBloomberg",trade$Id_Bloomberg),security=newSecurity,
					contractSize=contractSize,quantity=quantity,value=toMoney(quantity*contractSize*price,newSecurity@currency))
			
			return(OptionOnEquityPosition)
		}
)

setMethod("tradeToPositionFactory",signature(newSecurity="Opzioni_su_divise"),
		function(newSecurity,trade,blData) {
			
			# for options on fx we define the quantity to be the quantity of the first currency
			# in the xxxyyy mnemonic, (xxx is the iso code of the first currency and yyy
			# the iso code of the second currency)
			
			info <- parseFxForwardName(trade$Security_name)
			quantity <- toMoney(trade$Quantity,info[["underlying"]])
			
			# for options on fx we define the price to be equal to value of the position, i.e.
			# the amount
			value <- trade$Amount
	
			# create the class "PositionOpzioni_su_divise"
			optionOnFxPosition <- new("PositionOpzioni_su_divise",id=new("IdCharacter",trade$Security_name),security=newSecurity,
					contractSize=1.0,quantity=quantity,value=toMoney(value,newSecurity@currency))

			
			
			return(optionOnFxPosition)
		}
)

setMethod("tradeToPositionFactory",signature(newSecurity="FX_Forward"),
		function(newSecurity,trade,blData) {
			
			info <- parseFxForwardName(trade$Security_name)
			
			# for fx forwards we define the quantity to be the quantity bought or sold of the first currency
			# in the xxxyyy mnemonic, (xxx is the iso code of the first currency and yyy
			# the iso code of the second currency)
			# for fx forwards we use the same convention as the bloomberg spot price. 
			# The forward price of eurchf will be the price of 1 unit of EUR in CHF
			# and sekchf will be the price of 100 sek in chf
			
			quantity <- toMoney(sign(trade)*trade$Quantity,info[["underlying"]])
					
			# the underlying currency first
			currency <- new("Currency",info[["underlying"]])
			date <- format(strptime(info[["settlementDate"]],format="%m/%d/%Y"),"%d-%m-%Y")
			name <- paste(as.character(quantity),"value date",date)
			id <- new("IdCharacter",paste(name," ",trade$Price," ",info[["numeraire"]],"/",info[["underlying"]],sep=""))
			securityLeg1 <- new("FX_Forward",currency=currency,name=name,id=id) 
			
			positionLeg1 <- new("PositionFX_Forward",id=id,security=securityLeg1,
					quantity=quantity,value=quantity)
			
			# then the numeraire currency
			currency <- new("Currency",info[["numeraire"]])
			quantity <- (-1*trade$Price)*quantity
			name <- paste(as.character(quantity),"value date",date)
			securityLeg2 <- new("FX_Forward",currency=currency,name=name,id=id) 
			positionLeg2 <- new("PositionFX_Forward",id=id,security=securityLeg2,
					quantity=quantity,value=quantity)
			
			# create a list of positions
			positions <- new("Positions")
			positions[1] <- positionLeg1
			positions[2] <- positionLeg2
			return(positions)
		}
)