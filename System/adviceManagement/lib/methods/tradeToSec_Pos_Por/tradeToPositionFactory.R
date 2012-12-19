# TODO: Add comment
# 
# Author: Claudio
###############################################################################

setGeneric("tradeToPositionFactory",def=function(newSecurity,trade,blData) standardGeneric("tradeToPositionFactory"))

setMethod("tradeToPositionFactory",signature(newSecurity="Security"),
		function(newSecurity,trade,blData) {
		
			if (is.null(trade$Confirmed_quantity)) {
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value
				
				quantity <- sign(trade)*trade$Quantity
			} else {
				price <- trade$Confirmed_price
				quantity <- sign(trade)*trade$Confirmed_quantity
			}

			# create the class position
			className <- paste("Position",class(newSecurity)[[1]],sep="")
			
			position <- new(className,id=newSecurity@id,security=newSecurity,
					quantity=quantity,value=toMoney(quantity*price,newSecurity@currency))
			
			return(position)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Fondi_obbligazionari"),
		function(newSecurity,trade,blData) {
			
			if (is.null(trade$Confirmed_quantity)) {
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value
				
				quantity <- sign(trade)*trade$Quantity
			} else {
				price <- trade$Confirmed_price
				quantity <- sign(trade)*trade$Confirmed_quantity
			}
			
			# create the class "PositionFondi_obbligazionari"
			accruedInterest <- new("AccruedInterest",toMoney(0,"EUR"))
			Position <- new("PositionFondi_obbligazionari",accruedInterest=accruedInterest,id=newSecurity@id,
							security=newSecurity,quantity=quantity,
							value=toMoney(quantity*price,newSecurity@currency))
			
			return(Position)
		}
)



setMethod("tradeToPositionFactory",signature(newSecurity="Futures_EQ"),
		function(newSecurity,trade,blData) {

			# get the last price
			priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
			price <- blData[[priceId]]@value	
		
			if (is.null(trade$Confirmed_quantity)) {
				deliveryPrice <- price
				quantity <- sign(trade)*trade$Quantity
			} else {
				deliveryPrice <- trade$Confirmed_price
				quantity <- sign(trade)*trade$Confirmed_quantity			
			}
			
			# get the value of 1 pt
			ptId <- paste(trade$Id_Bloomberg,"FUT_VAL_PT",sep="__")
			valueOnePoint <- blData[[ptId]]@value
			
			# get the underlying ticker
			underlyingId <- paste(trade$Id_Bloomberg,"UNDL_SPOT_TICKER",sep="__")
			underlyingName <- paste("Future",blData[[underlyingId]]@value)
			
			# get the delivery date
			deliveryDateId <- paste(trade$Id_Bloomberg,"FUT_DLV_DT_FIRST",sep="__")
			deliveryDate <- blData[[deliveryDateId]]@value
			deliveryDate <- format(strptime(deliveryDate,format="%m/%d/%Y"),"%Y-%m-%d")
			
			
			# update the newSecurity
			newSecurity@name <- paste(underlyingName,
					format(strptime(deliveryDate,format="%Y-%m-%d"),"%d-%m-%Y"),
					"/",valueOnePoint)
			newSecurity@deliveryDate <- deliveryDate
			newSecurity@deliveryPrice <- deliveryPrice # error: cambia nome slot -> deliveryLevel
			newSecurity@underlying <- new("IndexEquity",name=underlyingName,id=new("IdCharacter",underlyingName))
			
			## "Future SMI 2012-12-27"
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",paste(underlyingName,deliveryDate,sep="")),
					idStrumento=50)
			
			newSecurity@id <- id
			
			# create the class "PositionFutures_EQ"
			futureEquityIndexPosition <- new("PositionFutures_EQ",valueOnePoint=toMoney(valueOnePoint,
						newSecurity@currency),id=id,security=newSecurity,
					quantity=quantity,value=toMoney(quantity*price*valueOnePoint,newSecurity@currency))
			
			return(futureEquityIndexPosition)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Bond"),
		function(newSecurity,trade,blData) {
		
			if (is.null(trade$Confirmed_quantity)) {
				# get the last price
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value
				quantity <- new("NominalValue",amount=new("Amount",sign(trade)*trade$Quantity),currency=newSecurity@currency)
			} else {
				price <- trade$Confirmed_price
				quantity <- new("NominalValue",amount=new("Amount",sign(trade)*trade$Confirmed_quantity),currency=newSecurity@currency)			
			}
				
			# get the value of the accrued interest
			accInterestId <- paste(trade$Id_Bloomberg,"INT_ACC",sep="__")
			accInterest <- new("AccruedInterest",toMoney(trade$Quantity*blData[[accInterestId]]@value/100,newSecurity@currency))
			accInterestPercentage <- blData[[accInterestId]]@value
			
			# get the Standard and Poors rating
			ratingId <- paste(trade$Id_Bloomberg,"RTG_SP_LONG",sep="__")
			rating <- c(spRating=blData[[ratingId]]@value)
			
			# get the maturity date
			maturityId <- paste(trade$Id_Bloomberg,"MATURITY",sep="__")
			maturity <- blData[[maturityId]]@value
			
			# update the newSecurity
			newSecurity@maturity <- maturity
					
			# create the class "PositionBond"
			value <- (0.01*(price+accInterestPercentage))*quantity
			value <- as(value,"Money")
			bondPosition <- new("PositionBond",rating=rating,accruedInterest=accInterest,id=newSecurity@id,security=newSecurity,
					quantity=quantity,value=value)
			
			return(bondPosition)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Conto_corrente"),
		function(newSecurity,trade,blData) {
			
			securityType <- trade$Security_type
	
			if (securityType=="FX spot") {
				
				# identify the currency codes defining the trades, i.e. EURCHF (buy EUR vs CHF)
				info <- parseFxSpotId_Bloomberg(trade$Id_Bloomberg)
				currCodes <- paste(info[["underlying"]],info[["numeraire"]],sep="") 
			
				if (is.null(trade$Confirmed_quantity)) {
					# get the last price
					priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
					price <- blData[[priceId]]@value * repositories$exchangeRates$getPricePositionMultFactor(currCodes)
					tradeQuantity <- trade$Quantity
				} else {
					price <- trade$Confirmed_price * repositories$exchangeRates$getPricePositionMultFactor(currCodes)
					tradeQuantity <- trade$Confirmed_quantity			
				}
					
				# we create two conto_corrente positions with respect the spot trade "xxxyyy curncy" .
				# the first conto_corrente contains the amount in currency "xxx" while the second the 
				# corresponding amount in "yyy" currency.
				# the amount in currency "xxx" can be extracted directly from the trade$quantity field
				
				# create the class "PositionConto_corrente"
				# the underlying currency first
				name <- newSecurity@name
				conto_corrente_xxx <- new("PositionConto_corrente",id=newSecurity@id,security=newSecurity,
						quantity=1.0,value=toMoney(sign(trade)*tradeQuantity,newSecurity@currency))
				
				# the numeraire currency then
				name <- paste(info[["numeraire"]],"-",tolower(info[["numeraire"]])," ",trade$Id_Bloomberg,sep="")
				id <- new("IdAyrton",
						idAAA=new("IdAAA_character",paste(info[["numeraire"]],tolower(info[["numeraire"]]),sep="-")),
						idStrumento=40)
				
				newSecurity <- new("Conto_corrente",currency=new("Currency",info[["numeraire"]]),name=name,id=id) 
				conto_corrente_yyy <- new("PositionConto_corrente",id=id,security=newSecurity,
						quantity=1.0,value=toMoney(-sign(trade)*tradeQuantity*price,info[["numeraire"]]))
				
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
	
			if (is.null(trade$Confirmed_quantity)) {
				# get the last price
				priceId <- paste(trade$Id_Bloomberg,"LAST_PRICE",sep="__")
				price <- blData[[priceId]]@value
				quantity <- sign(trade)*trade$Quantity
			} else {
				price <- trade$Confirmed_price
				quantity <- sign(trade)*trade$Confirmed_quantity			
			}

			# get the underlying ticker
			underlyingId <- paste(trade$Id_Bloomberg,"OPT_UNDL_TICKER",sep="__")
			underlyingTicker <- blData[[underlyingId]]@value
			
			# get the underlying isin (if index like s&p 500 then value is NA)
			underlyingId <- paste(trade$Id_Bloomberg,"OPT_UNDL_ISIN",sep="__")
			underlyingIsin <- blData[[underlyingId]]@value
			if (is.na(underlyingIsin)) underlyingIsin <- underlyingTicker
			
			# get the underlying price 
			underlyingId <- paste(trade$Id_Bloomberg,"OPT_UNDL_PX",sep="__")
			underlyingPrice <- blData[[underlyingId]]@value
			
			# get the expiry date
			expiryDateId <- paste(trade$Id_Bloomberg,"OPT_EXPIRE_DT",sep="__")
			expiryDate <- blData[[expiryDateId]]@value
			
			# get the strike price
			strikeId <- paste(trade$Id_Bloomberg,"OPT_STRIKE_PX",sep="__")
			strike <- blData[[strikeId]]@value	
			
			# get the option type
			optionTypeId <- paste(trade$Id_Bloomberg,"OPT_PUT_CALL",sep="__")
			optionType <- blData[[optionTypeId]]@value
			if (tolower(optionType)=="put") optionType <- "P" else optionType <- "C"
			optionTypeAAA <- c(P="Put",C="Call")
			
			# get the option contract size
			contractSizeId <- paste(trade$Id_Bloomberg,"OPT_CONT_SIZE",sep="__")
			contractSize <- blData[[contractSizeId]]@value
			
			# determine the underlying
			tmp <- createEquitySecurityFromIsin(underlyingIsin)
			if (tmp@name=="Equity not in DBEquity") tmp@name <- underlyingTicker
			newSecurity@underlying <- tmp
			
			# construct the security name according to ayrton_position
			## "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
			name <- paste(quantity,
					optionTypeAAA[[optionType]],
					newSecurity@underlying@name,
					format(strptime(expiryDate,format="%m/%d/%Y"),"%d-%m-%y"),
					paste("Strike",strike),
					paste("Premio(",-quantity*contractSize*price," ",newSecurity@currency,")",sep=""),
					underlyingIsin,
					underlyingPrice,
					contractSize,
					sep=" / ")
						
			# update the newSecurity
			newSecurity@expiryDate <- expiryDate
			
			newSecurity@expiryDate <- format(strptime(expiryDate,format="%m/%d/%Y"),"%Y-%m-%d")
			newSecurity@strike <- strike
			newSecurity@optionType <- optionType
			newSecurity@name <- name
			
			# create the idAyrton
			
			idAAA <- paste(optionType,
					underlyingIsin,
					newSecurity@expiryDate,
					strike,
					sep="/"
			)
			idAyrton <- new("IdAyrton",
					idAAA=new("IdAAA_character",idAAA),
					idStrumento=18
			)

			newSecurity@id <- idAyrton
			
			# create the class "PositionOpzioni_su_azioni"
			OptionOnEquityPosition <- new("PositionOpzioni_su_azioni",
					id=newSecurity@id,
					security=newSecurity,
					numberEquities=quantity*contractSize,
					quantity=quantity,
					contractSize=contractSize,
					value=toMoney(quantity*contractSize*price,newSecurity@currency)
					)

			return(OptionOnEquityPosition)
		}
)


setMethod("tradeToPositionFactory",signature(newSecurity="Opzioni_su_divise"),
		function(newSecurity,trade,blData) {
			
			# for options on fx we define the quantity to be the quantity of the first currency
			# in the xxxyyy mnemonic, (xxx is the iso code of the first currency and yyy
			# the iso code of the second currency)
	
			underlying <- newSecurity@underlying
			
			if (is.null(trade$Confirmed_quantity)) {
				price <- trade$Price
				quantity <- toMoney(sign(trade)*trade$Quantity,underlying)
			} else {
				price <- trade$Confirmed_price
				quantity <- toMoney(sign(trade)*trade$Confirmed_quantity,underlying)	
			}
			
			# compute the value of the position. By convention the price is in numeraire / 1 unit of
			# underlying quantity
			numeraire <- newSecurity@currency 
			
			value <- toMoney(as.numeric(quantity@amount)*price,numeraire)
			
			info <- parseOptionFxName(trade$Security_name)
	
			## construct the security name
			securityName <- paste(info[["optionType"]],"/",
					info[["expiryDate"]],"/",
					"Strike"," ",info[["strike"]],"/",
					underlying, " ",format(quantity@amount,scientific=FALSE),"/",
					"Premium ",as.character(-1*value@amount), " ",numeraire,
					sep=""
			)		
			newSecurity@name <- securityName
			
			# construct the id		
			idAAA <- paste(info[["optionType"]],
					paste(underlying,numeraire,sep=""),
					info[["expiryDate"]],
					info[["strike"]],
					sep="/")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idAAA),
					idStrumento=19)
			newSecurity@id <- id
			# create the class "PositionOpzioni_su_divise"
			optionOnFxPosition <- new("PositionOpzioni_su_divise",id=id,security=newSecurity,
					quantity=quantity,value=value)
			
			return(optionOnFxPosition)
		}
)

setMethod("tradeToPositionFactory",signature(newSecurity="FX_Forward"),
		function(newSecurity,trade,blData) {
			
			info <- parseFxForwardName(trade$Id_Bloomberg)
		
			# for fx forwards we define the quantity to be the quantity bought or sold of the first currency
			# in the xxxyyy mnemonic, (xxx is the iso code of the first currency and yyy
			# the iso code of the second currency)
			# for fx forwards we use the same convention as the bloomberg spot price. 
			# The forward price of eurchf will be the price of 1 unit of EUR in CHF
			# and sekchf will be the price of 100 sek in chf
			
			
			if (is.null(trade$Confirmed_quantity)) {
				# get the last price
				price <- trade$Price
				deliveryPrice <- trade$Price
				quantity <- toMoney(sign(trade)*trade$Quantity,info[["underlying"]])
			} else {
				price <- trade$Confirmed_price
				deliveryPrice <- trade$Confirmed_price
				quantity <- toMoney(sign(trade)*trade$Confirmed_quantity,info[["underlying"]])			
			}
				
			# the underlying currency first
			currency <- new("Currency",info[["underlying"]])
			deliveryDate <- format(strptime(info[["deliveryDate"]],format="%m/%d/%Y"),"%d-%m-%Y")
			# name <- paste(as.character(quantity),"value date",date)
			name <- paste("future_fx valuta ",deliveryDate," ",info[["currencyCodes"]]," ",deliveryPrice," ",
							as.character(quantity)," leg ",info[["underlying"]],sep="")
			id <- new("IdCharacter",name)
			securityLeg1 <- new("FX_Forward",currency=currency,name=name,id=id,
					deliveryDate=deliveryDate,deliveryPrice=toMoney(deliveryPrice,info[["numeraire"]])) 
			
			positionLeg1 <- new("PositionFX_Forward",id=id,security=securityLeg1,
					quantity=quantity,value=quantity)
			
			# then the numeraire currency
			currency <- new("Currency",info[["numeraire"]])
			name <- paste("future_fx valuta ",deliveryDate," ",info[["currencyCodes"]]," ",deliveryPrice," ",
					as.character(quantity)," leg ",info[["numeraire"]],sep="")
			id <- new("IdCharacter",name)
			quantity <- toMoney((-1*price)*quantity@amount,info[["numeraire"]])
			
			# name <- paste(as.character(quantity),"valuta",date)
			securityLeg2 <- new("FX_Forward",currency=currency,name=name,id=id,
					deliveryDate=deliveryDate,deliveryPrice=toMoney(deliveryPrice,info[["numeraire"]])) 
			positionLeg2 <- new("PositionFX_Forward",id=id,security=securityLeg2,
					quantity=quantity,value=quantity)
			
			# create a list of positions
			positions <- new("Positions")
			positions[1] <- positionLeg1
			positions[2] <- positionLeg2
			return(positions)
		}
)