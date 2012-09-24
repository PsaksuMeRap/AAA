# TODO: Add comment
# 
# Author: Claudio
###############################################################################
parseOptionFxName <- function(name) {
	tmp <- strsplit(name,"\\s+")[[1]]
	
	# identify the two currencies codes
	currencyCodes <- tmp[1]
	underlying <- toupper(substr(currencyCodes,1,3))
	numeraire <- toupper(substr(currencyCodes,4,6))
	
	# identify the expiryDate
	expiryDate <- tmp[2]
	# monthAndDay <- substr(expiryDate,1,5)
	twoDigitsYear <- substr(expiryDate,7,8)
	expiryDate <- paste(substr(expiryDate,1,6),"20",twoDigitsYear,sep="")
	expiryDate <- format(strptime(expiryDate,format="%m/%d/%Y"),"%d-%m-%Y")
	
	# identify the option type
	optionType <- substr(tmp[3],1,1)
	if (optionType=="c") {
		optionType <- "Call" 
	} else {
		optionType <- "Put" 
	}
	
	# identify the strike
	strike <- as.numeric(substr(tmp[3],2,nchar(tmp[3])))
	
	return(list(name=name,expiryDate=expiryDate,optionType=optionType,
					strike=strike,underlying=underlying,numeraire=numeraire))
}

parseFxForwardName <- function(name) {
	tmp <- strsplit(name,"\\s+")[[1]]
	
	# identify the two currencies codes, the underlying and the 
	# numeraire
	
	currencyCodes <- toupper(tmp[1])
	underlying <- toupper(substr(currencyCodes,1,3))
	numeraire <- toupper(substr(currencyCodes,4,6))
	
	# identify the deliveryDate
	deliveryDate <- tmp[2]
	monthAndDay <- substr(deliveryDate,1,5)
	twoDigitsYear <- substr(deliveryDate,7,8)
	deliveryDate <- paste(substr(deliveryDate,1,6),"20",twoDigitsYear,sep="")
	
	return(list(currencyCodes=currencyCodes,underlying=underlying,
					numeraire=numeraire,deliveryDate=deliveryDate))
}

parseFxSpotId_Bloomberg <- function(IdBloomberg) {
	tmp <- strsplit(IdBloomberg,"\\s+")[[1]]
	
	# identify the two currencies codes, the underlying and the 
	# numeraire
	
	currencyCodes <- toupper(tmp[1])
	underlying <- substr(currencyCodes,1,3)
	numeraire <- substr(currencyCodes,4,6)
	
	return(list(underlying=underlying,numeraire=numeraire))
}

tradeToSecurityFactory <- function(trade,blRequestHandler) {
	# determine the security type
	securityType <- trade$Security_type

	if (securityType=="Equity") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the last price
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		
		newSecurity <- new("Equity",currency=currency,name=name,id=id) 
		return(newSecurity)		
	}
	
	if (securityType=="Fund equity") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the last price
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		
		newSecurity <- new("Fondi_azionari",currency=currency,name=name,id=id) 
		return(newSecurity)		
	}

	if (securityType=="Fund bond") {
		currency <- new("Currency",trade$Currency)
		name <- paste(sysAyrton[["Fondi_obbligazionari"]][["preNameString"]],trade$Security_name,sep=" - ")
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the last price
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		
		newSecurity <- new("Fondi_obbligazionari",currency=currency,name=name,id=id,
				maturity=sysAyrton[["Fondi_obbligazionari"]][["maturity"]]) 
		return(newSecurity)		
	}
	
	if (securityType=="Future index") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the ticker of the underlying
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"UNDL_SPOT_TICKER")
		# collect the delivery date
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"FUT_DLV_DT_FIRST")
		# collect the last price
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		# collect the value of one point
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"FUT_VAL_PT")
		
		newSecurity <- new("Futures_EQ",currency=currency,name=name,id=id,underlying=new("IndexEquity")) 
		return(newSecurity)
	}
	
	if (securityType=="Bond") {
		currency <- new("Currency",trade$Currency)
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the last price (clean) 102.284
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		
		# collect the accrued interest (%) to be added to the clean price 1.7
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"INT_ACC")
		
		# collect the rating AA
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"RTG_SP")	
		
		# collect the maturity 05/08/2013
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"MATURITY")	
		
		newSecurity <- new("Bond",currency=currency,name=name,id=id) 
		return(newSecurity)
	}
	
	if (securityType=="FX Spot") {
		
		## by convention the currency is the numeraire (the second currency)
		## while the quantity is expressed in the underlying currency
		
		info <- parseFxSpotId_Bloomberg(trade$Id_Bloomberg)

		underlyingCurrency <- new("Currency",info[["underlying"]])

		name <- paste(info[["underlying"]],"-",tolower(info[["underlying"]])," ",trade$Id_Bloomberg,sep="")
		id <- new("IdBloomberg",name)
		
		# collect the last price (clean) 1200071
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		
		newSecurity <- new("Conto_corrente",currency=underlyingCurrency,name=name,id=id) 
		return(newSecurity)
	}
	
	if (securityType=="Option Equity") {
		
		currency <- new("Currency",trade$Currency)
		
		name <- trade$Security_name
		id=new("IdBloomberg",trade$Id_Bloomberg)
		
		# collect the ticker of the underlying NESN VX
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"OPT_UNDL_TICKER")
		# collect the expiry date 06/05/2012
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"OPT_EXPIRE_DT")
		# collect the strike price 55
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"OPT_STRIKE_PX")
		# collect the last price 0.16
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"LAST_PRICE")
		# collect the option type c
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"OPT_PUT_CALL")
		# collect the option contract size 	100
		blRequestHandler[["collect"]](trade$Id_Bloomberg,"OPT_CONT_SIZE")
	
		
		newSecurity <- new("Opzioni_su_azioni",currency=currency,name=name,id=id,underlying=new("Equity")) 
		return(newSecurity)
	}
	
	if (securityType=="Option FX") {
		
		# il nome della security andrebbe modificato in quanto non dovrebbe contenere
		# informazioni sulla posizione
		currency <- new("Currency",trade$Currency)
		
		name <- trade$Security_name
				
		info <- parseOptionFxName(name)
		
		# the strike is expressed as the value of 1 unit of the first currency (the underlying) 
		# w.r.t. the second currency (the numeraire). 
		# For underlying as NOK or SEK it is the value of 100 NOK in the second currency.
	
		underlying = new("Currency",info[["underlying"]])
		id=new("IdCharacter",name)
		
		expiryDateYYYMMDD <- format(strptime(info[["expiryDate"]],format="%d-%m-%Y"),"%Y-%m-%d") 
		newSecurity <- new("Opzioni_su_divise",currency=currency,name=name,id=id,underlying=underlying,
				expiryDate=expiryDateYYYMMDD,optionType=info[["optionType"]],strike=info[["strike"]]) 
		return(newSecurity)
	}
	
	if (securityType=="FX Forward") {
		
		info <- parseFxForwardName(trade$Security_name)
		
		# currency is the currency used to express the price of x units of
		# the underlying. We use the bloomberg convention, i.e. usdeur means
		# the price in eur of 1 unit of usd and sekusd the price of 100 sek in
		# usd
	
		if (info[["underlying"]]!=trade$Currency) {
			message <- paste("Error in trade ",trade$Id_Bloomberg,". \n\n",
					"The currency used for the Quantity field is ",trade$Currency,". \n",
					"It should be ",info[["underlying"]],sep="")
			tkmessageBox(message=message,icon="error",type="ok")
		}
		
		currency <- new("Currency",info[["numeraire"]])
		
		name <- paste(info$currencyCodes,info$deliveryDate)
		id=new("IdCharacter",name)
		
		newSecurity <- new("FX_Forward",currency=currency,name=name,id=id)
		
		return(newSecurity)
	}
	
}