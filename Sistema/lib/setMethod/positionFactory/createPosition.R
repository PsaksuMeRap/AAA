# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("createPosition",
		useAsDefault=function(security,origin) {
			id <- 10.2
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("Position",id=id,security=security,
					quantity=quantity,value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="NULL",origin="AyrtonPosition"),
		function(security,origin) {
			# if security is of class NULL, i.e. from an AccruedInterest
			# return NULL
			return(NULL)
		}
)

setMethod("createPosition",signature(security="Equity",origin="AyrtonPosition"),
		function(security,origin) {

			id <- 10.2
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("PositionEquity",id=id,security=security,
					quantity=quantity,value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Bond",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method

			id <- 10.2
			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
			position <- new("PositionBond",id=id,security=security,
					quantity=quantity,value=value,accruedInterest=accruedInterest)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Fondi_misti",origin="AyrtonPosition"),
		function(security,origin) {
			id <- 10.2
			quantity <- origin@Saldo
		
			split1 <- strsplit(security@name," ")
			split2 <- unlist(strsplit(split1[[1]][1],"-"))
			errorMessage <- paste("Error when parsing Fondi_misti:",security@name,"\nThe % weights are missing!")
			
			if (length(split2)!=2) stop(errorMessage)
			
			if (all(grepl("^[0-9]+.*[0-9]*$",split2))) {
				bondPart <- as.numeric(split2[1])
				equityPart <- as.numeric(split2[2])
			} else {
				stop(errorMessage)
			}
				
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("PositionFondi_misti",id=id,security=security,
					quantity=quantity,value=value,bondPart=bondPart,equityPart=equityPart)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Fondi_obbligazionari",origin="AyrtonPosition"),
		function(security,origin) {
				
			# identify if the fund is from OpenCapital
			fundsOpenCapital <- create_fundsDB()
			ID_STRUMENTI <- sapply(fundsOpenCapital,slot,"id")
			if (is.element(origin@ID_AAA,ID_STRUMENTI)) {
				id <- 10.2
				quantity <- origin@Saldo
				value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
				value <- repositories$exchangeRates$exchange(value,security@currency)
				
				accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
				position <- new("PositionFondiObbligazionariOC",accruedInterest=accruedInterest,id=id,security=security,
						quantity=quantity,value=value)
				return(position)
			} else {
				callNextMethod(security,origin)
			}
		}
)

