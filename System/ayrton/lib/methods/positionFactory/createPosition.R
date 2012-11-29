# TODO: Add comment
# 
# Author: claudio
###############################################################################

setGeneric("createPosition",
		useAsDefault=function(security,origin) {

			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("Position",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value)
			
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


setMethod("createPosition",signature(security="Conto_corrente",origin="AyrtonPosition"),
		function(security,origin) {

			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			
			if (is(security,"Conto_corrente_fittizio")) {
				position <- new("PositionConto_corrente_fittizio",
						id=security@id,
						security=security,
						quantity=1.0,
						value=value)
				
				return(position)				
			}
			
			position <- new("PositionConto_corrente",
					id=security@id,
					security=security,
					quantity=1.0,
					value=value)
			
			return(position)
			
		}
)

setMethod("createPosition",signature(security="Futures_EQ",origin="AyrtonPosition"),
		function(security,origin) {

			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			
			getValueOnePoint <- function(security) {

				result <- strsplit(security@name,"/")
				valueOnePoint <- as.numeric(result[[1]][[length(result[[1]])]])
				valueOnePoint <- toMoney(valueOnePoint,security@currency)
				return(valueOnePoint)
			}

			position <- new("PositionFutures_EQ",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					valueOnePoint=getValueOnePoint(security),
					indexLevel=origin@PrezzoMercato)
		
			return(position)
		}
)

setMethod("createPosition",signature(security="Equity",origin="AyrtonPosition"),
		function(security,origin) {
		
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("PositionEquity",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Bond",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method

			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))

			
			if (length(origin@rating)==0) origin@rating <- "NR"
			if (is.na(origin@rating)) origin@rating <- "NR"
			if (origin@rating=="#N/A N/A" | origin@rating=="#N/A N Ap") origin@rating <- "NR"
			
			rating <- longTermRatingFactory(origin@rating)
			
			position <- new("PositionBond",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					rating=rating,
					accruedInterest=accruedInterest)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Obbligazioni_convertibili",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method
		
			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
			
			if (length(origin@rating)==0) origin@rating <- "NR"
			if (is.na(origin@rating)) origin@rating <- "NR"
			if (origin@rating=="#N/A N/A" | origin@rating=="#N/A N Ap") origin@rating <- "NR"
			
			rating <- longTermRatingFactory(origin@rating)
			
			emptyEquity <- new("Equity",id=new("IdCharacter",NA_character_))
			emptyId <- new("IdCharacter",NA_character_)
			optionSecurity <- new("Opzioni_su_azioni",currency=new("Currency",origin@Moneta),optionType="C",
					id=emptyId,underlying=emptyEquity)
			positionCall <- new("PositionOpzioni_su_azioni",numberEquities=0,contractSize=0,
								quantity=0,value=toMoney(0,origin@Moneta),
								security=optionSecurity,id=emptyId)
			
			
			position <- new("PositionObbligazioni_convertibili",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					rating=rating,
					positionCall=positionCall,
					accruedInterest=accruedInterest)
			
			return(position)
		}
)


setMethod("createPosition",signature(security="Anticipi_fissi",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method
			
			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
			position <- new("PositionAnticipi_fissi",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					accruedInterest=accruedInterest)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Depositi_a_termine",origin="AyrtonPosition"),
		function(security,origin) {
			# the position will be completed with the accruedInterest after an
			# appropriate call to the createPositions method
			
			quantity <- new("NominalValue",amount=new("Amount",origin@Saldo),currency=new("Currency",origin@Moneta))
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
			position <- new("PositionDepositi_a_termine",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					accruedInterest=accruedInterest)
			
			return(position)
		}
)


setMethod("createPosition",signature(security="Fondi_misti",origin="AyrtonPosition"),
		function(security,origin) {
			
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
			position <- new("PositionFondi_misti",
					id=security@id,
					security=security,
					quantity=quantity,
					value=value,
					bondPart=bondPart,
					equityPart=equityPart)
			
			return(position)
		}
)

setMethod("createPosition",signature(security="Fondi_obbligazionari",origin="AyrtonPosition"),
		function(security,origin) {
			
			quantity <- origin@Saldo
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
		
			# identify if the fund is from OpenCapital
			fundsOpenCapital <- create_fundsDB()
			ID_STRUMENTI <- sapply(fundsOpenCapital,slot,"id")
			
			if (length(origin@rating)==0) origin@rating <- "NR"
			if (is.na(origin@rating)) origin@rating <- "NR"
			if (origin@rating=="#N/A N/A" | origin@rating=="#N/A N Ap") origin@rating <- "NR"
			
			rating <- longTermRatingFactory(origin@rating)
			
			
			if (is.element(security@id@idAAA,ID_STRUMENTI)) {
				accruedInterest <- new("AccruedInterest",toMoney(NA_real_,security@currency))
				position <- new("PositionFondi_obbligazionariOC",
						accruedInterest=accruedInterest,
						id=security@id,
						security=security,
						quantity=quantity,
						rating=rating,
						value=value)
				return(position)
			} else {
				accruedInterest <- new("AccruedInterest",toMoney(0.0,security@currency))
				position <- new("PositionFondi_obbligazionari",
						accruedInterest=accruedInterest,
						id=security@id,
						security=security,
						quantity=quantity,
						rating=rating,
						value=value)
				return(position)
			}
		}
)

setMethod("createPosition",signature(security="Opzioni_su_azioni",origin="AyrtonPosition"),
		function(security,origin) {

			## ! for Opzioni_su_azioni this is not the number of contracts but it is the
			## corresponding number of underlying equities
			info <- getOptionParameters(origin)
			numberEquities <- origin@Saldo*info[["contractSize"]]
			
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			position <- new("PositionOpzioni_su_azioni",
					id=security@id,
					security=security,
					numberEquities=numberEquities,
					quantity=info[["quantity"]],
					contractSize=info[["contractSize"]],
					value=value)
			
			return(position)
	
		}
)


setMethod("createPosition",signature(security="Opzioni_su_divise",origin="AyrtonPosition"),
		function(security,origin) {
			
			class(origin) <- "Ayrton_Opzioni_su_divise"
			info <- getOptionParameters(origin)
			
			value <- toMoney(origin@ValoreMercatoMonetaCHF,new("Currency","CHF"))
			value <- repositories$exchangeRates$exchange(value,security@currency)
			
			position <- new("PositionOpzioni_su_divise",
					id=security@id,
					security=security,
					quantity=toMoney(origin@Saldo,info$underlying),
					value=value)
			
			return(position)
			
		}
)

