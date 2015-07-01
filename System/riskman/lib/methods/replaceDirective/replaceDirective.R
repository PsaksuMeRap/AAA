# TODO: Add comment
# 
# Author: claudio
###############################################################################



setGeneric("replaceDirective",def=function(position,...) standardGeneric("replaceDirective"))

## 2012-09-18
## Questo metodo e' stato rimosso perche' i futuri sono conteggiati diversamente in DBPortfolioGenerale
#setMethod("replaceDirective",
#		signature(position="PositionFutures_EQ"),
#		function(position) {
			
#			positionValue <- (position@quantity * position@indexLevel) * position@valueOnePoint
			
#			position@value <- positionValue
			
			# crate the corresponding money flow
#			currency <- position@security@currency
#			nameAndId <- paste(currency,"- ",gsub(" / ","  ",position@security@name),sep="")
#			securityConto_corrente <- new("Conto_corrente",currency=currency,
#					name=nameAndId,id=new("IdCharacter",nameAndId))
#			positionConto_corrente <- new("PositionConto_corrente",security=securityConto_corrente,
#					value=-1 *position@value,quantity=1,id=new("IdCharacter",nameAndId))
			
#			return(new("Positions",list(position,positionConto_corrente)))
#		}
#)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_azioni"),
		function(position) {
			# the following rules apply:
			# long put: we return the worst case scenario, i.e. at expiry a cash position of 0
			# short put: a negative cash position and the corresponding underlying position
			# long call: a negative cash position and the corresponding underlying position
			# short call: the corresponding underlying position
			
			# if long put
			if (position@security@optionType=="P" & position@quantity >=0) {
				# create a cash position of 0
				currency <- as.character(position@security@currency)
				idCharacter <- paste(currency,tolower(currency),sep="-")
				id <- new("IdAyrton",
						idAAA=new("IdAAA_character",idCharacter),
						idStrumento=40)
				
				name <- paste(idCharacter,"from",position@security@name)
				securityConto_corrente <- new("Conto_corrente",
						currency=new("Currency",currency),
						name=name,id=id)
				
				positionConto_corrente <- new("PositionConto_corrente",
						id=id,
						security=securityConto_corrente,
						value=toMoney(0,position@security@currency),
						quantity=1.0)
				return(new("Positions",list(positionConto_corrente)))
			} 
			
			# get the option parameters
			info <- getOptionParameters(position)

			## construct the equity leg of the position
			securityEquity <- as(position@security@underlying,"Equity")
			securityEquity@name <- paste(securityEquity@name,"-",gsub(" / ","  ",position@security@name))
			
			## compute the value of the position
			currency <- position@security@currency
			if (position@security@optionType=="C") {
				quantity <- position@numberEquities
				value1 <- toMoney(info[["underlyingPrice"]] * quantity,currency)
				value2 <- toMoney(-position@security@strike * position@numberEquities,currency)
			} else {
				if (position@security@optionType=="P") {
					quantity <- -position@numberEquities
					value1 <- toMoney(info[["underlyingPrice"]] * quantity,currency)
					value2 <- toMoney(position@security@strike * position@numberEquities,currency)
				} else {
					stop("Invalid optionType in replaceDirective:PositionOpzioni_su_azioni.")
				}
				
			}
			
			equityPosition <- new("PositionEquity",
					id=securityEquity@id,
					security=securityEquity,
					quantity=quantity,
					value=value1)
			
			
			# create the corresponding money flow
			currency <- as.character(position@security@currency)
			idCharacter <- paste(currency,tolower(currency),sep="-")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idCharacter),
					idStrumento=40)
			
			name <- paste(idCharacter,"from",position@security@name)
			securityConto_corrente <- new("Conto_corrente",
					currency=new("Currency",currency),
					name=name,id=id)
			
			positionConto_corrente <- new("PositionConto_corrente",
					id=id,
					security=securityConto_corrente,
					value=value2,
					quantity=1.0)
			
			
			if (position@security@optionType=="C" & position@quantity >=0) return(new("Positions",list(equityPosition,positionConto_corrente)))
			if (position@security@optionType=="P" & position@quantity <0) return(new("Positions",list(equityPosition,positionConto_corrente)))
			if (position@security@optionType=="C" & position@quantity <0) return(new("Positions",list(equityPosition)))
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionOpzioni_su_divise"),
		function(position) {
			
			## construct the leg based on the underlying currency
			currency <- as.character(position@security@underlying)
			idCharacter <- paste(currency,tolower(currency),sep="-")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idCharacter),
					idStrumento=40)
			
			name <- paste(idCharacter,"from",position@security@name)
			
			securityConto_corrente1 <- new("Conto_corrente",
					currency=new("Currency",currency),name=name,id=id)
			
			## compute the value of the position
			if (position@security@optionType=="C") {
				value1 <- position@quantity
				value2 <- -position@security@strike * position@quantity
				value2@currency <- position@security@currency
			} else {
				if (position@security@optionType=="P") {
					value1 <- -1 * position@quantity
					value2 <- position@security@strike * position@quantity
					value2@currency <- position@security@currency
				} else {
					stop("Invalid optionType in replaceDirective:PositionOpzioni_su_divise.")
				}
			}
			positionConto_corrente1 <- new("PositionConto_corrente",
					security=securityConto_corrente1,
					id=securityConto_corrente1@id,
					value=value1,
					quantity=1.0)
			
			## create the corresponding money flow
			currency <- position@security@currency
			idCharacter <- paste(currency,tolower(currency),sep="-")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idCharacter),
					idStrumento=40)
			
			name <- paste(idCharacter,"from",position@security@name)
			
			securityConto_corrente2 <- new("Conto_corrente",
					currency=new("Currency",currency),
					name=name,id=id)
			

			positionConto_corrente2 <- new("PositionConto_corrente",
					security=securityConto_corrente2,
					id=securityConto_corrente2@id,
					value=value2,
					quantity=1.0)
			
			return(new("Positions",list(positionConto_corrente1,positionConto_corrente2)))		
			
		}
)

setMethod("replaceDirective",
		signature(position="PositionStrutturati_FX"),
		function(position) {
			#"20150918 - Put Warrant Vontobel EUR/CHF CHF 1"
			
			## construct the leg based on the currency2, CHF
	
			idCharacter <- paste0(position@security@name,"-leg2")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idCharacter),
					idStrumento=40)
			
			name <- paste("Leg 2 from",position@security@name)
			
			securityConto_corrente1 <- new("Conto_corrente",
					currency=position@quantity@currency,name=name,id=id)
			
			positionConto_corrente1 <- new("PositionConto_corrente",
					security=securityConto_corrente1,
					id=securityConto_corrente1@id,
					value=position@quantity,
					quantity=1.0)
			
			## create the corresponding money flow
			idCharacter <- paste0(position@security@name,"-leg1")
			id <- new("IdAyrton",
					idAAA=new("IdAAA_character",idCharacter),
					idStrumento=40)
			
			name <- paste("Leg 1 from",position@security@name)
			
			securityConto_corrente2 <- new("Conto_corrente",
					currency=position@otherLeg@currency,
					name=name,id=id)
			
			positionConto_corrente2 <- new("PositionConto_corrente",
					security=securityConto_corrente2,
					id=securityConto_corrente2@id,
					value=-1*position@otherLeg,
					quantity=1.0)
			
			return(new("Positions",list(positionConto_corrente1,positionConto_corrente2)))		
			
		}
)
