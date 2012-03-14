# TODO: Add comment
# 
# Author: claudio
###############################################################################



createSecurity <- function(origin) UseMethod("createSecurity")

setGeneric("createSecurity",
		useAsDefault=function(origin) {
			# this is a common slot of all instruments
			idAyrton <- new("IdAyrton",idAAA=origin@ID_AAA,
					idStrumento=origin@ID_strumento)
			
			security <- new(class(origin),currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton)
			return(security)
		}
)

setMethod("createSecurity",signature(origin="Bond"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- new("IdAyrton",idAAA=origin@ID_AAA,
					idStrumento=origin@ID_strumento)
			
			getMaturity <- function() {
				# extract the maturity
				name <- origin@Nome
				paymentDate <- substr(name,nchar(name)-8+1,
						nchar(name))
				
				# verifica che il nome sia una data
				day <- substr(paymentDate,1,2)
				month <- substr(paymentDate,4,5)	
				year <- paste("20",substr(paymentDate,7,8),sep="")		
				paymentDate <- paste(year,month,day,sep="-")
				
				if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=paymentDate,perl=TRUE)) {
					message <- as.character(position)
					message <- paste("Invalid date parsed",paymentDate,"for security:\n",name,"of type bond")
					stop(message)
				}
				return(paymentDate)
			}
			
			security <- new(class(origin),currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,maturity=getMaturity())
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Fondi_obbligazionari"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			callNextMethod()
			
		}
)

setMethod("createSecurity",signature(origin="Strutturati_FI"),
		function(origin) {
			
			# this is a common slot of all instruments
			idAyrton <- new("IdAyrton",idAAA=origin@ID_AAA,
					idStrumento=origin@ID_strumento)
			
			# extract the name of the security
			name <- origin@Nome
			
			# check if the underlying is a short term fixed income position
			if (grepl("<3Y",x=name)) underlyingHorizon = "<3Y"
			if (grepl(">3Y",x=name)) underlyingHorizon = ">3Y"
			
			# determine the expiry date of the structured product
			year <- substr(name,1,4)
			month <- substr(name,5,6)
			day <- substr(name,7,8)
			expiryDate = paste(year,month,day,sep="-")
			
			security <- new(class(origin),currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,
					expiryDate=expiryDate,underlyingHorizon=underlyingHorizon)
			return(security)	
		}
)