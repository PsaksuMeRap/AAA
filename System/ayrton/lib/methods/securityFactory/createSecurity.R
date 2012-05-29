# TODO: Add comment
# 
# Author: claudio
###############################################################################


setMethod("createSecurity",signature(origin="Ayrton_Bond"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
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
					message <- paste("Invalid date parsed",paymentDate,"for security:\n",name,"of type bond")
					stop(message)
				}
				return(paymentDate)
			}
			
			className <- substr("Ayrton_Bond",start=8,stop=nchar("Ayrton_Bond"))
			security <- new(className,currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,maturity=getMaturity())
			return(security)	
		}
)


setMethod("createSecurity",signature(origin="Ayrton_Anticipi_fissi"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			getMaturity <- function() {
				# extract the maturity
				name <- origin@Nome
				maturity <- substr(name,25,32)
				
				# verifica che il nome sia una data
				day <- substr(maturity,1,2)
				month <- substr(maturity,4,5)	
				year <- paste("20",substr(maturity,7,8),sep="")		
				maturity <- paste(year,month,day,sep="-")
				
				if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=maturity,perl=TRUE)) {
					message <- paste("Invalid date parsed",maturity,"for security:\n",name,"of type Anticipi_fissi")
					stop(message)
				}
				return(maturity)
			}
			
			className <- substr("Ayrton_Anticipi_fissi",start=8,stop=nchar("Ayrton_Anticipi_fissi"))
			security <- new(className,currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,maturity=getMaturity())
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Depositi_a_termine"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			getMaturity <- function() {
				# extract the maturity
				name <- origin@Nome
				maturity <- substr(name,27,34)
				
				# verifica che il nome sia una data
				day <- substr(maturity,1,2)
				month <- substr(maturity,4,5)	
				year <- paste("20",substr(maturity,7,8),sep="")		
				maturity <- paste(year,month,day,sep="-")
				
				if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=maturity,perl=TRUE)) {
					message <- paste("Invalid date parsed",maturity,"for security:\n",name,"of type Depositi_a_termine")
					stop(message)
				}
				return(maturity)
			}
			
			className <- substr("Ayrton_Depositi_a_termine",start=8,stop=nchar("Ayrton_Ayrton_Depositi_a_termine"))
			security <- new(className,currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,maturity=getMaturity())
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Fondi_obbligazionari"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			callNextMethod()
			
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Strutturati_FI"),
		function(origin) {
			
			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
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
			
			className <- substr("Ayrton_Strutturati_FI",start=8,stop=nchar("Ayrton_Strutturati_FI"))
			security <- new(className,currency=new("Currency",origin@Moneta),name=origin@Nome,id=idAyrton,
					expiryDate=expiryDate,underlyingHorizon=underlyingHorizon)
			return(security)	
		}
)