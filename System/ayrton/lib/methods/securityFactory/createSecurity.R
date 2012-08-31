# TODO: Add comment
# 
# Author: claudio
##############g)#################################################################

setMethod("createSecurity",signature(origin="Ayrton_Futures_EQ"),
		function(origin) {
			
			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
			className <- class(origin)
			className <- substr(className,start=8,stop=nchar(className))
			
			underlying <- new("IndexEquity",name=origin@Nome,id=idAyrton)

			# get the deliveryDate			
			getDeliveryDate <- function(origin) {

				result <- strsplit(origin@Nome,"/")
				result <- result[[1]][[length(result[[1]])-1]]
				stringLength <- nchar(result)
				dateString <- substr(result,stringLength-10,stringLength-1)
				deliveryDate <- format(strptime(dateString,format="%d-%m-%Y"),"%Y-%m-%d")
				return(deliveryDate)
			}
			
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,underlying=underlying,
					deliveryDate=getDeliveryDate(origin))
			return(security)
		}
)


setMethod("createSecurity",signature(origin="Ayrton_Opzioni_su_divise"),
		function(origin) {
			
			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
			className <- class(origin)
			className <- substr(className,start=8,stop=nchar(className))
			
			info <- getOptionParameters(origin)

			security <- new(className,
					currency=new("Currency",origin@Moneta),
					name=origin@Nome,
					id=new("IdCharacter",origin@Nome),
					underlying=new("Currency",info$underlying),
					expiryDate=info$expiryDate,
					optionType=info$optionType,
					strike=info[["strike"]]) 
						
			return(security)
		}
)


setMethod("createSecurity",signature(origin="Ayrton_Opzioni_su_azioni"),
		function(origin) {
		
			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
			className <- class(origin)
			className <- substr(className,start=8,stop=nchar(className))
	
			info <- getOptionParameters(origin)
		
			# identify the underlying equity
			underlying <- createEquitySecurityFromIsin(info[["isin"]])
			## adjust for the missing currency in case that the underlying equity is not in the DBEquity
			if (identical(underlying@currency,new("Currency"))) underlying@currency <- new("Currency",origin@Moneta)
		
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,underlying=underlying,
					strike=info[["strike"]],expiryDate=info[["expiryDate"]],
					optionType=info[["optionType"]])
			
			return(security)
		}
)


setMethod("createSecurity",signature(origin="Ayrton_Obbligazioni_convertibili"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			# extract the maturity from the name
			maturity=getMaturity(origin)
			
			className <- substr("Ayrton_Obbligazioni_convertibili",start=8,stop=nchar("Ayrton_Obbligazioni_convertibili"))
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,maturity=maturity)
			return(security)	
		}
)


setMethod("createSecurity",signature(origin="Ayrton_Bond"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Oacc") return(NULL)
	
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			# extract the maturity from the name
			maturity=getMaturity(origin)
			
			className <- substr("Ayrton_Bond",start=8,stop=nchar("Ayrton_Bond"))
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,maturity=maturity)
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Fondi_obbligazionari"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
		
			if (origin@Strumento=="Oacc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idFactory(origin)
			
			# extract the maturity from the name
			maturity=getMaturity(origin)
			
			className <- substr("Ayrton_Fondi_obbligazionari",start=8,stop=nchar("Ayrton_Fondi_obbligazionari"))
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,maturity=maturity)
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Anticipi_fissi"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Macc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			# extract the maturity from the name
			maturity = getMaturity(origin)
			
			className <- substr("Ayrton_Anticipi_fissi",start=8,stop=nchar("Ayrton_Anticipi_fissi"))
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,maturity=maturity)
			return(security)	
		}
)

setMethod("createSecurity",signature(origin="Ayrton_Depositi_a_termine"),
		function(origin) {
			# se si tratta di accrued interest non considerarli ora,
			# verranno considerati solo nella costruzione delle posizioni
			
			if (origin@Strumento=="Macc") return(NULL)
			
			# this is a common slot of all instruments
			idAyrton <- idAyrton <- idFactory(origin)
			
			# extract the maturity from the name
			maturity=getMaturity(origin)
			
			className <- substr("Ayrton_Depositi_a_termine",start=8,stop=nchar("Ayrton_Ayrton_Depositi_a_termine"))
			security <- new(className,currency=new("Currency",origin@Moneta),
					name=origin@Nome,id=idAyrton,maturity=maturity)
			return(security)	
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