# TODO: Add comment
# 
# Author: claudio
###############################################################################


securityFactory <- function(origin) UseMethod("securityFactory")

securityFactory.default <- function(origin) {
	stop(paste("No suitable securityFactory method for origin of class",class(origin)))
}

securityFactory.ayrton <- function(origin) {
	
	# this is a common slot of all instruments
	idAyrton <- new("IdAyrton",idAAA=origin[["ID_AAA"]],
			idStrumento=origin[["ID_strumento"]])
	
	identifyInstrumentType <- function(record) {
		# record: a list
		
		# create the repository of the instruments if not available
		if (!exists("instruments",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments <- create_repositoryInstruments())
					,env=repositories)
		}
		
		securityType <- repositories$instruments$getInstrumentName(record["ID_strumento"])
		if (is.na(securityType)) {
			msg <- paste("Attenzione: lo strumento di ID",
					record[["ID_strumento"]],"non esite!")
			stop(msg)
		}
		return(securityType)
	}
	
	securityType <- identifyInstrumentType(origin)

	if (identical(securityType,"equity")) {
		equity <- new("Equity",name=origin[["Nome"]],id=idAyrton)
		return(equity)
	}
	
	if (identical(securityType,"bond")) {
		# se si tratta di accrued interest non considerarli ora
		# verranno considerati solo nella costruzione delle posizioni
		
		if (origin[["Strumento"]]=="Oacc") return(NULL)
		
		getMaturity <- function() {
			# extract the maturity
			name <- origin[["Nome"]]	
			paymentDate <- substr(name,nchar(name)-8+1,
					nchar(name))
			
			# verifica che il nome sia una data
			day <- substr(paymentDate,1,2)
			month <- substr(paymentDate,4,5)	
			year <- paste("20",substr(paymentDate,7,8),sep="")		
			paymentDate <- paste(year,month,day,sep="-")
			
			if (!grepl(pattern="[0-9]{4}-[0-9]{2}-[0-9]{2}",x=paymentDate,perl=TRUE)) {
				message <- position$toString()
				message <- paste("Invalid date parsed",paymentDate,"for security:\n",name,"of type bond")
				stop(message)
			}
			return(paymentDate)
		}
		
		bond <- new("Bond",name=origin[["Nome"]],id=idAyrton,maturity=getMaturity())
		return(bond)
		
	}
	
	stop(paste("Error: unknown securityType",securityType))
}

