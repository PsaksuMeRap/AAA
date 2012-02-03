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
	ID_Ayrton <- new("ID_Ayrton",ID_AAA=origin[["ID_AAA"]],
			ID_strumento=origin[["ID_strumento"]])
	
	identifyInstrument <- function(record) {
		# record: a list
		
		# create the repository of the instruments if not available
		if (!exists("instruments",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments <- create_repositoryInstruments())
					,env=repositories)
		}
		
		securityName <- repositories$instruments$getInstrumentName(record["ID_strumento"])
		if (is.na(securityName)) {
			msg <- paste("Attenzione: lo strumento di ID",
					record[["ID_strumento"]],"non esite!")
			stop(msg)
		}
		return(securityName)
	}
	
	securityType <- identifyInstrument(origin)
	if (identical(securityType,"equity")) {
		equity <- new("Equity",name=origin[["Nome"]],ID=ID_Ayrton)
		return(equity)
	}
	
	if (identical(securityType,"bond")) {
		# se si tratta di accrued interest non considerarli ora
		# verranno considerati solo nella costruzione delle posizioni
		
		if (origin[["Strumento"]]=="Oacc") return(NULL)
		
		getMaturity <- function(origin) {
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
		
		bond <- new("Bond",name=origin[["Nome"]],ID=ID_Ayrton,maturity=maturity)
		
		
		if (origin[["Strumento"]]=="Oacc") {
			# class(position) <- c("accruedInterest",class(position))
			
			stop("trovato accrued interest")
		}
		
		return(bond)
		
	}
	
	stop(paste("Error: unknown securityType",securityType))
}

