# TODO: Add comment
# 
# Author: claudio
###############################################################################



createSecurityFromAyrton <- function(origin) UseMethod("createSecurityFromAyrton")

createSecurityFromAyrton.default <- function(origin) {
	# this is a common slot of all instruments
	idAyrton <- new("IdAyrton",idAAA=origin[["ID_AAA"]],
			idStrumento=origin[["ID_strumento"]])
	
	security <- new(class(origin),name=origin[["Nome"]],id=idAyrton)
	return(security)
}

createSecurityFromAyrton.Bond <- function(origin) {
	# se si tratta di accrued interest non considerarli ora
	# verranno considerati solo nella costruzione delle posizioni
	
	if (origin[["Strumento"]]=="Oacc") return(NULL)
	
	# this is a common slot of all instruments
	idAyrton <- new("IdAyrton",idAAA=origin[["ID_AAA"]],
			idStrumento=origin[["ID_strumento"]])
	
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
	
	security <- new(class(origin),name=origin[["Nome"]],id=idAyrton,maturity=getMaturity())
	return(security)	
}