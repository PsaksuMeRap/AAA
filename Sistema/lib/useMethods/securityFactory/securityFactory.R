# TODO: Add comment
# 
# Author: claudio
###############################################################################
source("./lib/useMethods/securityFactory/createSecurityFromAyrton.R")

securityFactory <- function(origin,...) UseMethod("securityFactory")

securityFactory.default <- function(origin,...) {
	stop(paste("No suitable securityFactory method for origin of class",class(origin)))
}

securityFactory.ayrton <- function(origin,identifyOnly=FALSE) {
		
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
	
	# identify the security Type according to Ayrton classification
	securityType <- identifyInstrumentType(origin)
	if (identifyOnly) return(securityType)
	
	class(origin) <- securityType

	security <- createSecurityFromAyrton(origin)
	return(security)
}

