# TODO: Add comment
# 
# Author: claudio
###############################################################################
source("./ayrton/lib/methods/securityFactory/createSecurity.R")

setMethod("securityFactory",signature(origin="AyrtonPosition"),
		function(origin,identifyOnly=FALSE) {

			# define the function used to identify the position type, i.e. Equity, Bond, 
			# Hedge Fund, ...
		
			identifyInstrumentType <- function(origin) {
				# record: a list
				
				# create the repository of the instruments if not available
				if (!exists("instruments",envir=repositories,inherits=FALSE)) {
					eval(expression(instruments <- create_repositoryInstruments())
							,env=repositories)
				}
				
				securityType <- repositories$instruments$getInstrumentName(origin@ID_strumento)
				if (is.na(securityType)) {
					msg <- paste("Warning: the instrument with ID",
							origin@ID_strumento,"does not exist!")
					stop(msg)
				}
				return(securityType)
			}
			
			# identify the security Type according to Ayrton classification
			securityType <- identifyInstrumentType(origin)
			if (identifyOnly) return(securityType)
			
			class(origin) <- paste("Ayrton",securityType,sep="_")
			
			security <- createSecurity(origin)
			return(security)
		}
)

