# TODO: Add comment
# 
# Author: claudio
###############################################################################
source("./allocare/lib/methods/securityFactory/createSecurity.R")

setMethod("securityFactory",signature(origin="AllocarePosition"),
		function(origin,identifyOnly=FALSE) {

			# define the function used to identify the position type, i.e. Equity, Bond, 
			# Hedge Fund, ...

			identifyInstrumentType <- function(origin) {
				# record: a list
				
				# create the repository of the instruments if not available
				if (!exists("instruments",envir=repositories,inherits=TRUE)) {
					eval(expression(allocareInvestmentType <- create_repositoryAllocareInvestmentType())
							,env=repositories)
				}
# arrivato qui: devi settare lo strumento				
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
			
			# class(origin) <- paste("Ayrton",securityType,sep="_")
			origin <- new(paste("Ayrton",securityType,sep="_"),origin)
			
			security <- createSecurity(origin)
			return(security)
		}
)

