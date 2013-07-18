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
				
				id_strumento <- repositories$allocareInvestmentType$getId(origin@Strumento)
				securityType <- repositories$instruments$getInstrumentName(id_strumento)
				if (is.na(securityType)) {
					msg <- paste("Warning: the instrument with ID",
							origin@Strumento,"does not exist!")
					stop(msg)
				}
				return(securityType)
			}
			
			# identify the security Type according to Ayrton classification
			securityType <- identifyInstrumentType(origin)
			if (identifyOnly) return(securityType)
			
			origin <- new(paste("Allocare",securityType,sep="_"),origin)
			
			security <- createSecurity(origin)
			return(security)
		}
)

