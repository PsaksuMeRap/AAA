# TODO: Add comment
# 
# Author: claudio
###############################################################################
source("./lib/setMethod/securityFactory/createSecurity.R")


setGeneric("idFactory",def=function(origin,...) standardGeneric("idFactory"))

setMethod("idFactory",signature(origin="AyrtonPosition"),
		function(origin) {
			
			# identify the security Type according to Ayrton classification
			securityType <- identifyInstrumentType(origin)
			if (identifyOnly) return(securityType)
			
			class(origin) <- securityType
			
			security <- createSecurity(origin)
			return(security)
		}
)

