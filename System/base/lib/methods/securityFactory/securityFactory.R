# TODO: Add comment
# 
# Author: claudio
###############################################################################
 source("./base/lib/methods/securityFactory/createSecurity.R")


setGeneric("securityFactory",def=function(origin,...) standardGeneric("securityFactory"))

# Per ogni nuova classe di origin occorre creare un metodo securityFactory corrispondente
#setMethod("securityFactory",signature(origin="AyrtonPosition"),
#		function(origin,identifyOnly=FALSE) {
#
#			# define the function used to identify the position type, i.e. Equity, Bond, 
#			# Hedge Fund, ...
#		
#			identifyInstrumentType <- function(origin) {
#				# record: a list
#				
#				# create the repository of the instruments if not available
#				if (!exists("instruments",envir=repositories,inherits=FALSE)) {
#					eval(expression(instruments <- create_repositoryInstruments())
#							,env=repositories)
#				}
#				
#				securityType <- repositories$instruments$getInstrumentName(origin@ID_strumento)
#				if (is.na(securityType)) {
#					msg <- paste("Warning: the instrument with ID",
#							origin@ID_strumento,"does not exist!")
#					stop(msg)
#				}
#				return(securityType)
#			}
#)


