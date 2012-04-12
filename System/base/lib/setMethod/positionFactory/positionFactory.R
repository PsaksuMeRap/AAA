# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./base/lib/setMethod/positionFactory/createPosition.R")

setGeneric("positionFactory",def=function(origin) standardGeneric("positionFactory"))

#setMethod("positionFactory",signature(origin="AyrtonPosition"),
#		function(origin) {
#
#			security <- securityFactory(origin)
#
#			return(createPosition(security,origin))
#		}
#)
