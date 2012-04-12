# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./ayrton/lib/methods/positionFactory/createPosition.R")

setMethod("positionFactory",signature(origin="AyrtonPosition"),
		function(origin) {

			security <- securityFactory(origin)

			return(createPosition(security,origin))
		}
)
