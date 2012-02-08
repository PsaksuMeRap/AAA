# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/useMethods/positionFactory/createPositionFromAyrton.R")

setGeneric("positionFactory")

fun <- function(origin) {
	security <- securityFactory(origin)
	return(createPositionFromAyrton(security,origin))
}

setMethod("positionFactory",signature(origin="ayrtonRecord"))