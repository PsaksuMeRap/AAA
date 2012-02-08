# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldFailWithNonAyrton <- function() {
	
	# create the origin
	origin <- list()
	class(origin) <- "pippo"
	
	checkException(securityFactory(origin))
}

test.shouldIdentifyAyrtonInstrument <- function() {
	
	# create the origin
	origin <- list()
	class(origin) <- "ayrton"
	origin$ID_strumento <- 2
	
	checkEquals(securityFactory(origin,identifyOnly=TRUE),"Bond")
}

test.shouldFailToIdentifyAyrtonInstrument <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the equity repository and instrument repository
	allocateTestRepositories("instruments")
	
	# create the origin
	origin <- list()
	class(origin) <- "ayrton"
	origin$ID_strumento <- -2
	
	checkException(securityFactory(origin,identifyOnly=TRUE))
	
	deallocateTestRepositories("instruments")	
}

