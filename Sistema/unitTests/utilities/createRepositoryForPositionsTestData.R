# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryForPositionsTestData <- function() {
	
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryForOriginTestData()
	
	testData <- new.env()

	# create a valid equity
	testData$equity1 <- positionFactory(repository$equity1)
	
	# create a non existing equity
	testData$noExists <- positionFactory(repository$noExists)
	
	# create a valid bond
	testData$bond1 <- positionFactory(repository$bond1)
	
	# create a valid bond
	testData$bond2 <- positionFactory(repository$bond2)
	
	# create a valid bond
	testData$bond3 <- positionFactory(repository$bond3)
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	testData$proRataFondiObbligazionari <- positionFactory(repository$proRataFondiObbligazionari)
	
	# create a valid AccruedInterest
	testData$proRata1 <- positionFactory(repository$proRata1)
	
	# create an Unclassified security
	testData$unclassified1 <- positionFactory(repository$unclassified1)
	
	return(testData)
}
