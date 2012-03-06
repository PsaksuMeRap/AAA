# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryPositions <- function() {
	
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	
	testData <- new.env()

	# create a valid equity
	testData$equity1 <- positionFactory(repository$equity1)

	# create a valid equity
	testData$equity2 <- positionFactory(repository$equity2)
	
	# create a valid index certificate
	testData$indexCertificate <- positionFactory(repository$indexCertificate)
	
	# create a non existing equity
	testData$noExists <- positionFactory(repository$noExists)
	
	# create a valid bond
	testData$bond1 <- positionFactory(repository$bond1)
	
	# create a valid bond
	testData$bond2 <- positionFactory(repository$bond2)
	
	# create a valid bond
	testData$bond3 <- positionFactory(repository$bond3)
	
	# create a valid bond (match to proRata1)
	testData$bond4 <- positionFactory(repository$bond4)
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	testData$proRataFondiObbligazionari <- positionFactory(repository$proRataFondiObbligazionari)
	
	# create a valid AccruedInterest
	testData$proRata1 <- positionFactory(repository$proRata1)
	
	# create an Unclassified security
	testData$unclassified1 <- positionFactory(repository$unclassified1)
	
	# create a strutturati_FI 
	testData$strutturati_FI <- positionFactory(repository$strutturati_FI)
	
	return(testData)
}
