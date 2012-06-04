# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldMatchIdAAA_string <- function() {
	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	repositoryAyrtonPositions <- createRepositoryAyrtonPositions()
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# test1: a correct match 
	# create the origin
	opzioni_su_azioni1AyrtonPosition <- repositoryAyrtonPositions$Opzioni_su_azioni1

	# create the corresponding position
	opzioni_su_azioni1 <- repository$Opzioni_su_azioni1
	
	checkEquals(identical(opzioni_su_azioni1@security@id@idAAA,opzioni_su_azioni1AyrtonPosition),TRUE)
	
	# test2: an incorrect match between two "IdAAA_string"
	# create the origin
	opzioni_su_azioni1AyrtonPosition <- repositoryAyrtonPositions$Opzioni_su_azioni1
	
	# create a non corresponding position
	opzioni_su_azioni2 <- repository$Opzioni_su_azioni2
	
	checkEquals(identical(opzioni_su_azioni2@security@id@idAAA,opzioni_su_azioni1AyrtonPosition),FALSE)
	
	# test3: an incorrect match 
	# create the origin
	equity2AyrtonPosition <- repositoryAyrtonPositions$equity2
	
	# create a non corresponding position
	opzioni_su_azioni2 <- repository$Opzioni_su_azioni2
	
	checkEquals(identical(opzioni_su_azioni2@security@id@idAAA,opzioni_su_azioni1AyrtonPosition),FALSE)
	
}

test.shouldMatchIdAAA_numeric <- function() {
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	repositoryAyrtonPositions <- createRepositoryAyrtonPositions()
	
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# test1
	# create the origin
	equity1AyrtonPosition <- repositoryAyrtonPositions$equity1
	
	# create the corresponding position
	equity1 <- repository$equity1
	
	checkEquals(identical(equity1@security@id@idAAA,equity1AyrtonPosition),TRUE)
	
	# test2: an incorrect match between two "IdAAA_numeric"
	# create the origin
	equity2AyrtonPosition <- repositoryAyrtonPositions$equity2
	
	# create a non corresponding position
	equity1 <- repository$equity1
	
	checkEquals(identical(equity1@security@id@idAAA,equity2AyrtonPosition),FALSE)
	
	# test3: an incorrect match 
	# create the origin
	opzioni_su_azioni1AyrtonPosition <- repositoryAyrtonPositions$Opzioni_su_azioni1
	
	# create a non corresponding position
	equity1 <- repository$equity1
	
	checkEquals(identical(equity1@security@id@idAAA,opzioni_su_azioni1AyrtonPosition),FALSE)
	
}