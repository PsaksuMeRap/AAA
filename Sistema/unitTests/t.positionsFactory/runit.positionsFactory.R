# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateEmpyPositions <- function() {
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origins
	origins <- new("AyrtonPositions",list())
	
	# create positions
	positions <- positionsFactory(positions=origins)
	
	checkEquals(length(positions),0)
	checkEquals(is(positions,"Positions"),TRUE)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}


test.shouldCreateValidPositions <- function() {
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origins
	repository <- createRepositoryForOriginTestData()
	origin1 <- repository$equity1
	origin2 <- repository$bond1
	origin3 <- repository$unclassified1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3))
	
	# create positions
	positions <- positionsFactory(positions=origins)
	
	checkEquals(length(positions),3)
	checkEquals(positions[[1]]@quantity,15)
	checkEquals(positions[[2]]@value,toMoney(124345.632268,new("Currency","EUR")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldRemoveAccruedInterest <- function() {
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryForOriginTestData.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origins
	repository <- createRepositoryForOriginTestData()
	origin1 <- repository$proRataFondiObbligazionari
	origin2 <- repository$bond1
	origin3 <- repository$unclassified1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3))
	
	# create positions
	positions <- positionsFactory(origins)
	
	checkEquals(length(positions),2)
	checkEquals(is(positions[[1]]@security,"Bond"),TRUE)
	checkEquals(positions[[1]]@quantity,100000)
	checkEquals(is(positions[[2]]@security,"Unclassified"),TRUE)
	checkEquals(positions[[2]]@value,toMoney(123.55,new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
	
}