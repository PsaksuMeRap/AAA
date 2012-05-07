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
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origins
	repository <- createRepositoryAyrtonPositions()
	origin1 <- repository$equity1
	origin2 <- repository$bond1
	origin3 <- repository$unclassified1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3))
	
	# create positions
	positions <- positionsFactory(positions=origins)
	
	checkEquals(length(positions),3)
	checkEquals(positions[[1]]@quantity,15)
	money <- repositories$exchangeRates$exchange(toMoney(124345.632268,"CHF"),new("Currency","EUR"))
	checkEquals(positions[[2]]@value,money)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
}

test.shouldRemoveAccruedInterest <- function() {
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	
	# create the origins
	repository <- createRepositoryAyrtonPositions()
	origin1 <- repository$proRataFondiObbligazionari
	origin2 <- repository$equity1
	origin3 <- repository$unclassified1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3))
	
	# create positions
	positions <- positionsFactory(origins)
	
	checkEquals(length(positions),2)
	checkEquals(is(positions[[1]]@security,"Equity"),TRUE)
	checkEquals(positions[[1]]@quantity,15)
	checkEquals(is(positions[[2]]@security,"Unclassified"),TRUE)
	checkEquals(positions[[2]]@value,toMoney(new("Amount",123.55),new("Currency","CHF")))
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
	
}

test.shouldAdjustForAccruedInterestBond <- function() {
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	# create the origins
	repository <- createRepositoryAyrtonPositions()
	origin1 <- repository$proRata1
	origin2 <- repository$bond4
	origin3 <- repository$unclassified1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3))
	
	# create positions
	positions <- positionsFactory(origins)
	shouldValue <- toMoney(origin2@ValoreMercatoMonetaCHF+origin1@ValoreMercatoMonetaCHF,"CHF")
	checkEquals(positions[[1]]@value,shouldValue)

	# restore initial conditions	
	deallocateTestRepositories("instruments")
	
}

test.shouldAdjustForAccruedInterestFondiObbligazionariOC <- function() {
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")	
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	# create the origins
	repository <- createRepositoryAyrtonPositions()
	origin1 <- repository$proRataFondiObbligazionari
	origin2 <- repository$fondiObbligazionari
	origins <- new("AyrtonPositions",list(origin1,origin2))
	
	# create positions
	positions <- positionsFactory(origins)
	shouldValue <- toMoney(origin2@ValoreMercatoMonetaCHF+origin1@ValoreMercatoMonetaCHF,"CHF")
	shouldValue <- repositories$exchangeRates$exchange(shouldValue,new("Currency","EUR"))
	checkEquals(positions[[1]]@value,shouldValue)
	
	# restore initial conditions	
	deallocateTestRepositories("instruments")
	
}