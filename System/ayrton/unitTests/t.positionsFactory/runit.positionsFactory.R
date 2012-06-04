# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateEmpyPositions <- function() {
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")
	
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
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
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
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
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

test.shouldAdjustForAccruedInterest <- function() {
	# this test should verify the correct account of the
	# accrued interest for bond, Anticipi_fissi and 
	# depositi_a_termine
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
	# create the instrument repository	
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")
	
	# create the origins
	repository <- createRepositoryAyrtonPositions()
	origin1 <- repository$proRata1
	origin2 <- repository$bond4
	origin3 <- repository$unclassified1
	origin4 <- repository$Deposito_a_termine_acc1
	origin5 <- repository$Deposito_a_termine
	origin6 <- repository$Anticipi_fissiAccrual1
	origin7 <- repository$Anticipi_fissi1
	origins <- new("AyrtonPositions",list(origin1,origin2,origin3,origin4,origin5,origin6,origin7))
	
	# create positions
	positions <- positionsFactory(origins)
	
	# test for bond
	shouldValue <- toMoney(origin2@ValoreMercatoMonetaCHF+origin1@ValoreMercatoMonetaCHF,"CHF")
	checkEquals(positions[[1]]@value,shouldValue)

	# test for Anticipi_fissi
	shouldValue <- toMoney(origin2@ValoreMercatoMonetaCHF+origin1@ValoreMercatoMonetaCHF,"CHF")
	checkEquals(positions[[1]]@value,shouldValue)

	# restore initial conditions	
	deallocateTestRepositories("instruments")
	
}

test.shouldAdjustForAccruedInterestFondiObbligazionariOC <- function() {
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")	
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R")
	
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