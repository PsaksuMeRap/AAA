# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldJoinTwoPositions <- function() {
	# The class Positions is a container class (a list)
	# where the content is not checked for consistency
	
	# test1: two non empty positions
	e1 <- new("Positions",list("a","B",1:10))
	e2 <- new("Positions",list("Claudio",12.3))
	
	e3 <- join(e1,e2)
	
	checkEquals(e3,new("Positions",list("a","B",1:10,"Claudio",12.3)))
	
	# test2: two empty positions
	e1 <- new("Positions",list())
	e2 <- new("Positions",list())
	
	e3 <- join(e1,e2)
	
	checkEquals(e3,new("Positions",list()))
}



test.shouldExtractPositions <- function() {
	
	source("./unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	Positions <- new("Positions",positions)
	
	# test1: with a numeric index
	result <- Positions[c(1,3)]
	checkEquals(is(result,"Positions"),TRUE)
	checkEquals(result,new("Positions",positions[-2]))

	# test1: with a logical index
	result <- Positions[c(TRUE,FALSE,TRUE)]
	checkEquals(is(result,"Positions"),TRUE)
	checkEquals(result,new("Positions",positions[-2]))
}

test.shouldSumPositions <- function() {
	
	source("./unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	# equity1 CHF: 88205 CHF, equity2 EUR: 7439.7503136 CHF, bond1 EUR: 124345.632268 CHF,
	positions <- new("Positions",positions)
	
	# test
	result <- sum(positions)
	should <- toMoney(88205 + 7439.7503136 + 124345.632268,"CHF")
	checkEquals(result,should)
}

test.shouldReweightPositions <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1 # 88'205 chf 
	p2 <- repo$bond1 # 92896.6 eur 
	positions <- new("Positions",list(p1,p2))
	
	p <- reweight(positions,0.4)
	
	# test 1
	checkEquals(p[[1]]@value,toMoney(88205*0.4,"CHF"))
	checkEquals(p[[1]]@quantity,positions[[1]]@quantity*0.4)
	checkEquals(p[[1]]@security@name,positions[[1]]@security@name)
	
	# test 2
	p2New <- reweight(positions,0.4)
	checkEquals(p[[2]]@value,toMoney(positions[[2]]@value@amount*0.4,"EUR"))
	checkEquals(p[[2]]@quantity,positions[[2]]@quantity*0.4)
	checkEquals(p[[2]]@accruedInterest,positions[[2]]@accruedInterest*0.4)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}

test.shouldReplacePositions <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1 
	p3 <- repo$equity2
	positions <- new("Positions",list(p1,p2))
	
	# test 1
	positions[2] <- p3 
	checkEquals(positions[[2]],p3)

	# test 2
	positions <- new("Positions",list(p1,p2,p3))
	positions[c(1,3)] <- list(p3,p1) 
	checkEquals(positions[[1]],p3)
	checkEquals(positions[[2]],p2)
	checkEquals(positions[[3]],p1)
	
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}