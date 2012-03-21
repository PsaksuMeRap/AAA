# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldReturnFieldsToPrint <- function() {
	# uses a default method
	source("./unitTests/utilities/createRepositoryPositions.R")
	repoPosition <- createRepositoryPositions()
	
	unclassified <- repoPosition$unclassified1
	result <- fieldsToPrint(unclassified,list(empty=TRUE))
	should <- list(securityClassName="Unclassified",
			currency="CHF",
			amount="123.55",
			name="Security not classified")
	
	checkEquals(result,should)
}

test.shouldTransformAs.character <- function() {
	
	# uses a default method
	source("./unitTests/utilities/createRepositoryPositions.R")
	repoPosition <- createRepositoryPositions()
	
	unclassified <- repoPosition$unclassified1
	result <- as.character(unclassified)
	
	checkEquals(result,"Unclassified / CHF / 123.55 / Security not classified")
	
	# now check with some width requirements
	width=list(securityClassName=20,amount=12)
	result <- as.character(unclassified,width)
	checkEquals(result,"Unclassified         / CHF /       123.55 / Security not classified")
	
}


test.shouldDivedeByAMoney <- function() {
	
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
	
	p1 <- repo$equity1 # 88'205 chf / chf
	p2 <- repo$bond1 # 124'345.632268 chf / 92'896.5967 eur
	
	# test 1	
	money <- toMoney(100,"CHF")
	result <- p1 / money
	checkEquals(result,882.05)
	
	# test 2	
	money <- toMoney(0,"CHF")
	result <- p1 / money
	checkEquals(result,Inf)
	
	# test 3	
	money <- toMoney(1,"EUR")
	result <- p2 / money
	checkEquals(result,124345.632268/1.33853808)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}

test.shouldReweight <- function() {
	
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
	
	# test 1
	p1New <- reweight(p1,0.4)
	checkEquals(p1New@value,toMoney(88205*0.4,"CHF"))
	checkEquals(p1New@quantity,p1@quantity*0.4)
	checkEquals(p1New@security@name,p1@security@name)
	
	# test 2
	p2New <- reweight(p2,0.4)
	checkEquals(p2New@value,toMoney(p2@value@amount*0.4,"EUR"))
	checkEquals(p2New@quantity,p2@quantity*0.4)
	checkEquals(p2New@accruedInterest,p2@accruedInterest*0.4)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}