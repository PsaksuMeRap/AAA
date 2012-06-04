# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldApplyDirectiveString <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$Fondi_misti # CHF
	p2 <- repo$equity1
	p3 <- repo$bond1
	positions <- new("Positions",list(p1,p2,p3))
	directiveString <- new("DirectiveString","explode:Fondi_misti")
	
	result <- Apply(x=directiveString,positions=positions)
	
	
	checkEquals(is(result[[3]]@security,"Fondi_azionari"),TRUE)
	checkEquals(is(result[[4]]@security,"Fondi_obbligazionari"),TRUE)
	checkEquals(identical(result[[1]],positions[[2]]),TRUE)
	checkEquals(identical(result[[2]],positions[[3]]),TRUE)
	
	if (!is.null(repository)) repositories$exchangeRates <- repository
}


test.shouldFailToApplyDirectiveString <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$Fondi_misti # CHF
	p2 <- repo$equity1
	p3 <- repo$bond1
	positions <- new("Positions",list(p1,p2,p3))
	
	
	# Test 1: directiveString="dadd" -> stop
	directiveString <- new("DirectiveString","dadd")
	checkException(Apply(x=directiveString,positions=positions))
	
	
	# reset the repositories in the original state
	deallocateTestRepositories("exchangeRates")
	deallocateTestRepositories("instruments")
	deallocateTestRepositories("politicaInvestimento")
}


test.shouldapplyOneCheckStringOnPositions <- function() {
	# exchange rates required for position initialization
	# initialize exchange rates
	repository <- repositories$exchangeRates
	source("./base/unitTests/utilities/createExchangeRatesTestRepository.R")
	testRepository <- createExchangeRatesTestRepository() 
	repositories$exchangeRates <- testRepository
	# exchange rate USD-CHF: 0.9627
	# exchange rate EUR-CHF: 1.33853808
	
	# initialize the position
	source("./base/unitTests/utilities/createRepositoryPositions.R")
	repo <- createRepositoryPositions()
	
	p1 <- repo$equity1
	p2 <- repo$bond1
	p3 <- repo$strutturati_FI
	p4 <- repo$equity2
	positions <- new("Positions",list(p1,p2,p3,p4))
	
	
	# Test 1
	checkString <- scan(file="./riskman/unitTests/data/criteriSelezionePerPortafoglioTest.txt",
			what="character",sep="\n",quiet = TRUE)
	
	checkEquals(checkString,"security:Bond & currency:CHF ; > 5%")
	
	# Test 2
	# redefine the checkString
	checkString <- "security:Bond & currency:EUR ; > 30%"
	checkString <- new("CheckString",checkString)
	
	result <- Apply(checkString,positions=positions)
	checkEquals(result$checkResult,TRUE)
	checkEquals(result$percentageValue,"30.00%")
	
	# Test 3
	# use an absolute constraint
	checkString <- "security:Bond & currency:CHF ; >= 3000000CHF"
	checkString <- new("CheckString",checkString)
	
	result <- Apply(checkString,positions=positions)
	checkEquals(result$checkResult,FALSE)
	checkEquals(result$actualPercentage,"0.00%")
	
	# reset the repository in the original state
	repositories$exchangeRates <- repository
}

