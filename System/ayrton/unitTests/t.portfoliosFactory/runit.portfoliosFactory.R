test.shouldCreateDefaultPortfolio <- function() {
	
	source("./base/unitTests/utilities/createPoliticaInvestimentoDataFrame.R")
	politicaInvestimento.df <-	createPoliticaInvestimentoDataFrame()
	
	# uses a default method
	source("./base/unitTests/utilities/allocateTestRepositories.R")  
	source("./base/unitTests/utilities/createRepositoryAyrtonPositions.R") 
	
	# create the instrument repository  
	allocateTestRepositories("instruments")
	allocateTestRepositories("politicaInvestimento")
	
	# create the origin
	repository <- createRepositoryAyrtonPositions()
	ayrtonPositions <- list(repository$unclassified1,
			repository$equity1,
			repository$equity2,
			repository$bond1,
			repository$bond2)
	ayrtonPositions <- new("AyrtonPositions",ayrtonPositions)
	politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df
	portfolios <- portfoliosFactory(ayrtonPositions,politicaInvestimento.df)
	
	checkEquals(length(portfolios),4)
	checkEquals(length(portfolios[[4]]),2)	
	checkEquals(portfolios[[4]]@owner,"pippo3")
	
	
}
