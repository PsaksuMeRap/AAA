test.shouldCreateDefaultPortfolio <- function() {
	
	source("./unitTests/utilities/createPoliticaInvestimentoDataFrame.R")
	politicaInvestimento.df <-	createPoliticaInvestimentoDataFrame()
	
	# uses a default method
	source("./unitTests/utilities/allocateTestRepositories.R")  
	source("./unitTests/utilities/createRepositoryAyrtonPositions.R") 
	
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
	
	checkEquals(TRUE,FALSE)
	
}
