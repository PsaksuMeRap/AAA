# TODO: Add comment
# 
# Author: claudio
###############################################################################

allocateTestRepositories <- function(repoName) {
	if (repoName=="DBEquities") {
		# create the data 
		source("./base/unitTests/utilities/createDBEquitiesDataFrame.R")
		DBEquities.df <- createDBEquitiesDataFrame()
		
		# create the instrument repository
		DBEquities <- create_DBEquities(DBEquities.df)
		
		# create backup if necessary
		if (exists("DBEquities",envir=repositories,inherits=FALSE)) {
			eval(expression(DBEquities_back <- DBEquities),envir=repositories)
		}
		
		# assign the repository	
		assign("DBEquities",DBEquities,envir=repositories)
	}
	
	if (repoName=="instruments") {
		# create the data 
		source("./base/unitTests/utilities/createInstrumentsDataFrame.R")
		instruments.df <- createInstrumentsDataFrame()
		
		# create the instrument repository
		instruments <- create_repositoryInstruments(instruments.df)
		
		# create backup if necessary
		if (exists("instruments",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments_back <- instruments),envir=repositories)
		}
		
		# assign the repository	
		assign("instruments",instruments,envir=repositories)
	}

	if (repoName=="exchangeRates") {
		
		# create the data for the instrument repository
		source("./base/unitTests/utilities/createExchangeRatesVector.R")
		rates <- createExchangeRatesVector()
		
		# create the instrument repository		
		exchangeRates <- create_testRepositoryExchangeRates(rates)
		
		# create backup if necessary
		if (exists("exchangeRates",envir=repositories,inherits=FALSE)) {
			eval(expression(exchangeRates_back <- exchangeRates),envir=repositories)
		}
	
		# assign the repository	
		assign("exchangeRates",exchangeRates,envir=repositories)
	}
	
	if (repoName=="politicaInvestimento") {
		
		# create the data for the instrument repository
		source("./base/unitTests/utilities/createPoliticaInvestimentoDataFrame.R")
		politicaInvestimento.df <- createPoliticaInvestimentoDataFrame()
		
		# create the instrument repository		
		politicaInvestimento <- create_repositoryPoliticaInvestimento(politicaInvestimento.df=politicaInvestimento.df) 
		
		# create backup if necessary
		if (exists("politicaInvestimento",envir=repositories,inherits=FALSE)) {
			eval(expression(politicaInvestimento_back <- politicaInvestimento),envir=repositories)
		}
		
		# assegna i repositories
		assign("politicaInvestimento",politicaInvestimento,envir=repositories)
	}

}


deallocateTestRepositories <- function(repoName) {
	if (repoName=="DBEquities") {
		if (exists("DBEquities_back",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments <- DBEquities_back),envir=repositories)
			rm("DBEquities_back",envir=repositories)
		} else {
			if (exists("DBEquities",envir=repositories,inherits=FALSE)) {
				rm("DBEquities",envir=repositories)				
			}
			
		}
	}
	
	
	if (repoName=="instruments") {
		if (exists("instruments_back",envir=repositories,inherits=FALSE)) {
			eval(expression(instruments <- instruments_back),envir=repositories)
			rm("instruments_back",envir=repositories)
		} else {
			if (exists("instruments",envir=repositories,inherits=FALSE)) {
				rm("instruments",envir=repositories)				
			}
			
		}
	}
	
	if (repoName=="exchangeRates") {
		if (exists("exchangeRates_back",envir=repositories,inherits=FALSE)) {
			eval(expression(exchangeRates <- exchangeRates_back),envir=repositories)
			rm("exchangeRates_back",envir=repositories)
		} else {
			if (exists("exchangeRates",envir=repositories,inherits=FALSE)) {
				rm("exchangeRates",envir=repositories)				
			}
			
		}
	}
	
	if (repoName=="politicaInvestimento") {
		if (exists("politicaInvestimento_back",envir=repositories,inherits=FALSE)) {
			eval(expression(politicaInvestimento <- politicaInvestimento_back),envir=repositories)
			rm("politicaInvestimento_back",envir=repositories)
		} else {
			if (exists("politicaInvestimento",envir=repositories,inherits=FALSE)) {
				rm("politicaInvestimento",envir=repositories)		
			}
			
		}
	}
	
}
