# TODO: Add comment
# 
# Author: claudio
###############################################################################

allocateTestRepositories <- function(repoName) {
	# source("./lib/repository.R")
	
	if (repoName=="equities") {
		# import the data
		source("./unitTests/utilities/createEquityDataFrame.R")
		equities.df <- createEquityDataFrame()	
		
		# create the equity repository
		source("./lib/repository.R")
		equityRepository <- create_repositoryEquities(equities.df)
		
		# create backup if necessary
		if (exists("equities",envir=repositories,inherits=FALSE)) {
			eval(expression(equities_back <- equities),envir=repositories)
		}
		
		# assign the repository		
		assign("equities",equityRepository,envir=repositories)
	}
	
	if (repoName=="instruments") {
		# create the data 
		source("./unitTests/utilities/createInstrumentsDataFrame.R")
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
		source("./unitTests/utilities/createExchangeRatesVector.R")
		rates <- createExchangeRatesVector()
		
		# create the instrument repository		
		exchangeRates <- create_repositoryExchangeRates(rates)
		
		# create backup if necessary
		if (exists("exchangeRates",envir=repositories,inherits=FALSE)) {
			eval(expression(exchangeRates_back <- exchangeRates),envir=repositories)
		}
		
		# assign the repository	
		assign("exchangeRates",exchangeRates,envir=repositories)
	}
	
	if (repoName=="interestRates") {

		# create the data for the instrument repository
		file <- "./unitTests/t.repositories/interestRates.csv"
		interestRates.df = read.csv(file,header=TRUE,stringsAsFactors=FALSE)
		
		# create the instrument repository		
		interestRates <- create_repositoryInterestRates(interestRates.df=interestRates.df) 

		# create backup if necessary
		if (exists("interestRates",envir=repositories,inherits=FALSE)) {
			eval(expression(interestRates_back <- interestRates),envir=repositories)
		}
		
		# assegna i repositories
		assign("interestRates",interestRates,envir=repositories)
	}
	if (repoName=="politicaInvestimento") {
		
		# create the data for the instrument repository
		source("./unitTests/utilities/createPoliticaInvestimentoDataFrame.R")
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
	if (repoName=="fixedIncome") {
		
		# create the data for the instrument repository
		source("./unitTests/utilities/createFixedIncomeDataFrame.R")
		fixedIncome.df <- createFixedIncomeDataFrame()
		
		# create the instrument repository		
		fixedIncome <- create_repositoryFixedIncome(fixedIncome=fixedIncome.df) 
		
		# create backup if necessary
		if (exists("fixedIncome",envir=repositories,inherits=FALSE)) {
			eval(expression(fixedIncome_back <- fixedIncome),envir=repositories)
		}
		
		# assegna i repositories
		assign("fixedIncome",fixedIncome,envir=repositories)
	}
}


deallocateTestRepositories <- function(repoName) {
	if (repoName=="equities") {
		if (exists("equities_back",envir=repositories,inherits=FALSE)) {
			eval(expression(equities <- equities_back),envir=repositories)
			rm("equities_back",envir=repositories)
		} else {
			if (exists("equities",envir=repositories,inherits=FALSE)) {
				rm("equities",envir=repositories)				
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
	
	if (repoName=="interestRates") {
		if (exists("interestRates_back",envir=repositories,inherits=FALSE)) {
			eval(expression(interestRates <- interestRates_back),envir=repositories)
			rm("interestRates_back",envir=repositories)
		} else {
			if (exists("interestRates",envir=repositories,inherits=FALSE)) {
				rm("interestRates",envir=repositories)			
			}
			
		}
	}
	
	if (repoName=="fixedIncome") {
		if (exists("fixedIncome_back",envir=repositories,inherits=FALSE)) {
			eval(expression(fixedIncome <- fixedIncome_back),envir=repositories)
			rm("fixedIncome_back",envir=repositories)
		} else {
			if (exists("fixedIncome",envir=repositories,inherits=FALSE)) {
				rm("fixedIncome",envir=repositories)			
			}
			
		}
	}
}
