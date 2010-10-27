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
			eval(expression(equities_bak <- equities),envir=repositories)
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
			eval(expression(instruments_bak <- instruments),envir=repositories)
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
			eval(expression(interestRates_bak <- interestRates),envir=repositories)
		}
		
		# assegna i repositories
		assign("interestRates",interestRates,envir=repositories)
	}
	
}


deallocateTestRepositories <- function(repoName) {
	if (repoName=="equities") {
		if (exists("equities_bak",envir=repositories,inherits=FALSE)) {
			eval(expression(equities <- equities_bak),envir=repositories)
			rm("equities_bak",envir=repositories)
		} else {
			rm("equities",envir=repositories)
		}
	}
	
	if (repoName=="instruments") {
		if (exists("instruments_bak",envir=repositories,inherits=FALSE)) {
				eval(expression(instruments <- instruments_bak),envir=repositories)
				rm("instruments_bak",envir=repositories)
			} else {
				rm("instruments",envir=repositories)
			}
	}

	if (repoName=="exchangeRates") {
		if (exists("exchangeRates_bak",envir=repositories,inherits=FALSE)) {
			eval(expression(exchangeRates <- exchangeRates_bak),envir=repositories)
			rm("exchangeRates_bak",envir=repositories)
		} else {
			rm("exchangeRates",envir=repositories)
		}
	}
	
	if (repoName=="interestRates") {
		if (exists("interestRates_bak",envir=repositories,inherits=FALSE)) {
			eval(expression(interestRates <- interestRates_bak),envir=repositories)
			rm("interestRates_bak",envir=repositories)
		} else {
			rm("interestRates",envir=repositories)
		}
	}
	
}
