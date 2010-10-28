# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldParseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	# create the equity repository
	source("./lib/repository.R")
	equityRepository_tmp <- create_repositoryEquities(equities.df)
    
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	equityRepository <- repositories$equities
    assign("equities",equityRepository_tmp,envir=repositories)
	instrumentsRepository <- repositories$instruments
    assign("instruments",instrumentsRepository_tmp,
			envir=repositories)

	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")
	dati.df <- createPositionsData()
	
	equity <- parser$parse(dati.df[2,])
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"PHIA.AS")
	
	# restore initial conditions
	assign("equities",equityRepository,envir=repositories)
	assign("instruments",instrumentsRepository,
			envir=repositories)
}


test.shouldParseBondPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	instrumentsRepository <- repositories$instruments
	assign("instruments",instrumentsRepository_tmp,
			envir=repositories)
	
	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")	
	dati.df <- createPositionsData()
	
	bond <- parser$parse(dati.df[700,])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	
    # restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
}



test.shouldIdenfyAccruedInterest <- function() {
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument repository
	source("./unitTests/utilities/createInstrumentsDataFrame.R")
	instruments.df <- createInstrumentsDataFrame()
	
	# create the instrument repository
	instrumentsRepository_tmp <- create_repositoryInstruments(instruments.df)
	
	# assegna i repositories
	instrumentsRepository <- repositories$instruments
	assign("instruments",instrumentsRepository_tmp,
			envir=repositories)
	
	
	# create the positions data.frame
	source("./unitTests/utilities/createPositionsData.R")	
	dati.df <- createPositionsData()
	
	bond <- parser$parse(dati.df[493,])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	checkEquals(is.element("accruedInterest",class(bond)),TRUE)
	
	# restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
	
}

test.shouldParseSelectionCriterium <- function() {
	
	splitClassesAndFactorsFromValueType <- function(criteriumString) {
		
		result <- unlist(strsplit(criteriumString,";"))
		if (length(result)==0) stop("Error: empty criteriumString")
		
		if (length(result)==2) names(result) <- c("classesAndFactors","valuesAndType")
		if (length(result)==1) names(result) <- "classesAndFactors"
		return(result)
	}
	
	splitUnionOfClassesAndFactorsBlocks <- function(string) {
		result <- unlist(strsplit(string,"\\+"))
		return(result)
	}
	
	splitClassesAndFactorsBlocks <- function(string) {
		result <- unlist(strsplit(string,"\\&"))
		return(result)
	}

	splitClassesFromFactors <- function(string) {
		result <- unlist(strsplit(string,":"))
		return(result)
	}
	
	criteriumString = paste("instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF ; =0USD")
	
	result <- splitClassesAndFactorsFromValueType(criteriumString)
	
	checkEquals(result[["classesAndFactors"]],"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF ")
	checkEquals(result[["valuesAndType"]]," =0USD")
	
	result <- splitUnionOfClassesAndFactorsBlocks(result[[1]])
	checkEquals(result[1],"instrument:bond & currency:JPY ")
	
	result <- splitClassesAndFactorsBlocks("instrument:bond & currency:JPY ")
	checkEquals(result[1],"instrument:bond ")

	result <- splitClassesFromFactors("instrument:bond ")
	checkEquals(result[1],"instrument")
	checkEquals(result[2],"bond ")
	
}