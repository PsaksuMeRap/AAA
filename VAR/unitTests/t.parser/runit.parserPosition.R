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

test.shouldParseSelectionCriteria <- function() {
	
	parser <- create_parserSelectionCriteria()
		
	criteriumString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			"; =0USD")
	
	result <- parser$splitFactorsAndValuesFromTypeAndValue(criteriumString)
	
	checkEquals(result[["classesAndFactors"]],"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(result[["valuesAndType"]],"=0USD")
	
	result <- parser$splitUnionOfFactorsAndValuesBlocks(result[["classesAndFactors"]])
	checkEquals(result[1],"instrument:bond & currency:JPY")
	
	result <- parser$splitFactorsAndValuesBlocks("instrument:bond & currency:JPY")
	checkEquals(result[1],"instrument:bond")

	result <- parser$splitFactorsFromValues("instrument:bond")
	checkEquals(result[1],"instrument")
	checkEquals(result[2],"bond")

}

identifyQuantitativeConstraint <- function(string) {
	# a string with the quantitative constraint to identify
	# "=0USD" or "> 1.77 EUR" are examples
	
	string <- removeStartEndSpaces(string)
	string <-"< 12.44 aaaB5B"

	# check for operator
	checkResult <- regexpr("^[>=,<=,<,>,=,!=]", string)
	start = checkResult[[1]]
	stop = start + attributes(checkResult)$match.length-1
	if (start==-1) stop("Error: missing equality/inequality")
	operator <- substr(string,start,stop)
	string <- substr(string,stop+1,nchar(string))
	string <- removeStartEndSpaces(string)
	
	
	# check for currency
	checkResult <- regexpr("[A-Z]{3}$", string)
	start = checkResult[[1]]
	stop = start + attributes(checkResult)$match.length-1
	if (start>-1) {
		constraintType = "absolute"
		currency <- substr(string,start,stop)
		string <- substr(string,1,start-1)		
	} else {
		# check for %
		checkResult <- regexpr("%$", string)
		start = checkResult[[1]]
		stop = start + attributes(checkResult)$match.length-1
		if (start>-1) {
			constraintType = "relative"
			string <- substr(string,1,start-1)
		} else {
			stop("Error: missing currency or % identifier")
		}
	}
	amount <- as.numeric(string)
}