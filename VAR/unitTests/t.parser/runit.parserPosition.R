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
	should <- create_criteriumSelection(factor="instrument",values="bond")
	
	checkEquals(result,should)
	
}

test.shouldParseSelCritWithoutTypeAndValue <- function() {
	
	parser <- create_parserSelectionCriteria()
	
	criteriumString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	
	result <- parser$splitFactorsAndValuesFromTypeAndValue(criteriumString)
	
	checkEquals(result[["classesAndFactors"]],"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(result[["valuesAndType"]],"")
	
	result <- parser$splitUnionOfFactorsAndValuesBlocks(result[["classesAndFactors"]])
	checkEquals(result[1],"instrument:bond & currency:JPY")
	
}

test.shouldSplitFactorsFromValues <- function() {
	parser <- create_parserSelectionCriteria()
	
	# check a non "amount" type factor
	string <- "instrument:bond,equity,pippo 233"
	
	
	should <- create_criteriumSelection(
			factor="instrument",
			values=c("bond","equity","pippo 233")
	)
	
	
	result <- parser$splitFactorsFromValues(string)
	checkEquals(result,should)
	
	
	# check an absolute criterium first
	string <- "amount:<= 10.4           EUR  "

	criteriumCheck <- create_criteriumCheck(
			operator="<=",
			value=toMoney(10.4,"EUR"),
			kind="absolute")
	
	should <- create_criteriumSelection(
			factor="amount",
			values="<= 10.4           EUR",
			criteriumCheck=criteriumCheck
	)
	
	result <- parser$splitFactorsFromValues(string)
	checkEquals(result,should)
	
	
	# check a relative criterium
	string <- "amount:= 10.4  %  "
	
	criteriumCheck <- create_criteriumCheck(
			operator="=",
			value=10.4,
			kind="relative")
	
	should <- create_criteriumSelection(
			factor="amount",
			values="= 10.4  %",
			criteriumCheck=criteriumCheck
	)
	
	result <- parser$splitFactorsFromValues(string)
	checkEquals(result,should)
}



test.shouldConstructCriteriumCheck <- function() {

	# test an absolute criterium
	parser <- create_parserSelectionCriteria()
	string = " <=  100.34 EUR "

	result <- parser$constructCriteriumCheck(string)
	should <- create_criteriumCheck(operator="<=",
			value=toMoney(100.34,"EUR"),kind="absolute")
	
	checkEquals(result,should)
	
	# test a relative criterium
	string = " <  100.34 % "
	
	result <- parser$constructCriteriumCheck(string)
	should <- create_criteriumCheck(operator="<",
			value=100.34,kind="relative")
	
	checkEquals(result,should)
	
}

