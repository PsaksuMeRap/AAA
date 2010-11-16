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
	source("./unitTests/utilities/createOriginData.R")
	origin <- createOriginData()
	
	allocateTestRepositories("exchangeRates")
	equity <- parser$parse(origin[[100]])
	deallocateTestRepositories("exchangeRates")	
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"ROG.S")
	
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
	
	# create origin data list
	source("./unitTests/utilities/createOriginData.R")
	origin <- createOriginData()

	allocateTestRepositories("exchangeRates")
	bond <- parser$parse(origin[[300]])
	checkEquals(is.element("bond",class(bond)),TRUE)

	
    # restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
	deallocateTestRepositories("exchangeRates")		
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
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	source("./unitTests/utilities/createOriginData.R")
	origin <- createOriginData()
	

	
	bond <- parser$parse(origin[[900]])
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	checkEquals(is.element("accruedInterest",class(bond)),TRUE)
	
	# restore initial conditions
	assign("instruments",instrumentsRepository,
			envir=repositories)
	deallocateTestRepositories("exchangeRates")	
}


test.shouldParserSplitFactorsFromValues <- function() {
	
	parser <- create_parserSelectionCriteria()
	
	result <- parser$splitFactorString("instrument:bond")
	should <- create_criteriumSelection(factor="instrument",values="bond")
	
	checkEquals(result,should)
	
	string = "amount:<=30 %  "
	criteriumCheck <- parser$constructCriteriumCheck("<=30 %")
	should <- criteriumSelection <- create_criteriumSelection(
			factor="amount",values="<=30 %",criteriumCheck=criteriumCheck)
	result <- parser$splitFactorString(string)
	checkEquals(result,should)		
}


test.shouldParseCheckString <- function() {
	
	parser <- create_parserSelectionCriteria()
		
	checkString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			"; =0USD")
	
	result <- parser$splitCheckString(checkString)
	
	checkEquals(result[["selectionString"]],"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	
	result2 <- parser$constructCriteriumCheck(" =0    USD ")
	checkEquals(result[["criteriumCheck"]],result2)

}


test.shouldParseCheckStringWithoutConstraintString <- function() {
	
	parser <- create_parserSelectionCriteria()
	
	checkString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	
	result <- parser$splitCheckString(checkString)
	
	checkEquals(result[["selectionString"]],"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(result[["criteriumCheck"]],NA)
	
}


test.shouldConstructCriteriumSelectionList <- function() {
	parser <- create_parserSelectionCriteria()
	
	result <- parser$splitFactorsString("instrument:bond")
	should <- list( create_criteriumSelection(factor="instrument",values="bond") )
	
	checkEquals(result,should)
	
	string = "instrument:bond & currency:JPY & amount:<=30 %"
	result <- parser$splitFactorsString(string)
	
	should <- list()
	should[[1]] <- create_criteriumSelection(factor="instrument",values="bond")
	should[[2]] <- create_criteriumSelection(factor="currency",values="JPY")
	criteriumCheck <- parser$constructCriteriumCheck("<=30 %")
	should[[3]] <- criteriumSelection <- create_criteriumSelection(
			factor="amount",values="<=30 %",criteriumCheck=criteriumCheck)
	
	checkEquals(result,should)	

}


test.shouldConstructUnionsOfFactorsAndValuesBlocks <- function() {
	parser <- create_parserSelectionCriteria()
	string = "instrument:bond,equity"
	
	result <- parser$splitSelectionString(string)
	should <- create_criteriumSelection(factor="instrument",values=c("bond","equity"))
	should <- list(should)
	should <- list(should)
	
	checkEquals(result,should)
	
	string = "instrument:bond,equity & currency:USD"
	
	result <- parser$splitSelectionString(string)
	should1.1 <- create_criteriumSelection(factor="instrument",values=c("bond","equity"))
	should1.2 <- create_criteriumSelection(factor="currency",values="USD")
	should <- list(should1.1,should1.2)
	should <- list(should)
	
	checkEquals(result,should)
	
	string = "instrument:bond,equity & currency:USD + instrument:money market"
	
	result <- parser$splitSelectionString(string)
	should1.1 <- create_criteriumSelection(factor="instrument",values=c("bond","equity"))
	should1.2 <- create_criteriumSelection(factor="currency",values="USD")
	should2.1 <- create_criteriumSelection(factor="instrument",values="money market")
	should1 <- list(should1.1,should1.2)
	should2 <- list(should2.1)
	should <- list(should1,should2)
	
	checkEquals(result,should)	
	
}


test.shouldSplitFactorsFromValues <- function() {
	parser <- create_parserSelectionCriteria()
	
	# check a non "amount" type factor
	string <- "instrument:bond,equity,pippo 233"
	
	
	should <- create_criteriumSelection(
			factor="instrument",
			values=c("bond","equity","pippo 233")
	)
	
	
	result <- parser$splitFactorString(string)
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
	
	result <- parser$splitFactorString(string)
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
	
	result <- parser$splitFactorString(string)
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

