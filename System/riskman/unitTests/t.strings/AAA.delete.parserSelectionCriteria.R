# TODO: Add comment
# 
# Author: claudio
###############################################################################


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

test.shouldParserSplitFactorsFromValuesWithNegation <- function() {
	
	parser <- create_parserSelectionCriteria()
	
	result <- parser$splitFactorString("instrument!:bond,equities")
	should <- create_criteriumSelection(factor="instrument",values=c("bond","equities"),negation=TRUE)
	
	checkEquals(result,should)
	
	# negation is disregarded for factors of type amount
	string = "amount!:<=30 %  "
	criteriumCheck <- parser$constructCriteriumCheck("<=30 %")
	should <- criteriumSelection <- create_criteriumSelection(
			factor="amount",values="<=30 %",criteriumCheck=criteriumCheck)
	result <- parser$splitFactorString(string)
	checkEquals(result,should)		
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


