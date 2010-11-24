# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldParseEquityPosition <- function() {
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the equity repository and instrument repository
	allocateTestRepositories("equities")	
	allocateTestRepositories("instruments")	

	# create the positions data.frame
	# source("./unitTests/utilities/createOriginData.R")
	# origin <- createOriginData()
	origin <- list()
	origin$Cliente <- "pippo160"
	origin$Strumento <- "A"
	origin$Moneta <- "CHF"
	origin$Nome <- "Roche Holding Gs"
	origin$ValoreMercatoMonetaCHF <- 88205
	origin$ID_AAA <- 824
	origin$ID_strumento <- 1

	source("./unitTests/utilities/allocateTestRepositories.R")
	allocateTestRepositories("exchangeRates")
	
	equity <- parser$parse(origin)
	deallocateTestRepositories("exchangeRates")	
	
	checkEquals(is.element("equity",class(equity)),TRUE)
	checkEquals(equity$ticker,"ROG.S")
	
	# restore initial conditions
	deallocateTestRepositories("equities")	
	deallocateTestRepositories("instruments")		

}


test.shouldParseBondPosition <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument and exchange rates repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	origin <- list()
	origin$Cliente <- "pippo61"	
	origin$Strumento <- "O      "
	origin$Moneta <- "EUR"
	origin$Nome <- "20201231 - 0% CB-Accent Lux Sicav - Fixed Income EUR 31-12-20"
	origin$ValoreMercatoMonetaCHF <- 306595.4
	origin$ID_AAA <- 825
	origin$ID_strumento <- 2
	

	bond <- parser$parse(origin)
	checkEquals(is.element("bond",class(bond)),TRUE)

	
    # restore initial conditions
	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")		
}

test.shouldParseStrutturatoFixedIncome <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# create the data for the instrument and exchange rates repositories
	allocateTestRepositories("instruments")
	allocateTestRepositories("exchangeRates")	
	
	origin <- list()
	origin$Cliente <- "pippo136"
	origin$Strumento <- "PS"
	origin$Moneta <- "EUR"
	origin$Nome <- "20130521 - <3Y - Floored Floares with Cap 1.75%-4.625% p.a. On CS"
	origin$ValoreMercatoMonetaCHF <- 399892.3
	origin$ID_AAA <- 98
	origin$ID_strumento <- 49
	
	ps <- parser$parse(origin)
	checkEquals(is.element("Strutturati FI",class(ps)),TRUE)
	
	# restore initial conditions
	deallocateTestRepositories("instruments")	
	deallocateTestRepositories("exchangeRates")		
	
}

test.shouldIdenfyAccruedInterest <- function() {
	source("./unitTests/utilities/allocateTestRepositories.R")
	
	# create the parser
	source("./lib/parser.R")
	parser <- create_parserPosition()
	
	# allocate the repositories instruments and exchangeRates
	allocateTestRepositories("instruments")	
	allocateTestRepositories("exchangeRates")	
	
	# create origin data list
	origin <- list()
	origin$Cliente <- "pippo185"
	origin$Strumento <- "Oacc"
	origin$Moneta <- "CHF"
	origin$Nome <- "20110527 - 3.25% IBM 27-05-11 Pro-rata"
	origin$ValoreMercatoMonetaCHF <- 368.3333
	origin$ID_AAA <- 1172
	origin$ID_strumento <- 2
	
	bond <- parser$parse(origin)
	
	checkEquals(is.element("bond",class(bond)),TRUE)
	checkEquals(is.element("accruedInterest",class(bond)),TRUE)
	
	# restore initial conditions

	deallocateTestRepositories("instruments")	
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

