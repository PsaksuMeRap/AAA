# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldSplitCheckString <- function() {
	
	checkString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			"; =0USD  ::   explode:  Fondo_misto")
	
	checkString <- new("CheckString",checkString)
	
	result <- split(checkString)
	checkEquals(result$selectionString,new("SelectionString","instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF"))
	checkEquals(result$constraintString,new("ConstraintString","=0USD"))
	checkEquals(result$directiveString,new("DirectiveString","explode:  Fondo_misto"))
	
}


test.shouldParseCheckStringWithoutConstraintString <- function() {
	
	
	checkString = paste("instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkString <- new("CheckString",checkString)
	
	result <- split(checkString)
	
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["constraintString"]]),NA_character_)
	
}


test.shouldSplitSelectionString <- function() {
	
	selectionString = paste("instrument:bond & currency:JPY +  instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	
	selectionString <- new("SelectionString",selectionString)

	result <- split(selectionString)
	checkEquals(result[[1]],new("FactorStrings","instrument:bond & currency:JPY"))
	checkEquals(result[[2]],new("FactorStrings","instrument:bond,equity & currency:usd,chf"))
	checkEquals(result[[3]],new("FactorStrings","amount:<=100.3CHF"))
}


test.shouldSplitFactorStrings <- function() {
	
	factorStrings <- "instrument:bond,equity & currency:usd,chf"
	factorStrings <- new("FactorStrings",factorStrings)
		
	result <- split(factorStrings)
	checkEquals(result[[1]],new("FactorString","instrument:bond,equity"))
	checkEquals(result[[2]],new("FactorString","currency:usd,chf"))

}


test.shouldSplitFactorString <- function() {
	
	factorString1 <- "security:bond,equity"
	factorString1 <- new("FactorString",factorString1)
	
	factorString2 <- "currency!:usd,chf"
	factorString2 <- new("FactorString",factorString2)
	
	factorString3 <- "amount:  <=100.3CHF  "
	factorString3 <- new("FactorString",factorString3)
	
	# start tests	
	result1 <- split(factorString1)
	checkEquals(result1@criterium,"security")
	checkEquals(result1@negation,FALSE)
	checkEquals(result1@values,c("bond,equity"))
	
	result2 <- split(factorString2)
	checkEquals(result2@criterium,"currency")
	checkEquals(result2@negation,TRUE)
	checkEquals(result2@values,c("usd,chf"))	
	
	result3 <- split(factorString3)
	checkEquals(result3@criterium,"amount")
	checkEquals(result3@negation,FALSE)
	checkEquals(result3@values,new("ConstraintString","<=100.3CHF"))	
}


