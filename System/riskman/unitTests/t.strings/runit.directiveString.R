# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldIdentifyDirective <- function() {
	
	## test 1: all 3 strings
	checkString = paste(" instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			"; =0  USD     ::explode:Fondi_misti ")
	checkString <- new("CheckString",checkString)
	
	result <- split(checkString)
	checkEquals(unclass(result[["constraintString"]]),"=0  USD")
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["directiveString"]]),"explode:Fondi_misti")
	
	
	## test 2: selectionString con directiveString, no constraintString
	checkString = paste(" instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			" ::   explode:Fondi_misti ")
	checkString <- new("CheckString",checkString)
	result <- split(checkString)
	
	checkEquals(unclass(result[["constraintString"]]),NA_character_)
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["directiveString"]]),"explode:Fondi_misti")
	
	## test 3: selectionString con constraintString
	checkString = paste(" instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF",
			"; =0  USD")
	checkString <- new("CheckString",checkString)
	result <- split(checkString)
	
	checkEquals(unclass(result[["constraintString"]]),"=0  USD")
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["directiveString"]]),NA_character_)
	
	## test 4: solo selectionString
	checkString = paste(" instrument:bond & currency:JPY +",
			"instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkString <- new("CheckString",checkString)
	result <- split(checkString)
	
	checkEquals(unclass(result[["constraintString"]]),NA_character_)
	checkEquals(unclass(result[["selectionString"]]),"instrument:bond & currency:JPY + instrument:bond,equity & currency:usd,chf + amount:<=100.3CHF")
	checkEquals(unclass(result[["directiveString"]]),NA_character_)
	
	
}


test.shouldSplitExplodeDirectiveStringElement <- function() {
	explodeDirectiveString <- new("DirectiveStringElement"," explode : Fondi_misti  ")
	
	result <- split(explodeDirectiveString)
	checkEquals(result,new("ExplodeDirective","Fondi_misti"))
}

test.shouldSplitGroupByDirectiveStringElement <- function() {
	groupByDirectiveString <- new("DirectiveStringElement"," groupBy : securityId  ")
	
	result <- split(groupByDirectiveString)
	checkEquals(result,new("GroupByDirective","securityId"))
}


test.shouldSplitReplaceDirectiveStringElement <- function() {
	replaceDirectiveString <- new("DirectiveStringElement","   replace  :   Opzioni_su_azioni  , Opzioni_su_divise           ")

	result <- split(replaceDirectiveString)
	checkEquals(result,new("ReplaceDirective",c("Opzioni_su_azioni","Opzioni_su_divise")))
}


test.shouldFailToSplitDirectiveStringElement <- function() {
	replaceDirectiveString <- new("DirectiveStringElement","   roplace  :   Opzioni_su_azioni  , Opzioni_su_divise           ")
	
	checkException(split(replaceDirectiveString))
}

test.shouldSplitDirectiveString <- function() {
	
	# DirectiveString: a string like "explode:Fondi_misti & replace:Opzioni_su_azioni,Opzioni_su_divise"
	x <- new("DirectiveString","groupBy:securityId & explode:Fondi_misti & replace:Opzioni_su_azioni,Opzioni_su_divise")
	
	r1 <- new("GroupByDirective","securityId")
	r2 <- new("ExplodeDirective","Fondi_misti")
	r3 <- new("ReplaceDirective",c("Opzioni_su_azioni","Opzioni_su_divise"))
	result <- split(x)
	
	checkEquals(length(result),3)
	checkEquals(result[[1]],r1)	
	checkEquals(result[[2]],r2)
	checkEquals(result[[3]],r3)
	
}
