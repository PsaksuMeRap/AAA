# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldParseStringWithRelationalOperator <- function() {
	
	# check with an empty string
	string <- ""
	result <- relationalOperatorFactory(string)
	
	checkEquals(result[["operator"]],"")
	checkEquals(result[["values"]],"")	
	
	# check with a string with no operator
	string <- "Aiuto ciao "
	result <- relationalOperatorFactory(string)
	
	checkEquals(result[["operator"]],"")
	checkEquals(result[["values"]],"Aiuto ciao")
	
	# check with a complete string 
	string <- "   > abcDE a "
	result <- relationalOperatorFactory(string)
	
	checkEquals(result[["operator"]],">")
	checkEquals(result[["values"]],"abcDE a")
	
	# check with a string with the operator only
	string <- "   !=  "
	result <- relationalOperatorFactory(string)
	checkEquals(result[["operator"]],"!=")
	checkEquals(result[["values"]],"")

}
