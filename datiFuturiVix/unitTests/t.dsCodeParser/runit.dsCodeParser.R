# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.create_dsCodeParser <- function() {
	
	dsCodeParser <- create_dsCodeParser()
	
	checkEquals(class(dsCodeParser),"dsCodeParser")
	
}


test.dsCodeParser_extractContractName <- function() {
	
	code <- "CVX0311(OI)"
	parser <- create_dsCodeParser()
	attributes <- parser$extractContractName(code)
	
	checkEquals(attributes$nameWithoutType,"CVX0311")	
	checkEquals(attributes$dataType,"OI")
	checkEquals(attributes$year,"2011")
	checkEquals(attributes$month,"03")
	
}

test.dsCodeParser_extractNameWithoutType <- function() {
	
	code <- "CVX0311(OI)"
	parser <- create_dsCodeParser()
	nameWithoutType <- parser$extractNameWithoutType(code)
			
	checkEquals(nameWithoutType,"CVX0311")
}