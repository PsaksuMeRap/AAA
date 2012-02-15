# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldReturnFieldsToPrint <- function() {
	# uses a default method
	source("./unitTests/utilities/createRepositoryPositions.R")
	repoPosition <- createRepositoryPositions()
	
	unclassified <- repoPosition$unclassified1
	result <- fieldsToPrint(unclassified,list(empty=TRUE))
	should <- list(class="Unclassified",
			currency="CHF",
			amount="123.55",
			name="Security not classified")
	
	checkEquals(result,should)
}

test.shouldTransformAs.character <- function() {
	
	# uses a default method
	source("./unitTests/utilities/createRepositoryPositions.R")
	repoPosition <- createRepositoryPositions()
	
	unclassified <- repoPosition$unclassified1
	result <- as.character(unclassified)
	
	checkEquals(result,"Unclassified / CHF / 123.55 / Security not classified")
	
	# now check with some width requirements
	width=list(securityClassName=20,amount=12)
	result <- as.character(unclassified,width)
	checkEquals(result,"Unclassified         / CHF /       123.55 / Security not classified")
	
}
