# TODO: Add comment
# 
# Author: ortellic
###############################################################################


test.shouldJoinTwoPositions <- function() {
	# The class Positions is a container class (a list)
	# where the content is not checked for consistency
	
	# test1: two non empty positions
	e1 <- new("Positions",list("a","B",1:10))
	e2 <- new("Positions",list("Claudio",12.3))
	
	e3 <- join(e1,e2)
	
	checkEquals(e3,new("Positions",list("a","B",1:10,"Claudio",12.3)))
	
	# test2: two empty positions
	e1 <- new("Positions",list())
	e2 <- new("Positions",list())
	
	e3 <- join(e1,e2)
	
	checkEquals(e3,new("Positions",list()))
}



test.shouldExtractPositions <- function() {
	
	source("./unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	Positions <- new("Positions",positions)
	
	# test1: with a numeric index
	result <- Positions[c(1,3)]
	checkEquals(is(result,"Positions"),TRUE)
	checkEquals(result,new("Positions",positions[-2]))

	# test1: with a logical index
	result <- Positions[c(TRUE,FALSE,TRUE)]
	checkEquals(is(result,"Positions"),TRUE)
	checkEquals(result,new("Positions",positions[-2]))
}

test.shouldSumPositions <- function() {
	
	source("./unitTests/utilities/createRepositoryPositions.R")
	repository <- createRepositoryPositions()
	
	# create the positions
	positions <- list(repository$equity1,repository$equity2,repository$bond1)
	# equity1 CHF: 88205 CHF, equity2 EUR: 7439.7503136 CHF, bond1 EUR: 124345.632268 CHF,
	positions <- new("Positions",positions)
	
	# test
	result <- sum(positions)
	should <- toMoney(88205 + 7439.7503136 + 124345.632268,"CHF")
	checkEquals(result,should)
}