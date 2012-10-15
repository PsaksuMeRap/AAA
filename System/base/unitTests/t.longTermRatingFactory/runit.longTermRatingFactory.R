# TODO: Add comment
# 
# Author: claudio
###############################################################################


tests.shouldCreateSPRate <- function() {
	rate <- "AA+"

	rateSP <- LongTermRatingFactory(rate)
	
	checkEquals(as.character(rateSP),rate)
}

tests.shouldFailToCreateLongTermRating <- function() {
	rate <- "A1+"
	
	checkException(LongTermRatingFactory(rate))
}


tests.shouldCreateMoodyRate <- function() {
	rate <- "B1"
	
	rateSP <- LongTermRatingFactory(rate)
	
	checkEquals(as.character(rateSP),rate)
}
