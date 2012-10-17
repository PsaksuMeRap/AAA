# TODO: Add comment
# 
# Author: claudio
###############################################################################


tests.shouldCreateSPRate <- function() {
	rate <- "AA+"

	rateSP <- longTermRatingFactory(rate)
	
	checkEquals(as.character(rateSP),rate)
}



tests.shouldCreateMoodyRate <- function() {
	rate <- "B1"
	
	rateSP <- longTermRatingFactory(rate)
	
	checkEquals(as.character(rateSP),rate)
}
