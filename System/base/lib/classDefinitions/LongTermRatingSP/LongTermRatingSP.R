# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("LongTermRatingSP",contains="factor")

getLongTermRatingLevelsSP <- function() {
	levels <- c("NR","D","C","CC","CCC-","CCC","CCC+",
			"B-","B","B+","BB-","BB","BB+","BBB-","BBB","BBB+",
			"A-","A","A+","AA-","AA","AA+","AAA")
	return(levels)
}

as.longTermRatingSP <- function(x) {
	
	levels <- getLongTermRatingLevelsSP()
	if (missing(x)) {
		x <- levels
	}
	longTermRating <- factor(x,levels=levels,ordered=TRUE)
	longTermRating <- new("LongTermRatingSP",longTermRating)
	return(longTermRating)
}



