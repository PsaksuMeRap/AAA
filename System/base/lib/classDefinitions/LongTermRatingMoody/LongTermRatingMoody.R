# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("LongTermRatingMoody",contains="factor")

getLongTermRatingLevelsMoody <- function() {
	levels <- c("WR","C","Ca","Caa3","Caa2","Caa1",
			"B3","B2","B1","Ba3","Ba2","Ba1","Baa3","Baa2","Baa1",
			"A3","A2","A1","Aa3","Aa2","Aa1","Aaa")
	return(levels)
}

as.MoodylongTermRating <- function(x) {
	
	levels <- getLongTermRatingLevelsMoody()
	if (missing(x)) {
		x <- levels
	}
	longTermRating <- factor(x,levels=levels,ordered=TRUE)
	longTermRating <- new("LongTermRatingMoody",longTermRating)
	return(longTermRating)
}



