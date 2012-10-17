# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("LongTermRatingMoody",contains="character")

getLongTermRatingLevelsMoody <- function() {
	levels <- 1:22
	rates 	<- c("WR","C","Ca","Caa3","Caa2","Caa1",
			"B3","B2","B1","Ba3","Ba2","Ba1","Baa3","Baa2","Baa1",
			"A3","A2","A1","Aa3","Aa2","Aa1","Aaa")
	names(levels) <- rates
	return(levels)
}

#as.longTermRatingMoody <- function(x) {
	
#	levels <- getLongTermRatingLevelsMoody()
#	if (missing(x)) {
#		x <- levels
#	}
#	longTermRating <- factor(x,levels=levels,ordered=TRUE)
#	longTermRating <- new("LongTermRatingMoody",longTermRating)
#	return(longTermRating)
#}
