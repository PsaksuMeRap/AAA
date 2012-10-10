# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("SPlongTermRating",contains="factor")


as.SPlongTermRating <- function(x) {
	levels <- c("NR","D","C","CC","CCC-","CCC","CCC+",
			"B-","B","B+","BB-","BB","BB+","BBB-","BBB","BBB+",
			"A-","A","A+","AA-","AA","AA+","AAA")
	if (missing(x)) {
		x <- levels
	}
	longTermRating <- factor(x,levels=levels,ordered=TRUE)
	longTermRating <- new("SPlongTermRating")
	return(longTermRatings)
}



