# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("LongTermRatingSP",contains="character")

getLongTermRatingLevelsSP <- function() {
	levels <- 1:23
	rates <- c("NR","D","C","CC","CCC-","CCC","CCC+",
			"B-","B","B+","BB-","BB","BB+","BBB-","BBB","BBB+",
			"A-","A","A+","AA-","AA","AA+","AAA")
	names(levels) <- rates
	return(levels)
}

