# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertMoodyRateToSPRate <- function() {
	
	rate <- LongTermRatingFactory("Ca")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,as.longTermRatingSP("C"))

	rate <- LongTermRatingFactory("Aa2")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,as.longTermRatingSP("AA"))
	
	rate <- LongTermRatingFactory("C")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,as.longTermRatingSP("D"))
	
}
