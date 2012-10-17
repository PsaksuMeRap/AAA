# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertMoodyRateToSPRate <- function() {
	
	rate <- longTermRatingFactory("Ca")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","C"))

	rate <- longTermRatingFactory("Aa2")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","AA"))
	
	rate <- longTermRatingFactory("C")
	result <- LongTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","D")
	
}
