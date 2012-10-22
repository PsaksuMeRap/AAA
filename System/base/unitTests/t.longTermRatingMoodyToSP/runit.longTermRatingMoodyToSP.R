# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldConvertMoodyRateToSPRate <- function() {
	
	rate <- longTermRatingFactory("Ca")
	result <- longTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","C"))

	rate <- longTermRatingFactory("Aa2")
	result <- longTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","AA"))
	
	rate <- longTermRatingFactory("C")
	result <- longTermRatingMoodyToSP(rate)		
	checkEquals(result,new("LongTermRatingSP","D"))
	
}
