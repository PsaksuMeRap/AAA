# TODO: Add comment
# 
# Author: claudio
###############################################################################


LongTermRatingFactory <- function(characterRate) {
	
	levels <- getLongTermRatingLevelsSP()
	
	if (is.element(characterRate,levels)) {
		return(as.longTermRatingSP(characterRate))
	}
	
	levelsMoody <- getLongTermRatingLevelsMoody
	
	if (is.element(characterRate,levels)) {
		return(as.longTermRatingMoody(characterRate))
	}
	
	stop(paste("Error in LongTermRatingFactory.\nUnknown rate:",characterRate,"\nPlease verify the input."))
}


