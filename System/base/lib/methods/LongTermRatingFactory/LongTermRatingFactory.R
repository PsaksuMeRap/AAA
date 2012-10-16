# TODO: Add comment
# 
# Author: claudio
###############################################################################


LongTermRatingFactory <- function(characterRate) {
	# check if the argument is a Standard and Poors rate	
	levels <- getLongTermRatingLevelsSP()
	
	if (is.element(characterRate,levels)) {
		return(as.longTermRatingSP(characterRate))
	}
	
	# check if the argument is a Moodys rate	
	levels <- getLongTermRatingLevelsMoody()
	
	if (is.element(characterRate,levels)) {
		return(as.longTermRatingMoody(characterRate))
	}
	
	stop(paste("Error in LongTermRatingFactory.\nUnknown rate:",characterRate,"\nPlease verify the input."))
}


