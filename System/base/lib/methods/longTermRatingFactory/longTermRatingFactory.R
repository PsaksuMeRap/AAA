# TODO: Add comment
# 
# Author: claudio
###############################################################################


longTermRatingFactory <- function(characterRate) {

	if (is.na(characterRate) | is.null(characterRate) | length(characterRate)==0) {
		rating <- new("LongTermRatingSP","NR")
	} else {
		ratingCodesSP <- names(getLongTermRatingLevelsSP())
		if (is.element(characterRate,ratingCodesSP)) {
			rating <- new("LongTermRatingSP",characterRate)		
		} else  {
			ratingCodesMoody <- names(getLongTermRatingLevelsMoody())
			if (is.element(characterRate,ratingCodesMoody)) {
				rating <- new("LongTermRatingMoody",characterRate)					
			} else {
				rating <- new("LongTermRatingSP","NR")
			}
		}
		
	}
	
	return(rating)
	
}


