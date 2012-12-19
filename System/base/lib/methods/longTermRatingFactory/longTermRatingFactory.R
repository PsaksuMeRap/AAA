# TODO: Add comment
# 
# Author: claudio
###############################################################################


longTermRatingFactory <- function(characterRate) {

	if (length(characterRate)==0) return(new("LongTermRatingSP","NR"))
	
	if (is.na(characterRate) | is.null(characterRate)) {
		rating <- return(new("LongTermRatingSP","NR"))
	} else {
		ratingCodesSP <- names(getLongTermRatingLevelsSP())
		if (is.element(characterRate,ratingCodesSP)) {
			rating <- new("LongTermRatingSP",characterRate)		
		} else  {
			ratingCodesMoody <- names(getLongTermRatingLevelsMoody())
			if (is.element(characterRate,ratingCodesMoody)) {
				rating <- new("LongTermRatingMoody",characterRate)					
			} else {

				stop(paste("Invalid LongTermRating:",characterRate,"\n",
						"Please verify the checkfile. Exception raised from longTermRatingFactory"))
			}
		}
		
	}
	
	return(rating)
	
}
