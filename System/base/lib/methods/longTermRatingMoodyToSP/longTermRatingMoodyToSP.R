# TODO: Add comment
# 
# Author: claudio
###############################################################################


longTermRatingMoodyToSP <- function(MoodyRate) {
	
	MoodyRate <- as.character(MoodyRate)

	if (MoodyRate=="Ca") return(new("LongTermRatingSP","C"))
	
	# create the Standard and Poors levels
	levelsSP <- names(getLongTermRatingLevelsSP())
	
	# remove the unnecessary terms
	toHold <- !is.element(levelsSP,c("CC","C"))
	levelsSP <- levelsSP[toHold]
	
	# idem for Moodys
	levelsMoody <- names(getLongTermRatingLevelsMoody())
	toHold <- levelsMoody != "Ca"
	levelsMoody <- levelsMoody[toHold]

	names(levelsSP) <- levelsMoody
	return(new("LongTermRatingSP",levelsSP[[MoodyRate]]))
}

