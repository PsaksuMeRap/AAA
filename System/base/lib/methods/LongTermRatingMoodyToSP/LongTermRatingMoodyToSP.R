# TODO: Add comment
# 
# Author: claudio
###############################################################################


LongTermRatingMoodyToSP <- function(MoodyRate) {
	MoodyRate <- as.character(MoodyRate)
	
	if (MoodyRate=="Ca") return(as.longTermRatingSP("C"))
	
	# create the Standard and Poors levels
	levelsSP <- getLongTermRatingLevelsSP()
	
	# remove the unnecessary terms
	toHold <- !is.element(levelsSP,c("CC","C"))
	levelsSP <- levelsSP[toHold]
	
	# idem for Moodys
	levelsMoody <- getLongTermRatingLevelsMoody()
	toHold <- levelsMoody != "Ca"
	levelsMoody <- levelsMoody[toHold]

	names(levelsSP) <- levelsMoody
	return(as.longTermRatingSP(levelsSP[[MoodyRate]]))
}

