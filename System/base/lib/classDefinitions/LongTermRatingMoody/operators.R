# TODO: Add comment
# 
# Author: claudio
###############################################################################




setMethod(">",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]>levels[[e2]])
		}
)


setMethod(">=",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]>=levels[[e2]])
		}
)


setMethod("<",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]<levels[[e2]])
		}
)

setMethod("<=",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]<=levels[[e2]])
		}
)

setMethod("==",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]==levels[[e2]])
		}
)

setMethod("!=",signature("LongTermRatingMoody","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsMoody()
			return(levels[[e1]]!=levels[[e2]])
		}
)



# now the same operators but with the second argument as LongTermRatingSP

setMethod(">",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]>levels[[e2]])
		}
)


setMethod(">=",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]>=levels[[e2]])
		}
)


setMethod("<",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]<levels[[e2]])
		}
)

setMethod("<=",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]<=levels[[e2]])
		}
)

setMethod("==",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]==levels[[e2]])
		}
)

setMethod("!=",signature("LongTermRatingMoody","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[longTermRatingMoodyToSP(e1)]]!=levels[[e2]])
		}
)

