# TODO: Add comment
# 
# Author: claudio
###############################################################################



setMethod(">",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]>levels[[e2]])
		}
)


setMethod(">=",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]>=levels[[e2]])
		}
)


setMethod("<",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]<levels[[e2]])
		}
)

setMethod("<=",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]<=levels[[e2]])
		}
)

setMethod("==",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]==levels[[e2]])
		}
)

setMethod("!=",signature("LongTermRatingSP","LongTermRatingSP"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]!=levels[[e2]])
		}
)



# now the same operators but with the secondOperator as LongTermRatingMoody

setMethod(">",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]>levels[[LongTermRatingMoodyToSP(e2)]])
		}
)


setMethod(">=",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]>=levels[[LongTermRatingMoodyToSP(e2)]])
		}
)


setMethod("<",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]<levels[[LongTermRatingMoodyToSP(e2)]])
		}
)

setMethod("<=",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]<=levels[[LongTermRatingMoodyToSP(e2)]])
		}
)

setMethod("==",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]==levels[[LongTermRatingMoodyToSP(e2)]])
		}
)

setMethod("!=",signature("LongTermRatingSP","LongTermRatingMoody"),
		function(e1,e2) {
			levels <- getLongTermRatingLevelsSP()
			return(levels[[e1]]!=levels[[LongTermRatingMoodyToSP(e2)]])
		}
)

