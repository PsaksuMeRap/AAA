# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("groupByDirective",def=function(x,positions) standardGeneric("groupByDirective"))

setMethod("groupByDirective",
		signature(x="GroupByDirective",positions="Positions"),
		function(x,positions) {
			groupCriterium <- as.character(x)
			
			if (groupCriterium=="securityId") {
				result <- groupBy(positions,groupCriterium)
				return(result)
			}
			
			stop(paste("Error: the groupByDirective",x,"has not been implemented yet."))
		}
)