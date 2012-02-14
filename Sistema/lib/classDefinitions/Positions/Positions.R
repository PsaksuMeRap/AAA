# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setClass("Positions",contains="list")

setMethod("union",signature(x="Positions",y="Positions"),
		
		function(x,y) {
			return(new("Positions",c(x@.Data,y@.Data)))
		}
)