# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setClass("Positions",contains="list")

setMethod("+",signature(e1="Positions",e2="Positions"),
		# it is mandatory for binary operators like "+" to use e1, e2 as argum.
		function(e1,e2) {
			return(new("Positions",c(e1@.Data,e2@.Data)))
		}
)