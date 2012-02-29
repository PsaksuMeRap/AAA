# TODO: Add comment
# 
# Author: ortellic
###############################################################################


setClass("Positions",contains="list")

setMethod("join",signature(x="Positions",y="Positions"),
		
		function(x,y) {
			return(new("Positions",c(x@.Data,y@.Data)))
		}
)

setMethod(`[`,signature(x="Positions"),
		function(x,i,j,...,drop=TRUE) {
			positions <- x@.Data[i]
			return(new("Positions",positions))
		}
)

setMethod("[",signature(x="Positions"),
		function(x,i,j,...,drop=TRUE) {
			positions <- x@.Data[i]
			return(new("Positions",positions))
		}
)

setMethod("print","Positions",
		function(x,width=list(empty=TRUE)) {
			for (i in x@.Data) print(i,width=width)
		}
)

setMethod("sum","Positions",
		function(x) {
			# x: a variable of class Positions (is a list)
			
			balance <- toMoney(amount=0,currency="CHF")
			for (i in x) {
				balance <- sum(balance,i)
			}
			return(balance)
		}
)