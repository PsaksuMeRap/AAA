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
				balance <- balance + i@value
			}
			return(balance)
		}
)

setMethod("as.character","Positions", 
		function(x,width=list(empty=TRUE)) {
			
			return(sapply(x,as.character,width=width))
		}
)

setMethod("reweight",signature(x="Positions"),
		function(x,weight) {
			positions <- lapply(x,reweight,weight)
			return(new("Positions",positions))
		}
)
