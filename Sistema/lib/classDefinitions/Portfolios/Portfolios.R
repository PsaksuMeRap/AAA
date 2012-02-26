# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Portfolios",contains="list")

setMethod(`[`,signature(x="Portfolios"),
		function(x,i,j,...,drop=TRUE) {
			portfolios <- x@.Data[i]
			return(new("Portfolios",portfolios))
		}
)


setMethod("join",signature(x="Portfolio",y="Portfolios"),
		
		function(x,y,newOwner,newReferenceCurrency) {
			
			positions <- lapply(y,function(x)return(x@.Data))
			positions <- unlist(positions,recursive=FALSE)
			newPositions <- c(x@.Data,positions)
			
			if (missing(newOwner)) {
				owners <- c(x@owner,extractSlotFromList(y,"owner"))
				newOwner <- paste(owners,collapse="_")
			}
			
			if (missing(newReferenceCurrency)) newReferenceCurrency <- x@referenceCurrency
			return(new("Portfolio",owner=newOwner,referenceCurrency=newReferenceCurrency,
							newPositions))			
		}
)


setMethod("print","Portfolios",
		function(x,width=list(empty=TRUE)) {
			for (portfolio in x) {
				print(portfolio,width=width)
				print("")
			}
		}
)
