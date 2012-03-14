# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Portfolio",representation(owner="character",referenceCurrency="Currency"),
		contains="Positions"
)

setMethod("join",signature(x="Portfolio",y="Portfolio"),
		
		function(x,y,newOwner,newReferenceCurrency) {

			newPositions <-	join(x=as(x,"Positions"),y=as(y,"Positions"))
	
			if (missing(newOwner)) newOwner <- paste(x@owner,y@owner,sep="_")
	
			if (missing(newReferenceCurrency)) newReferenceCurrency <- x@referenceCurrency
			return(new("Portfolio",owner=newOwner,referenceCurrency=newReferenceCurrency,
							newPositions))			
		}
)

setMethod("print","Portfolio",
		function(x,width=list(empty=TRUE)) {
			print(paste("Owner:",x@owner))
			print(paste("Reference currency:",x@referenceCurrency))
			sapply(as(x@.Data,"Positions"),print,width=width)
		}
)

setMethod("as.character","Portfolio",
		function(x,width=list(empty=TRUE)) {
			strings <- paste("Owner:",x@owner)
			strings[2] <- paste("Reference currency:",x@referenceCurrency)
			strings <- c(strings,sapply(as(x@.Data,"Positions"),as.character,width=width))
			return(strings)
		}
)
