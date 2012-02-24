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
