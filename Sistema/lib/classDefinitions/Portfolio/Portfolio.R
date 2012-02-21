# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Portfolio",representation(owner="character"),contains="Positions")

setMethod("join",signature(x="Portfolio",y="Portfolio"),
		
		function(x,y,newOwner) {
			newPositions <-	join(x=as(x,"Positions"),y=as(y,"Positions"))
			if (missing(newOwner)) newOwner <- paste(x@owner,y@owner,sep="_")
			return(new("Portfolio",owner=newOwner,newPositions))
		}
)
