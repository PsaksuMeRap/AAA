# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shuldMergeTwoPortfolios <- function() {
	
	
	setClass("Portfolio",representation(owner="character"),contains="Positions")
	
	setMethod("union",signature(x="Portfolio",y="Portfolio"),
			
			function(x,y,newOwner) {
				newPositions <-	union(x=as(x,"Positions"),y=as(y,"Positions"))
				if (missing(newOwner)) newOwner <- paste(x@owner,y@owner,sep="_")
				return(new("Portfolio",owner=newOwner,newPositions))
			}
	)
	
	
	positions1 <- new("Positions",list(uno=1,stringa="uno"))
	positions2 <- new("Positions",list(due=2,stringa="due"))
	
	portfolio1 <- new("Portfolio",owner="Reto",positions1)
	portfolio2 <- new("Portfolio",owner="Claudio",positions2)
	
	# check with or without newOwner
	result <- union(portfolio1,portfolio2)
	checkEquals(result@owner="Reto_Claudio")
	
	result <- union(portfolio1,portfolio2,newOwner="Attilio")
	
}