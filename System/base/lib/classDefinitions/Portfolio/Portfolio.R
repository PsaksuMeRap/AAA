# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("Portfolio",representation(owner="character",referenceCurrency="Currency"),
		contains="Positions"
)

setMethod(`[`,signature(x="Portfolio"),
		function(x,i,j,...,drop=TRUE) {
			positions <- x@.Data[i]
			return(new("Positions",positions))
		}
)

setMethod("+",signature("Portfolio","Positions"),
		function(e1,e2) {
			nbNewPositions <- length(e2)
			
			if (nbNewPositions==0) return(portfolio)

			portfolio <- new("Portfolio",owner=e1@owner,
					referenceCurrency=e1@referenceCurrency,join(e1,e2))
			return(portfolio)
		}
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
		function(x,formatWidth=TRUE,withReferenceCurrency=FALSE) {

			print(paste("Owner:",x@owner))
			print(paste("Reference currency:",x@referenceCurrency))
			if (withReferenceCurrency) {
				a <- sapply(as(x@.Data,"Positions"),print,formatWidth,referenceCurrency=x@referenceCurrency)
			} else {
				a <- sapply(as(x@.Data,"Positions"),print,formatWidth)	
			}
			return(invisible())
		}
)

setMethod("as.character","Portfolio",
		function(x,withReferenceCurrency=FALSE) {
			strings <- paste("Owner:",x@owner)
			strings[2] <- paste("Reference currency:",x@referenceCurrency)
			if (withReferenceCurrency) {
				strings <- c(strings,sapply(as(x@.Data,"Positions"),as.character,x@referenceCurrency))
			} else {
				strings <- c(strings,sapply(as(x@.Data,"Positions"),as.character))
			}
			return(strings)
		}
)
