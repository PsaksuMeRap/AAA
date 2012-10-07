# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionOpzioni_su_divise"
setClass("PositionOpzioni_su_divise",contains="Position")

setMethod("groupBySecurityId",signature(x="PositionOpzioni_su_divise",y="PositionOpzioni_su_divise"),
		function(x,y) {
			info1 <- getOptionParameters(x)
			info2 <- getOptionParameters(y)
			
			newAmount <- info1[["amount"]]+info2[["amount"]]
			newPremium <- info1[["premium"]]+info2[["premium"]]
			
			## "C/2012-08-17/Strike 1.295/EUR -250000/Premium 1930 USD"
			newName <- paste(info1[["optionType"]],
					info1[["expiryDate"]],
					paste("Strike",info1[["strike"]]),
					paste(info1[["underlying"]],newAmount),
					paste("Premium",newPremium,info1[["numeraire"]]),
					sep="/"
			)
			
			security <- x@security
			security@name <- newName
			
			z <- new("PositionOpzioni_su_divise",
					id=x@id,
					security=security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
