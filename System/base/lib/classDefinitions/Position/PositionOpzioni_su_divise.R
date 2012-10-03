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
			
			"Call 17-08-12 Strike 1.295 EUR -250000 Premio(1930 USD)"
			
			z <- new("PositionOpzioni_su_divise",
					id=x@id,
					security=security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
