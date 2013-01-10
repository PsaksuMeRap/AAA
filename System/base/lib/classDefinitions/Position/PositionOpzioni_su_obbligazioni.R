# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionOpzioni_su_obbligazioni"
setClass("PositionOpzioni_su_obbligazioni",representation(rating="LongTermRating"),contains="Position")

setMethod("reweight",signature(x="PositionOpzioni_su_obbligazioni"),
		function(x,weight) {

			position <- new("PositionOpzioni_su_obbligazioni",
					id=x@id,
					security=x@security,
					quantity=weight*x@quantity,
					value=weight*x@value,
					rating=x@rating
			
			)
			return(position)
		}
)


setMethod("groupBySecurityId",signature(x="PositionOpzioni_su_obbligazioni",y="PositionOpzioni_su_obbligazioni"),
		function(x,y) {
			info1 <- getOptionParameters(x)
			info2 <- getOptionParameters(y)
			
			callPut <- function(x) {if (x=="C") return("Call") else return("Put")}
			
			p1 <- info1[["premium"]]
			p2 <- info1[["premium"]]
			newAmount<- p1$amount+p2$amount
			newPremium <- p1$premium+p2$premium
			newNumeraire <- p1$numeraire
			
			## This is the name structure
			## "PUT 17-05-12 Strike 103.5 CHF 125000 Premio(-345.45 CHF) CH0031240127 "
			premium <- paste("Premio(",format(newPremium,scientific=FALSE)," ",newNumeraire,")",sep="")
			newName <- paste(toupper(callPut(info1[["optionType"]])),
					format(strptime(info1[["expiryDate"]],format="%Y-%m-%d"),"%d-%m-%y"),
					"Strike",info1[["strike"]], info1[["numeraire"]],
					amount,
					premium,
					info1[["isin"]]
			)
			
			security <- x@security
			security@name <- newName
			
			z <- new("PositionOpzioni_su_obbligazioni",
					id=x@id,
					security=security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value,
					rating=x@rating
			)
			
			return(z)
		}
)
