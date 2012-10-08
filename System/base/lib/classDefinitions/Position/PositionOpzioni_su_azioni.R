# TODO: Add comment
# 
# Author: Claudio
###############################################################################


# crea la classe "PositionOpzioni_su_azioni"
setClass("PositionOpzioni_su_azioni",representation(numberEquities="numeric",contractSize="numeric"),contains="Position")

setMethod("reweight",signature(x="PositionOpzioni_su_azioni"),
		function(x,weight) {

			position <- new("PositionOpzioni_su_azioni",
					numberEquities=weight*x@numberEquities,
					contractSize=x@contractSize,
					id=x@id,
					security=x@security,
					quantity=weight*x@quantity,
					value=weight*x@value
			
			)
			return(position)
		}
)


setMethod("groupBySecurityId",signature(x="PositionOpzioni_su_azioni",y="PositionOpzioni_su_azioni"),
		function(x,y) {
			info1 <- getOptionParameters(x)
			info2 <- getOptionParameters(y)
			
			newQuantity <- info1[["quantity"]]+info2[["quantity"]]
			
			callPut <- function(x) {if (x=="C") return("Call") else return("Put")}
			
			parsePremium <- function(premium) {
				tmp <- stringr::str_trim(strsplit(premium,"Premio\\(")[[1]][[2]])
				tmp <- as.list(stringr::str_trim(strsplit(tmp," ")[[1]]))
				tmp[[1]] <- as.numeric(tmp[[1]])
				tmp[[2]] <- substr(tmp[[2]],1,3)
				names(tmp) <- c("premium","numeraire")
 				return(tmp)
			}
			
			p1 <- parsePremium(info1[["premium"]])
			p2 <- parsePremium(info1[["premium"]])
			newPremium <- p1$premium+p2$premium
			newNumeraire <- p1$numeraire
			
			## This is the name structure
			## "-100 / Call / Syngenta AG / 17-02-12 / Strike 290 / Premio(5500 CHF) / CH0011027469 / 337.90 / 10"
			premium <- paste("Premio(",format(newPremium,scientific=FALSE)," ",newNumeraire,")",sep="")
			newName <- paste(newQuantity,
					callPut(info1[["optionType"]]),
					info1[["name"]],
					format(strptime(info1[["expiryDate"]],format="%Y-%m-%d"),"%d-%m-%y"),
					paste("Strike",info1[["strike"]]),
					premium,
					info1[["isin"]],
					info1[["underlyingPrice"]],
					info1[["contractSize"]],
					sep=" / "
			)
			
			security <- x@security
			security@name <- newName
			
			z <- new("PositionOpzioni_su_azioni",
					numberEquities=x@numberEquities+y@numberEquities,
					contractSize = x@contractSize,
					id=x@id,
					security=security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
