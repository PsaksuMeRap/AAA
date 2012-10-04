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
			
			## "C/2012-08-17/Strike 1.295/EUR -250000/Premium 1930 USD"
			newName <- paste(info1[[optionType]],
					info1[["expiryDate"]],
					paste("Strike",info1[["strike"]]),
					sep="/"
			)
			
			
			????
			tmp1 <- stringr::str_trim(strsplit(name,"/")[[1]])
			names(tmp1) <- c("optionType","expiryDate","strike","amount","premium")
			tmp1 <- as.list(tmp1)
			
			tmp1[["strike"]] <- as.numeric(strsplit(tmp1[["strike"]]," ")[[1]][[2]])
			
			money <- strsplit(tmp1[["amount"]]," ")[[1]]
			tmp1[["amount"]] <- as.numeric(money[[2]])
			tmp1[["underlying"]] <- money[[1]]
			
			premium <- strsplit(tmp1[["premium"]]," ")[[1]]
			tmp1[["premium"]] <-  as.numeric(premium[[2]])
			tmp1[["numeraire"]] <-  premium[[3]]
			
			
			
			z <- new("PositionOpzioni_su_divise",
					id=x@id,
					security=security,
					quantity=x@quantity+y@quantity,
					value=x@value+y@value
			)
			
			return(z)
		}
)
