# TODO: Add comment
# 
# Author: ortellic
###############################################################################


# crea la classe AyrtonPositions uguale all'insieme dei records della tabella DBPortfolioGenerale
setClass("AyrtonPositions",contains="list")

setMethod(`[`,signature(x="AyrtonPositions"),
		function (x, i, j, ..., drop = TRUE) {
			return(new("AyrtonPositions",x@.Data[i]))
		}
)

