# TODO: Add comment
# 
# Author: ortellic
###############################################################################


# crea la classe AllocarePositions uguale all'insieme dei records del file di importazione
setClass("AllocarePositions",contains="list")

setMethod(`[`,signature(x="AllocarePositions"),
		function (x, i, j, ..., drop = TRUE) {
			return(new("AllocarePositions",x@.Data[i]))
		}
)

