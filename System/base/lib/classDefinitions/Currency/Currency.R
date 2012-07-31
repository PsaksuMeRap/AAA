# TODO: Add comment
# 
# Author: ortellic
###############################################################################

setClass("Currency",contains="character",
		validity=function(object) {
			allowedCurrencies <- sys[["allowedCurrencies"]]
			if (is.element(object@.Data,allowedCurrencies)) {
				return(TRUE)
			} else {
				string <- paste(allowedCurrencies,collapse="\n")
				string <- paste("\nInvalid currency: ",object@.Data,
						" is not in the current currency universe given by\n",
						string,sep="")
				return(string)
			}
		}
)

setMethod("as.character",signature(x="Currency"),function(x) return(unclass(x)))
