# TODO: Add comment
# 
# Author: ortellic
###############################################################################

setClass("Currency",contains="character",
		validity=function(object) {
			if (length(object@.Data)>1) return(paste("Invalid currency length:",object@.Data))
			allowedCurrencies = c("CHF","EUR","USD","AUD","NOK","GBP","INR","BRL","JPY","CAD","NZD","CHr")
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
