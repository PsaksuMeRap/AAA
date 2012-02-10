# TODO: Add comment
# 
# Author: ortellic
###############################################################################

validityCurrency <- function(object) {
	if (length(object@.Data)>1) return(paste("Invalid currency length:",object@.Data))
	allowedCurrencies = c("CHF","EUR","USD","CHr")
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

setClass("Currency",contains="character",validity=validityCurrency)


