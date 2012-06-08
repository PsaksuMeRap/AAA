# TODO: Add comment
# 
# Author: Claudio
###############################################################################


setClass("Trade",contains="namedList")

setMethod("sign",signature(x="Trade"),
		definition=function(x) {
			buy <- c("buy","buy to open","buy to sell")
			if (is.element(tolower(x$Buy_Sell),buy)) return(1) else return(-1)
		}
)
