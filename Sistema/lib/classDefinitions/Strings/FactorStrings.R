# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("FactorStrings",contains="character")

setMethod("split",
		signature(x = "FactorStrings"),
		function (x, f, drop = FALSE, ...) 
		{
			# factorsString: a string like "instrument:equity & currency:CHF"
			
			factorString.v <- unlist(strsplit(x,"\\&"))
			factorString.v <- removeStartEndSpaces(factorString.v)
			factorString.v <- lapply(factorString.v,function(x)return(new("FactorString",x)))
			return(factorString.v)
		}
)
