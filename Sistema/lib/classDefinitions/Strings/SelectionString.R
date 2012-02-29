# TODO: Add comment
# 
# Author: claudio
###############################################################################


setClass("ConstraintString",contains="character")

setMethod("split",
		signature(x = "SelectionString"),
		function (x, f, drop = FALSE, ...) 
		{
			#selectionString: a string like "instrument:equity & currency:CHF + instrument:bond"
			
			factorStrings.v <- unlist(strsplit(x,"\\+"))
			factorStrings.v <- removeStartEndSpaces(factorStrings.v)
			factorStrings.v <- lapply(factorStrings.v,function(y)return(new("FactorStrings",y)))
			return(factorStrings.v)
		}
)
