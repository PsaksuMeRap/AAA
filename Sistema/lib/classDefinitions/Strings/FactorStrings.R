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
			factorString.l <- lapply(factorString.v,function(x)return(new("FactorString",x)))
			return(factorString.l)
		}
)



setGeneric("toSelectionCriteria", function(x) standardGeneric("toSelectionCriteria"))

setMethod("toSelectionCriteria",
		signature(x = "FactorStrings"),
		function (x) 
		{
			factorString.l <- split(x)
			factorStringParsed.l <- lapply(factorString.l,split)
			selectionCriterium.l <- sapply(factorStringParsed.l,selectionCriteriumFactory)
			return(new("SelectionCriteria",selectionCriterium.l))
		}
)