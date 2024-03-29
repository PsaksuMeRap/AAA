# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("check",def=function(x,selectionCriterium) standardGeneric("check"))

setMethod("check",signature(x="Position",selectionCriterium="AmountSelectionCriterium"),
		function(x,selectionCriterium) {
			operator <- selectionCriterium@constraint@operator
			# we assume that selectionCriterium$constraint is of class AbsoluteConstraint 
			# i.e. any RelativeConstraint has been converted from relative (%) to absolute
			# before beeing passed to this method. The field negation is not considered for
			# criteria of kind amount.
	
			# excecute the check
			if (operator==">" ) {
				result <- x@value > selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (operator==">=")  {
				result <- x@value >= selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (operator=="<" )  {
				result <- x@value < selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (operator=="<=")  {
				result <- x@value <= selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (operator=="!=")  {
				result <- x@value != selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (operator=="=" )  {
				result <- x@value == selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			
			stop (paste("Error: invalid operator",operator))
		}
)


setMethod("check",signature(x="Position",selectionCriterium="SecuritySelectionCriterium"),
		function(x,selectionCriterium) {
			classes <- selectionCriterium@values
			securityClass <- class(x@security)
			result <- any(is.element(securityClass,classes))
			if (selectionCriterium@negation) result <- !result
			return(result)			
		}
)


setMethod("check",signature(x="Position",selectionCriterium="CurrencySelectionCriterium"),
		function(x,selectionCriterium) {
			currencies <- selectionCriterium@values
			instrumentCurrency <- unclass(x@security@currency)
			result <- is.element(instrumentCurrency,currencies)
			if (selectionCriterium@negation) result <- !result
			return(result)
		}
)

