# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("check",def=function(x,selectionCriterium) standardGeneric("check"))

setMethod("check",signature(x="Position",selectionCriterium="RatingSelectionCriterium"),
		function(x,selectionCriterium) {
			
			# if the position x does not have a slot rating then return FALSE
			if (!.hasSlot(x,"rating")) return(FALSE) 
			
			# if the relational operator is empty then perform a normal check
			relationalOperator <- selectionCriterium@relationalOperator
			if (relationalOperator=="") return(is.element(x@rating,selectionCriterium@values))
			
			# otherwise transform the values string into longTermRating
			values <- lapply(selectionCriterium@values,longTermRatingFactory)
			
			# excecute the check
			if (relationalOperator==">" ) {
				result <- longTermRatingFactory(x@rating)>values
				result <- all(result)
				if (selectionCriterium@negation) result <- !result
				
				return(result)
			}
			if (relationalOperator==">=")  {
				result <- x@rating >= selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (relationalOperator=="<" )  {
				result <- x@rating < selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (relationalOperator=="<=")  {
				result <- x@rating <= selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (relationalOperator=="!=")  {
				result <- x@rating != selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			if (relationalOperator=="=" )  {
				result <- x@rating == selectionCriterium@constraint@value
				if (selectionCriterium@negation) result <- !result
				return(result)
			}
			
			stop (paste("Error: invalid operator",operator))
		}
)


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
			securityClass <- class(x@security)[[1]]
			
			# 2012-09-17 eventualmente creare nuovo selectionCriterium per prendere anche le sottoclassi			
			# securityClass <- is(x@security)
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

