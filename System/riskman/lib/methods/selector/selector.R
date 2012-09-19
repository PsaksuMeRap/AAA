# TODO: Add comment
# 
# Author: claudio
###############################################################################


selector <- function(x,positions,...) {
	extract <- lapply(positions,check,x)
	extract <- unlist(extract)
	
	return(extract)
}

setGeneric("selector",def=function(x,positions,...) standardGeneric("selector"))


setMethod("selector",signature(x="AmountSelectionCriterium",positions="Positions"),
		function(x,positions) {
	
			if (is(x@constraint,"RelativeConstraint")) {
				totalValue <- sum(positions)
				percentageValue <- x@constraint@value/100
				absoluteConstraint <- new("AbsoluteConstraint",
						operator=x@constraint@operator,
						value=toMoney(percentageValue*totalValue@amount,totalValue@currency)
				)
				x@constraint <- absoluteConstraint
			}
		
			extract <- lapply(positions,check,x)
			extract <- unlist(extract)
			
			return(extract)
		}
)


setMethod("selector",signature(x="MaturityHorizonSelectionCriterium",positions="Positions"),
		function(x,positions,...) {
			# extract the baseDate from the ... arguments
			y <- list(...)
			if (length(y)==0) baseDate = Sys.Date() else baseDate=as.Date(y[[1]])
		
			FUNC <- function(position,selectionCriterium,baseDate) {
				
				values <- x@values
				result <- FALSE				
				if (is(position@security,"Fondi_mercato_monetario")) {
					if (identical(values,"<3Y")) result <- TRUE
					if (x@negation) result <- !result
					return(result)
				}
				
				if (is(position@security,"Fondi_obbligazionari")) {
					if (grepl("<3Y",x=position@security@name)) {
						averageHorizon = "<3Y"
					} else {
						if (grepl(">3Y",x=position@security@name)) {
							averageHorizon = ">3Y"
						} else {
							message <- paste("Error: the bond fund",position@security@name)
							message <- paste(message,"\nhas no duration index of type '<3Y' or '>3Y'",sep="")
							stop(message)
						}
					}
					
					if (identical(averageHorizon,values)) result <- TRUE
					if (x@negation) result <- !result
					return(result)
				}
				
				if (is(position@security,"Strutturati_FI")) {
					if (identical(position@security@underlyingHorizon,values)) result <- TRUE
					if (x@negation) result <- !result
					return(result)
				}		
				
				if (is(position@security,"Bond")) {
					bondMaturity <- as.Date(position@security@maturity)
					maturityInYears <- as.integer(bondMaturity - baseDate)/365
					if (maturityInYears <= 3) maturityHorizon <- "<3Y"
					if (maturityInYears > 3) maturityHorizon <- ">3Y"
					
					if (identical(maturityHorizon,values)) result <- TRUE
					if (x@negation) result <- !result
					return(result)
				}
	
				return(result)
			}
			
			# apply the function FUNC
			extract <- lapply(positions,FUN=FUNC,x,baseDate)
			extract <- unlist(extract)
			
			return(extract)
			
		}
)



setMethod("selector",signature(x="SelectionString",positions="Positions"),
		function(x,positions) {
			factorStrings.l <- split(x)
		
			selectionCriteriaList <- new("SelectionCriteriaList",lapply(factorStrings.l,toSelectionCriteria))
			toExtract <- filterByCriteriaLogicalOr(selectionCriteriaList,positions)
			
			# crea la lista delle posizioni
			if (any(toExtract)) positionsFiltered <- positions[toExtract] else positionsFiltered <- new("Positions")
			return(positionsFiltered)
		}
)



setMethod("selector",signature(x="SelectionCriteriaList",positions="Positions"),
		function(x,positions) {
			
			toExtract <- filterByCriteriaLogicalOr(x,positions)
			
			# crea la lista delle posizioni
			if (any(toExtract)) positionsFiltered <- positions[toExtract] else positionsFiltered <- new("Positions")
			return(positionsFiltered)
		}
)
