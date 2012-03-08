# TODO: Add comment
# 
# Author: claudio
###############################################################################


selector <- function(selectionCriterium,positions,...) {
	# apply the function FUNC
	extract <- lapply(positions,check,selectionCriterium)
	extract <- unlist(extract)
	
	return(extract)
}

setGeneric("selector",def=function(selectionCriterium,positions,...) standardGeneric("selector"))

#setMethod("selector",signature(selectionCriterium="CurrencySelectionCriterium",positions="Positions"),
#		function(selectionCriterium,positions) {
		
#			extract <- lapply(positions,FUNC,selectionCriterium)
#			extract <- unlist(extract)
			
#			return(extract)
#		}
#)


setMethod("selector",signature(selectionCriterium="AmountSelectionCriterium",positions="Positions"),
		function(selectionCriterium,positions) {
	
			if (is(selectionCriterium@constraint,"RelativeConstraint")) {
				totalValue <- sum(positions)
				percentageValue <- selectionCriterium@constraint@value/100
				absoluteConstraint <- new("AbsoluteConstraint",
						operator=selectionCriterium@constraint@operator,
						value=toMoney(percentageValue*totalValue@amount,totalValue@currency)
				)
				selectionCriterium@constraint <- absoluteConstraint
			}
		
			extract <- lapply(positions,check,selectionCriterium)
			extract <- unlist(extract)
			
			return(extract)
		}
)


setMethod("selector",signature(selectionCriterium="MaturityHorizonSelectionCriterium",positions="Positions"),
		function(selectionCriterium,positions,...) {
			# extract the baseDate from the ... arguments
			x <- list(...)
			if (length(x)==0) baseDate = Sys.Date() else baseDate=as.Date(x[[1]])
		
			FUNC <- function(position,selectionCriterium,baseDate) {
				
				values <- selectionCriterium@values
				result <- FALSE				
				if (is(position@security,"Fondi_mercato_monetario")) {
					if (identical(values,"<3Y")) result <- TRUE
					if (selectionCriterium@negation) result <- !result
					return(result)
				}
				
				if (is(position@security,"Bond")) {
					bondMaturity <- as.Date(position@security@maturity)
					maturityInYears <- as.integer(bondMaturity - baseDate)/365
					if (maturityInYears <= 3) maturityHorizon <- "<3Y"
					if (maturityInYears > 3) maturityHorizon <- ">3Y"
					
					if (identical(maturityHorizon,values)) result <- TRUE
					if (selectionCriterium@negation) result <- !result
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
					if (selectionCriterium@negation) result <- !result
					return(result)
				}
				
				if (is(position@security,"Strutturati_FI")) {
					if (identical(position@security@underlyingHorizon,values)) result <- TRUE
					if (selectionCriterium@negation) result <- !result
					return(result)
				}		
				return(result)
			}
			
			# apply the function FUNC
			extract <- lapply(positions,FUN=FUNC,selectionCriterium,baseDate)
			extract <- unlist(extract)
			
			return(extract)
			
		}
)
