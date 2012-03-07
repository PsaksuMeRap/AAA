# TODO: Add comment
# 
# Author: claudio
###############################################################################



setGeneric("selector",def=function(selectionCriterium,positions,...) standardGeneric("selector"))


selector <- function(selectionCriterium,positions) {
	# apply the function FUNC
	extract <- lapply(positions,check,selectionCriterium)
	extract <- unlist(extract)
	
	return(extract)
}


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
						value=toMoney(percentageValue*totalValue@amount,totalValue$currency)
				)
				selectionCriterium@constraint <- absoluteConstraint
			}
			
			# apply the function FUNC
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
				
				values <- selectionCriterium$values
				
				# attenzione: poiché una position accruedInterest è anche bond
				# occorre testare prima accruedInterest
				if (is.element("accruedInterest",class(position))) {
					if (values=="<3Y") return(TRUE) else return(FALSE)
				}
				
				if (is.element("Fondi_mercato_monetario",class(position))) {
					if (values=="<3Y") return(TRUE) else return(FALSE)
				}
				
				if (is.element("bond",class(position))) {
					bondMaturity <- as.Date(position$getMaturity())
					maturityInYears <- as.integer(bondMaturity - baseDate)/365
					if (maturityInYears <= 3) maturityHorizon <- "<3Y"
					if (maturityInYears > 3) maturityHorizon <- ">3Y"
					if (maturityHorizon == values[1]) return(TRUE) else return(FALSE)
				}
				
				if (is.element("Fondi_obbligazionari",class(position))) {
					if (grepl("<3Y",x=position$name)) {
						averageHorizon = "<3Y"
					} else {
						if (grepl(">3Y",x=position$name)) {
							averageHorizon = ">3Y"
						} else {
							stop("errore il fondo obbligazionario non ha un indice di durata <3Y o >3Y")
						}
					}
					
					if (averageHorizon == values[1]) return(TRUE) else return(FALSE)
				}
				
				if (is.element("Strutturati_FI",class(position))) {
					if (position$underlyingHorizon == values[1]) return(TRUE) else return(FALSE)
				}		
				return(NA)
			}
			# apply the function FUNC
			extract <- lapply(positions$positions,FUN=FUNC,selectionCriterium,baseDate)
			extract <- unlist(extract)
			
			# convert NA to FALSE
			extract[is.na(extract)] <- FALSE
			
			return(extract)
			
		}
)
