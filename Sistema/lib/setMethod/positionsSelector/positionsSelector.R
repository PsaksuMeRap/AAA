# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("positionsSelector",function(selectionCriterium,positions,...) standardGeneric("positionsSelector"))
# selectionCriterium: a variable of class selectionCriterium

setMethod("positionsSelector",
		signature(selectionCriterium="CurrencySelectionCriterium",positions="Positions"),
		definition=function(selectionCriterium,positions) {
			FUNC <- function(position,selectionCriterium) {
				check <- is.element(position@security@currency,selectionCriterium@values)
				if (selectionCriterium@negation) return(!check)
				return(check)
			}
			
			# apply the function FUNC
			extract <- lapply(positions,FUNC,selectionCriterium)
			extract <- unlist(extract)
			
			return(extract)
		}
)

setMethod("positionsSelector",
		signature(selectionCriterium="SecuritySelectionCriterium",positions="Positions"),
		definition=function(selectionCriterium,positions) {
			FUNC <- function(position,selectionCriterium) {
				check <- any(is.element(is(position@security),selectionCriterium@values))
				if (selectionCriterium@negation) return(!check)
				return(check)
			}
			
			# apply the function FUNC
			extract <- lapply(positions,FUNC,selectionCriterium)
			extract <- unlist(extract)
			
			return(extract)
		}
)


setMethod("positionsSelector",
		signature(selectionCriterium="AmountSelectionCriterium",positions="Positions"),
		definition=function(selectionCriterium,positions) {
			
			criteriumNew <- selectionCriterium
			if (selectionCriterium@constraint@kind=="relative") {
				totalValue <- sum(positions)
				# ? errore di tipo?
				percentageValue <- selectionCriterium@criteriumCheck@value/100
				criteriumNew@criteriumCheck@value <- toMoney(percentageValue*totalValue@amount,totalValue@currency)
			}
			
			
			# apply the function FUNC
			extract <- lapply(positions,FUN=check,criteriumNew)
			extract <- unlist(extract)
			
			return(extract)
		}
)

setMethod("positionsSelector",
		signature(selectionCriterium="maturityHorizonSelectionCriterium",positions="Positions"),
		definition=function(selectionCriterium,positions,...) {
			# extract the baseDate from the ... arguments
			x <- list(...)
			if (length(x)==0) baseDate <- Sys.Date() else baseDate <- as.Date(x[[1]])
			
			FUNC <- function(position,selectionCriterium,baseDate) {
				
				values <- selectionCriterium@values
				
				if (is(position,"Fondi_mercato_monetario")) {
					if (values=="<3Y") return(TRUE) else return(FALSE)
				}
				
				if (is(position,"Bond")) {
					maturityInYears <- as.integer(as.Date(position@security@maturity) - baseDate)/365
					if (maturityInYears <= 3) maturityHorizon <- "<3Y"
					if (maturityInYears > 3) maturityHorizon <- ">3Y"
					if (identical(maturityHorizon,values[[1]])) return(TRUE) else return(FALSE)
				}
				
				if (is(position,"Fondi_obbligazionari")) {
					if (grepl("<3Y",x=position@security@name)) {
						averageHorizon = "<3Y"
					} else {
						if (grepl(">3Y",x=position@security@name)) {
							averageHorizon = ">3Y"
						} else {
							stop("errore il fondo obbligazionario non ha un indice di durata <3Y o >3Y")
						}
					}
					
					if (identical(averageHorizon,values[[1]])) return(TRUE) else return(FALSE)
				}
				
				if (is.element("Strutturati_FI",class(position))) {
					if (identical(position@security@underlyingHorizon,values[[1]])) return(TRUE) else return(FALSE)
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