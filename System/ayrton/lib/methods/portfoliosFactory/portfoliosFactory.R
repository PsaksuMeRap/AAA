# TODO: Add comment
# 
# Author: claudio
###############################################################################

setMethod("portfoliosFactory",signature(positions="AyrtonPositions"),
		function(positions,politicaInvestimento.df) {
			
			if (missing(politicaInvestimento.df)) {
				politicaInvestimento.df <- repositories$politicaInvestimento$politicaInvestimento.df				
				
				if (nrow(politicaInvestimento.df)==0) {
					message <- "Impossibile determinare la moneta di investimento. La tabella DBPoliticaInvestimento è vuota."
					message <- paste(message,"Utilizzata moneta di default (CHF) per tutti i portafogli.",sep="\n")
					tkmessageBox(message=message,icon="error",type="ok")
					useDefRefCur <- TRUE
				} else {
					useDefRefCur <- FALSE
				}
				
			} else {
				useDefRefCur <- FALSE
			}
			
			identifyReferenceCurrency <- function(owner) {
				isDesiredOwner <- politicaInvestimento.df[,"ID"] == owner
				if (sum(isDesiredOwner)>0) {
					stringCurrency <- unique(politicaInvestimento.df[isDesiredOwner,"MonetaInvestimento"])
					if (length(stringCurrency)>1) {
						message <- paste(owner,"has multiple reference currencies.",stringCurrency[1],"selected")
						tkmessageBox(message=message,icon="error",type="ok")
					}
					refCurrency <- new("Currency",stringCurrency[1])
				} else {
					message <- paste("Impossibile determinare la moneta di investimento di '",owner,"'",sep="")
					message <- paste(message,"Utilizzata moneta di default (CHF).",sep="\n")
					tkmessageBox(message=message,icon="error",type="ok")
					refCurrency <- new("Currency","CHF")
				}
				return(refCurrency)
			}
			
			owners <- sapply(positions,function(x) return(x@Cliente))
			positionsByOwner <- split(positions,owners)
			uniqueOwners <- unique(owners)
			
			createPortfolio <- function(owner,positionsByOwner) {
				if (useDefRefCur) {
					refCurrency <- new("Currency","CHF")
				} else {
					refCurrency <- identifyReferenceCurrency(owner)
				}
				
				return(portfolioFactory(positions=positionsByOwner[[owner]],
								owner=owner,referenceCurrency=refCurrency)
				)
			}
			portfolios <- new("Portfolios",lapply(uniqueOwners,createPortfolio,positionsByOwner))
			return(portfolios)
		}
)
