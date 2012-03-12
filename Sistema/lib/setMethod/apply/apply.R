
setMethod("apply",
		signature(X = "DirectiveString",MARGIN="missing",FUN="missing"),
		function (X, positions) {
			
			directiveString <- unclass(X)
			if (is.na(directiveString)) return(positions)
			
			if (directiveString!="explode:Fondi_misti") stop("directiveString other than explode:Fondi_misti not implemented yet.")
			
			# create newPositions
			isMixedFund <- sapply(positions,is,"PositionFondi_misti")
			if (any(isMixedFund)) {
				p1 <- positions[!isMixedFund]
				p2 <- unlist(lapply(positions[isMixedFund],explode),recursive=FALSE)
				newPositions <- new("Positions",c(p1,p2))
				return(newPositions)
			} else {
				return(positions)
			}
		}
)


setMethod("apply",
		signature(X = "CheckString",MARGIN="missing",FUN="missing"),
		function(checkString,positions,logFile,refCurrency) {
			
			# compute the total value of the positions
			positionsValue <- sum(positions)		
			if (!missing(refCurrency)) { 
				positionsValue <- repositories@exchangeRate@exchange(positionsValue,refCurrency)
			}
			
			# parse the checkString
			checkStringParsed <- parser(X) 
			directiveString <- checkStringParsed@directiveString
			
			# utilizza la directiveString
			if (!is.na(directiveString)) positions <- apply(checkStringParsed@directiveString,positions)
			
			extractedPositions <- extractPositionsFromSelectionString(parsed[["selectionString"]],positions)
			
			# crea il criterio di selezione per la verifica del vincolo finale
			criteriumSelection <- create_criteriumSelection(factor="amount",
					criteriumCheck=parsed[["criteriumCheck"]]
			)
			
			# crea il valore assoluto da verificare se il check è relativo mentre se il tipo
			# di vincolo è assoluto crea il valore limite percentuale da stampare assieme 
			# a quello effettivo nel summary
			if (criteriumSelection$criteriumCheck$kind=="relative") {
				percentageValue <- criteriumSelection$criteriumCheck$value/100
				criteriumSelection$criteriumCheck$value <- toMoney(percentageValue*positionsValue$amount,positionsValue$currency)
			} else {
				# in questo caso criteriumSelection$criteriumCheck$value è una variabile di tipo money
				percentageValue <- criteriumSelection$criteriumCheck$value$divide(positionsValue)
			}
			percentageValue <- paste(formatC(percentageValue*100,digits=2,
							format="f"),"%",sep="")
			
			
			if (missing(refCurrency)) extractedPositionsValue <- extractedPositions$sum() else extractedPositionsValue <- extractedPositions$sum(refCurrency)
			
			fakePosition <- create_position()
			fakePosition$create(name="fake",currency=extractedPositionsValue$currency,
					amount=extractedPositionsValue$amount) 
			
			actualPercentage <- extractedPositionsValue$divide(positionsValue)*100
			actualPercentage <- paste(formatC(actualPercentage,digits=2,
							format="f"),"%",sep="")
			
			checkResult <- check(fakePosition,criteriumSelection)
			result <- list()
			result$checkString <- checkString
			result$checkResult <- checkResult
			result$percentageValue <- percentageValue
			result$actualPercentage <- actualPercentage
			
			if (!missing(logFile)) {
				
				cat(paste("check:",checkResult,"->", checkString),
						file=logFile,sep="\n",append=TRUE)
				
				positionsToBePrinted  <- extractedPositions$toString()
				result$positions <- positionsToBePrinted
				
				for (p in positionsToBePrinted) {	
					cat(paste("      ",p), file=logFile, sep="\n",append=TRUE)
				}
				
				cat(paste("Total:",extractedPositionsValue$toString(), "over", 
								positionsValue$toString(), 
								"(",actualPercentage,")","\n"),file=logFile,sep="\n",append=TRUE)
			}
			
			# return( checkResult )
			return( result )
		}
)