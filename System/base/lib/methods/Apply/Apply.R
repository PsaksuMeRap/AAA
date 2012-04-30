

setGeneric("Apply",def=function(x,...) standardGeneric("Apply"))


#setMethod("Apply",
#		signature(x="DirectiveString"),
#		function (x, positions) {
#			
#			directiveString <- unclass(x)
#			if (is.na(directiveString)) return(positions)
#			
#			if (directiveString!="explode:Fondi_misti") stop("directiveString other than explode:Fondi_misti not implemented yet.")
#			
#			# create newPositions
#			isMixedFund <- sapply(positions,is,"PositionFondi_misti")
#			if (any(isMixedFund)) {
#				p1 <- positions[!isMixedFund]
#				p2 <- unlist(lapply(positions[isMixedFund],explode),recursive=FALSE)
#				newPositions <- new("Positions",c(p1,p2))
#				return(newPositions)
#			} else {
#				return(positions)
#			}
#		}
#)


#setMethod("Apply",
#		signature(x="CheckString"),
#		function(x,positions,logFile) {
#			# compute the total value of the positions
#			positionsValue <- sum(positions)		
			
			# parse the checkString
#			checkStringParsed <- parser(x) 
#			directiveString <- checkStringParsed@directiveString
#			constraint <- checkStringParsed@constraint
#	
#			# utilizza la directiveString
#			if (!is.na(directiveString)) positions <- apply(directiveString,positions)
#		
#			extractedPositions <- selector(checkStringParsed@selectionCriteriaList,positions)
#			
#			# If the constraint is relative transform it in an absolute constraint. If the
#			# constraint is absolute compute the corresponding % rate to be used in the summary
#			if (is(constraint,"RelativeConstraint"))  {
#				percentageValue <- constraint@value
#				value <- toMoney(positionsValue@amount*percentageValue/100,positionsValue@currency)
#				constraint <- new("AbsoluteConstraint",value=value,operator=constraint@operator)
#			} else {
#				# compute the percentageValue
#				percentageValue <- (constraint@value / positionsValue) * 100
#			}
#			
#			percentageValue <- paste(formatC(percentageValue,digits=2,format="f"),"%",sep="")
#			
#			extractedPositionsValue <- sum(extractedPositions)
#			
#			fakePosition <- new("Position",id=-1,quantity=1,value=extractedPositionsValue,security=new("Unclassified"))
#			
#			actualPercentage <- (extractedPositionsValue / positionsValue)*100
#			actualPercentage <- paste(formatC(actualPercentage,digits=2,
#							format="f"),"%",sep="")
#			
#			# create a new selectionCriterium to be used by the check function
#			selectionCriterium <- new("AmountSelectionCriterium",constraint=constraint,negation=FALSE)
#			
#			checkResult <- check(fakePosition,selectionCriterium)
#			result <- list()
#			result$checkString <- x
#			result$checkResult <- checkResult
#			result$percentageValue <- percentageValue
#			result$actualPercentage <- actualPercentage
#			
#			if (!missing(logFile)) {
#				
#				cat(paste("check:",checkResult,"->", x),
#						file=logFile,sep="\n",append=TRUE)
#				
#				positionsToBePrinted  <- as.character(extractedPositions)
#				result$positions <- positionsToBePrinted
#				
#				for (p in positionsToBePrinted) {	
#					cat(paste("      ",p), file=logFile, sep="\n",append=TRUE)
#				}
#				
#				cat(paste("Total:",as.character(extractedPositionsValue), "over", 
#								as.character(positionsValue), 
#								"(",actualPercentage,")","\n"),file=logFile,sep="\n",append=TRUE)
#			}
			
#			return( result )
#		}
#)