# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_parserSelectionCriteria <- function() {
	
	parser <- new.env()
	class(parser) <- "parserSelectionCriteria"
	# checkString = factorString & factorString & ... 
	#			  + factorString & factorString & ...
	#			  + factorString & factorString & ...
	#			  ; constraintString
	#             :: directive string
	
	parser$splitCheckString <- function(checkString) {
		#checkString: a string like "instrument:equity & currency:CHF + instrument:bond ; <=3% :: explode:Fondo_misto"
		# i.e. the union of a selectionString, a constraintString and a directive string, separated by a 
		# a semicolon and ::, respectively
		# the selectionString is mandatory, the constraintString and directive string are optional
		
		firstSplit <- unlist(strsplit(checkString,";"))
		
		if (length(firstSplit)==0) stop("Error: empty checkString")
		firstSplit <- removeStartEndSpaces(firstSplit)
		
		if (length(firstSplit)==1)  {
			criteriumCheck <- NA
			secondSplit <- unlist(strsplit(firstSplit[[1]],"::"))
			secondSplit <- removeStartEndSpaces(secondSplit)
			if (length(secondSplit)==1)  {
				directiveString <- NA
				selectionString <- secondSplit[[1]]
			} else {
				directiveString <- secondSplit[[2]]
				selectionString <- secondSplit[[1]]				
			}	
		} else {
			selectionString <- firstSplit[[1]]
		
			secondSplit <- unlist(strsplit(firstSplit[[2]],"::"))
			secondSplit <- removeStartEndSpaces(secondSplit)
		
			if (length(secondSplit)==1)  {
				directiveString <- NA
				criteriumCheck <- parser$constructCriteriumCheck(secondSplit[[1]])
			} else {
				directiveString <- secondSplit[[2]]
				criteriumCheck <- parser$constructCriteriumCheck(secondSplit[[1]])				
			}				
		}
		
		x <- list(selectionString=selectionString,criteriumCheck=criteriumCheck,
				directiveString=directiveString)		
		return(x)
	}
	
	parser$splitSelectionString <- function(selectionString) {
		# selectionString: a string like "instrument:equity & currency:CHF + instrument:bond"
		# where factorsStrings are joined by the "+" character
		result <- unlist(strsplit(selectionString,"\\+"))
		result <- removeStartEndSpaces(result)
		result <- lapply(result,parser$splitFactorsString)
		return(result)
	}
	
	parser$splitFactorsString <- function(factorsString) {
		# factorsString: a string like "instrument:equity & currency:CHF"
		# where one or more factorString are concatenated with the "&" character
	    result <- unlist(strsplit(factorsString,"\\&"))
		result <- removeStartEndSpaces(result)
		
		result <- lapply(result,parser$splitFactorString)
		return(result)
	}
	
	parser$splitFactorString <- function(factorString) {
		# factorString: a string like "instrument:equity,bond" or
		# "amount:<= 50 USD" or the negation form "instrument!: equity, bond"
		result <- unlist(strsplit(factorString,":"))
		if (length(result)<2) stop(paste("Error: splitFactorString should return 2 blocks.",
							factorString))
		result <- removeStartEndSpaces(result)

		# check if negation is required
		factor <- result[1]
		nbChar <- nchar(factor)
		if (substr(factor,nbChar,nbChar)=="!") {
			factor <- removeStartEndSpaces(substr(factor,1,nbChar-1))
			negation = TRUE
		} else {
			negation = FALSE
		}
		rm(nbChar)
		
		values <- removeStartEndSpaces(unlist(strsplit(result[2],",")))
		
		if (factor=="amount") {
			criteriumCheck <- parser$constructCriteriumCheck(values)
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values,
					criteriumCheck=criteriumCheck
			)
		} else {
			criteriumSelection <- create_criteriumSelection(
					factor=factor,
					values=values,
					negation=negation
			)		
		}

		return(criteriumSelection)
	}
	
	parser$constructCriteriumCheck <- function(constraintString) {
		# a constraintString with the quantitative constraint to identify
		# "=0USD" or "> 1.77 EUR" or < 5% 
	
		string <- removeStartEndSpaces(constraintString)
		
		# check for operator
		start = 1
		if (grepl("^>=|<=|!=", string)) {
			stop=2
		} else {
			if (grepl("^>|<|=", string)) {
				stop=1
			} else {
				stop(paste("Error: missing operator:",string))
			}
		}
		operator <- substr(string,start,stop)
		
		string <- substr(string,stop+1,nchar(string))
		string <- removeStartEndSpaces(string)
		
		# check for currency
		if (grepl("[A-Z]{3}$", string)) {
			stop  = nchar(string)
			start = stop - 2
			
			kind = "absolute"
			currency <- substr(string,start,stop)
			string <- substr(string,1,start-1)
			amount <- as.numeric(string)
			money <- toMoney(amount,currency)
			
			return(create_criteriumCheck (operator,value=money,kind))
		} else {
			# check for %
			if (grepl("%$", string)) {
				kind = "relative"
				string <- substr(string,1,nchar(string)-1)
				amount <- as.numeric(string)
				return(create_criteriumCheck (operator=operator
								,value=amount,kind=kind))
			} else {
				stop("Error: missing currency or % identifier")
			}
		}
	}
	
	return(parser)	
}


