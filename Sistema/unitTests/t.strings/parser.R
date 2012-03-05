# TODO: Add comment
# 
# Author: claudio
###############################################################################

source("./lib/portfolio/portfolio.R")
source("./lib/position/position.R")



create_parserSelectionCriteria <- function() {
	
	parser <- new.env()
	class(parser) <- "parserSelectionCriteria"
	# checkString = factorString & factorString & ... 
	#			  + factorString & factorString & ...
	#			  + factorString & factorString & ...
	#			  ; constraintString
	
	
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


create_parserTestSuite <- function() {
	parser <- new.env()
	class(parser) <- "parserTestSuite"
	
	parser$strings <- NA
	
	parser$identifyComment <- function(line) {
		line <- removeStartEndSpaces(line)
		return(substr(line,1,1) == "#")
	}
	
	parser$identifyEmptyLine <- function(line) {
		line <- removeStartEndSpaces(line)
		return(line == "")
	}	
	
	parser$importFile <- function(fileName) {
		strings <- scan(file=fileName,quiet=TRUE,
				what="character",sep="\n")
		strings <- removeStartEndSpaces(strings)
		parser$strings <<- strings
	}
	
	parser$parseStrings <- function() {
		# remove comments
		isComment <- sapply(parser$strings,parser$identifyComment)
		strings <- parser$strings[!isComment]
		
		# remove empty lines
		isEmpty <- sapply(strings,parser$identifyEmptyLine)
		strings <- strings[!isEmpty]	
		
		# count the number of lines
		lineNumber <- length(strings)
		
		# identify the start end the end of the checkList
		startLineNb <- parser$lineCheckListStart(strings)
		endLineNb <- parser$lineCheckListEnd(strings)
		
		# extract the checkStrings
		linesToExtract <- startLineNb:endLineNb
		checkStrings <- strings[linesToExtract]
		checkStrings <- checkStrings[-c(1,length(checkStrings))]
		configLines <- strings[-linesToExtract]
		
		configLines <- lapply(configLines,parser$parseConfigLine)

		return(list(configLines=unlist(configLines),checkStrings=checkStrings))
	}
	
	parser$lineCheckListStart <- function(strings) {
		startLineNb <- (1:length(strings))[grepl("^checkListStart",strings)]
		return(startLineNb)
	}
	
	parser$lineCheckListEnd <- function(strings) {
		endLineNb <- (1:length(strings))[grepl("^checkListEnd",strings)]
		return(endLineNb)
	}
	
	parser$parseConfigLine <- function(configLine) {
		x <- strsplit(configLine,":")[[1]]
		value <- removeStartEndSpaces(x[2])
		names(value) <- x[1]
		return(value)
	}
	
	return(parser)
} 


removeStartEndSpaces <- function(string) {
	result <- sub("^[[:blank:]]+", "", string)
	result <- sub("[[:blank:]]+$", "", result)
	return(result)
}

