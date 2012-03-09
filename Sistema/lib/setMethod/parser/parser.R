# TODO: Add comment
# 
# Author: claudio
###############################################################################


setGeneric("parser",def=function(x,...) standardGeneric("parser"))


setMethod("parser",signature(x="CheckString"),
		function(x){
			# split the checkString in the 3 components
			checkString.l <- split(x)
			
			# split the selectionString in multiple factorStrings
			factorStrings.l <- split(checkString.l$selectionString)
			selectionCriteriaList <- new("selectionCriteriaList",lapply(factorStrings.l,toSelectionCriteria))
			
			# construct the parser
			constraint <- constraintFactory(checkString.l$constraintString)
			directiveString <- checkString.l$directiveString
			
		... arrivato qui
			setClass("CheckStringParsed",representation(selectionCriteriaList="SelectionCriteriaList",
							constraint="Constraint",directiveString="DirectiveString"))
			
			
			
		}
)


setMethod("parser",signature(x="TestSuite"),
		function(x) {
			
			f <- setParserTestsuiteUtils()
			
			file <- paste(x@path,x@fileName,sep=.Platform$file.sep)
			strings <- scan(file=file,quiet=TRUE,
					what="character",sep="\n")
			strings <- removeStartEndSpaces(strings)
			
			# remove comments
			isComment <- sapply(strings,f$identifyComment)
			strings <- strings[!isComment]
			
			# remove empty lines
			isEmpty <- sapply(strings,f$identifyEmptyLine)
			strings <- strings[!isEmpty]	
			
			# count the number of lines
			lineNumber <- length(strings)
			
			# identify the start end the end of the checkList
			startLineNb <- f$lineCheckListStart(strings)
			endLineNb <- f$lineCheckListEnd(strings)
			
			# extract the checkStrings
			linesToExtract <- startLineNb:endLineNb
			checkStrings <- strings[linesToExtract]
			checkStrings <- checkStrings[-c(1,length(checkStrings))]
			configLines <- strings[-linesToExtract]
			
			configLines <- lapply(configLines,f$parseConfigLine)

			return(new("ParsedTestSuite",x,configLines=unlist(configLines),checkStrings=checkStrings))

		}
)

setParserTestsuiteUtils <- function() {
	f <- list()
	f$identifyComment <- function(line) {
		line <- removeStartEndSpaces(line)
		return(substr(line,1,1) == "#")
	}
	
	f$identifyEmptyLine <- function(line) {
		line <- removeStartEndSpaces(line)
		return(line == "")
	}	
	
	f$lineCheckListStart <- function(strings) {
		startLineNb <- (1:length(strings))[grepl("^checkListStart",strings)]
		return(startLineNb)
	}
	
	f$lineCheckListEnd <- function(strings) {
		endLineNb <- (1:length(strings))[grepl("^checkListEnd",strings)]
		return(endLineNb)
	}
	
	f$parseConfigLine <- function(configLine) {
		x <- strsplit(configLine,":")[[1]]
		value <- removeStartEndSpaces(x[2])
		names(value) <- x[1]
		return(value)
	}
	return(f)
}