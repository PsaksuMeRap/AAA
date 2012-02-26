# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("TestSuite",representation(configLines="character",checkStrings="character"))

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
		
		return(new("TestSuite",configLines=unlist(configLines),checkStrings=checkStrings))
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


