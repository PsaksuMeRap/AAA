# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateParser <- function() {
	fileName <- "./unitTests/data/testImportTestSuite.txt"

		parser <- new.env()
		class(parser) <- "parserTestSuite"

		parser$identifyComment <- function(line) {
			return(substr(strings,1,1) == "#")
		}
		
		parser$importFile <- function(fileName) {
			strings <- scan(file=fileName,
					what="character",sep="\n")
			
			# remove comments
			isComment <- sapply(strings,parser$identifyComment)
			strings <- strings[!isComment]
			
			# identify the start of the checkList
			startLineNb <- (1:length(strings))[substr(strings,1,1) == "checkListStart"]
			endLineNb <- (1:length(strings))[substr(strings,1,1) == "checkListEnd"]
			lineNb
			parser$strings <<- strings
		}
		
		parser$parseLine <- function(line) {
			x <- strsplit(line,":")	
		}
		
		parser$importFile(fileName)
		
		checkEquals(class(parser),"parserTestSuite")
		checkEquals(parser$identifyComment("#aa"),TRUE)
		checkEquals(parser$strings[1],"testSuiteName: test importazione di una testSuite")
			
}
