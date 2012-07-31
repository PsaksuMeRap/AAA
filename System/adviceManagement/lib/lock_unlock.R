# TODO: Add comment
# 
# Author: Claudio
###############################################################################



lock <- function(message) {
	file <- file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]],"lock")
	ok <- file.create(file)
	return(ok)
}


unlock <- function(message) {
	file <- file.path(sys[["homeDir"]],"postOffice",message[["portfolioName"]],"lock")
	ok <- file.remove(file)
	return(ok)
}
