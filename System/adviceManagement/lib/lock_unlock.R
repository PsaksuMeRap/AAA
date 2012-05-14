# TODO: Add comment
# 
# Author: Claudio
###############################################################################



lock <- function(message) {
	file <- file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"lock")
	ok <- file.create(file)
	return(ok)
}


unlock <- function(message) {
	file <- file.path(systemOptions[["homeDir"]],"postOffice",message@advisor@folderName,"lock")
	ok <- file.remove(file)
	return(ok)
}
