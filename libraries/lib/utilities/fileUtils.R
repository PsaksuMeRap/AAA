# TODO: Add comment
# 
# Author: claudio
###############################################################################



isDirectory <- function(filePath) {
	isdir <- file.info(filePath)[1,"isdir"]
}

listOfFiles <- function() {
	from <- list.files()
	fileNames <- from[!sapply(from,isDirectory)]
}

