# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_archive <- function(rootDir) {
	
	archiveDirectory <- file.path(rootDir,"archive")
	if (!file.exists(archiveDirectory)) {
		isOk <- dir.create(file.path(rootDir,"archive","deleted"),recursive=TRUE)
		isOk <- dir.create(file.path(rootDir,"archive","processed","accepted"),recursive=TRUE)
		isOk <- dir.create(file.path(rootDir,"archive","processed","rejected"),recursive=TRUE)
		return()
	}
	
	deleteDirectory <- file.path(rootDir,"archive","deleted")
	if (!file.exists(deleteDirectory)) {
		isOk <- dir.create(deleteDirectory)
	}
	
	processedDirectory <- file.path(rootDir,"archive","processed")
	if (!file.exists(processedDirectory)) {
		isOk <- dir.create(file.path(rootDir,"archive","processed","accepted"),recursive=TRUE)
		isOk <- dir.create(file.path(rootDir,"archive","processed","rejected"),recursive=TRUE)	
		return()
	}
	
	acceptedDirectory <- file.path(rootDir,"archive","processed","accepted")
	if (!file.exists(acceptedDirectory)) {
		isOk <- dir.create(acceptedDirectory)
	}
	
	rejectedDirectory <- file.path(rootDir,"archive","processed","rejected")
	if (!file.exists(rejectedDirectory)) {
		isOk <- dir.create(rejectedDirectory)
	}
	
	return()
}

