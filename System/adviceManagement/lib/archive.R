# TODO: Add comment
# 
# Author: ortellic
###############################################################################


create_archive <- function(rootDir) {
	isOk <- dir.create(file.path(rootDir,"archive","deleted"),recursive=TRUE)
	isOk <- dir.create(file.path(rootDir,"archive","processed","accepted"),recursive=TRUE)
	isOk <- dir.create(file.path(rootDir,"archive","processed","rejected"),recursive=TRUE)	
	
}

