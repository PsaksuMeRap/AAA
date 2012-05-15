# TODO: Add comment
# 
# Author: Claudio
###############################################################################


test.shouldCrateFirstFile <- function() {
	createFirstFile <- function(whereDir) {
		
	}
	
	whereDir <- file.path(systemOptions[["homeDir"]])
	con <- file(des=file.path(whereDir,".Last"))
	cat(".First <- function(){",file=con,append=TRUE)
	cat(paste("",file=con,append=TRUE))
	cat("}",file=con,append=TRUE)
	
}
systemOptions[["homeDir"]]