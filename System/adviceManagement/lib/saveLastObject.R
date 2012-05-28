# TODO: Add comment
# 
# Author: claudio
###############################################################################


saveLastObject <- function(object,fileName,directory) {
	# This procedure moves the actual fileName.RData file in directory to
	# fileName_yyyy-mm-dd_hh-mm-ss.RData and saves the Object in
	# a new fileName.RData file
	
	# verify if an object "fileName" still exists in the directory
	fileExists <- file.exists(file.path(directory,fileName))
	dateTime <- format(Sys.time(),format="%Y-%m-%d_%H-%M-%S")
	if (fileExists) file.rename(from=file.path(directory,fileName),
				to=file.path(directory,paste(dateTime,"_",fileName,sep="")))
	
	save(object,file=file.path(directory,fileName))
	
}
