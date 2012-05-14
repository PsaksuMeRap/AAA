# TODO: Add comment
# 
# Author: Claudio
###############################################################################


file.move <- function(fileName,fromDir,toDir) {
	filePathFrom <- file.path(fromDir,fileName)
	filePathTo <- file.path(toDir,fileName)
	okCopy <- file.copy(from=filePathFrom,to=filePathTo)
	okRemove <- file.remove(filePathFrom)
	return(okCopy & okRemove)
}

