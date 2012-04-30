# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldCreateSniffer <- function() {
	
	setClass("DirectorySniffer",representation(absoluteDirectoryPath="character",
					newFileNames="character",oldFileNames="character"))
	
	
	directorySniffer <- new("DirectorySniffer",absoluteDirectoryPath="/home/claudio/workspace/AAA/System/directorySniffer/unitTests/inputDirectory/")
	
	directory <- directorySniffer@absoluteDirectoryPath
	
	files <- dir(path=directorySniffer@directory)
	
	if (length(files>0)) {
	
		for (fileName in files) {
			modificationTime <- file.info(paste(directory,fileName,sep=""))[1,"mtime"]
			
		}
	}

}
