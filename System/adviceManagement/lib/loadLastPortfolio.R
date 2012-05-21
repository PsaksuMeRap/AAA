# TODO: Add comment
# 
# Author: Claudio
###############################################################################


loadLastPortfolio <- function(directory) {
	# dateTime <- format(Sys.time(),format="%Y-%m-%d_%H-%M-%S")
	
	# get the file names in directory ending with ".RData"
	files <- list.files(path=directory,pattern="portfolio.RData$")
	if (length(files)==0) stop(paste("No portfolio files in directory\n",directory,sep=""))
	
	# sort the file names in ascending order 
	files <- sort(files)
	
	# load the last one
	load(files[length(files)])
	
	return(portfolio)
}

