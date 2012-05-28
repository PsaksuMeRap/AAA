# TODO: Add comment
# 
# Author: Claudio
###############################################################################


loadLastPortfolio <- function(directory) {
	# dateTime <- format(Sys.time(),format="%Y-%mpaste("No portfolio files in directory\n",directory,sep="")-%d_%H-%M-%S")

	# get the file names in directory ending with ".RData"
	files <- list.files(path=directory,pattern="portfolio.RData$",full.name=TRUE)
	if (length(files)==0) stop()
	
	# sort the file names in ascending order 
	files <- sort(files)
	
	# load the last one
	load(files[length(files)])
	
	return(portfolio)
}

