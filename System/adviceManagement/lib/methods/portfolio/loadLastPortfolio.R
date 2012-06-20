# TODO: Add comment
# 
# Author: Claudio
###############################################################################


loadLastPortfolio <- function(directory) {
	# dateTime <- format(Sys.time(),format="%Y-%mpaste("No portfolio files in directory\n",directory,sep="")-%d_%H-%M-%S")

	# get the file names in directory ending with ".RData"
	files <- list.files(path=directory,pattern="portfolio.RData$",full.name=TRUE)
	if (length(files)==0) stop("No portfolios to load!")
	
	# sort the file names in ascending order 
	files <- sort(files)
	
	# load the last one
	load(files[length(files)])
	
	return(portfolio)
}

loadPortfolio <- function(portfolioName="portfolio.RData",directory,portfolioId) {
	# portfolioName: the portfolio file name, default is "portfolio.RData"
	# portfolioID: portfolioId is for example "globalEquity"
	# directory: the directory path without the portfolioId
	
	if (missing(directory)) directory <- file.path(systemOptions[["homeDir"]],"data","portfolios")
	if (!missing(portfolioId)) directory <- file.path(directory,portfolioId)
	
	
	# load the last one
	load(file.path(directory,portfolioName))
	
	return(portfolio)
}