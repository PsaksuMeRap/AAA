# TODO: Add comment
# 
# Author: Claudio
###############################################################################

loadPortfolio <- function(portfolioName="portfolio.RData",directory,portfolioId) {
	# portfolioName: the portfolio file name, default is "portfolio.RData"
	# portfolioID: portfolioId is for example "globalEquity"
	# directory: the directory path without the portfolioId
	
	if (missing(directory)) directory <- file.path(sys[["homeDir"]],"data","portfolios")
	if (!missing(portfolioId)) directory <- file.path(directory,portfolioId)
	
	tmpEnv <- new.env()

	# load the portfolio
	load(file.path(directory,portfolioName),envir=tmpEnv)
	
	return(tmpEnv$object)
}

