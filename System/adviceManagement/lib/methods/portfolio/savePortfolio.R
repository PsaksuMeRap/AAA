# TODO: Add comment
# 
# Author: Claudio
###############################################################################


savePortfolio <- function(portfolio,portfolioName,directory=file.path(sys[["homeDir"]],"data","portfolios")) {
	
	oldFileFullName <- file.path(directory,portfolioName,"portfolio.RData")
	fileExists <- file.exists(oldFileFullName)
	
	if (fileExists) {
		dateTime <- format(Sys.time(),format="%Y-%m-%d_%H-%M-%S")
		file.rename(from=oldFileFullName,to=file.path(directory,portfolioName,paste(dateTime,"portfolio",sep="_")))
		
	}
	
	saveLastObject(object=portfolio,fileName="portfolio.RData",directory=file.path(directory,portfolioName))
	
}

saveTestPortfolio <- function(portfolio,portfolioName) {
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data","portfolios")
	
	save_portfolio(portfolio,portfolioName,directory=directory)
}


