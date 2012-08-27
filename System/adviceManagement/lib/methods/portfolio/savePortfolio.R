# TODO: Add comment
# 
# Author: Claudio
###############################################################################


savePortfolio <- function(portfolio,portfolioSubDirectory="",directory=file.path(sys[["homeDir"]],"data","portfolios")) {
	
	if (portfolioSubDirectory!="") directory <- file.path(directory,portfolioSubDirectory)
	#oldFileFullName <- file.path(directory,portfolioSubDirectory,"portfolio.RData")
	#fileExists <- file.exists(oldFileFullName)
	
	#if (fileExists) {
	#	dateTime <- format(Sys.time(),format="%Y-%m-%d_%H-%M-%S")
	#	file.rename(from=oldFileFullName,to=file.path(directory,portfolioSubDirectory,paste(dateTime,"portfolio",sep="_")))
		
	#}
	
	saveLastObject(object=portfolio,fileName="portfolio.RData",directory=directory)
	
}

saveTestPortfolio <- function(portfolio,portfolioSubDirectory) {
	directory <- file.path(sys[["sourceCodeDir"]],"adviceManagement","unitTests","files","riskman","data","portfolios")
	
	savePortfolio(portfolio,portfolioSubDirectory,directory=directory)
}
