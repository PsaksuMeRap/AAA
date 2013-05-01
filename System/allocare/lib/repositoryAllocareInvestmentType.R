# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_repositoryAllocareInvestmentType <- function(file="./allocare/data/repository_allocare_investment_type.csv") {
	
	repository <- list()
	class(repository) <- "repositoryInstruments"
	
	investmentType.df <- read.table(file,header=TRUE,sep=",",as.is=TRUE)
	
	repository$instruments.df <- investmentType.df
	
	repository$getId <- function(allocareInvestmentType) {
		isDesired <- investmentType.df [,"Allocare_investment_type"] == allocareInvestmentType
		if (any(isDesired)) return(investmentType.df [isDesired,"ID"])
		return(NA_integer_)
	}
	return(repository)
}
