# TODO: Add comment
# 
# Author: claudio
###############################################################################


createExchangeRatesTestRepository <- function() {
	source("./base/unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repositoryExchangeRates <- create_testRepositoryExchangeRates(rates)
	return(repositoryExchangeRates)
}