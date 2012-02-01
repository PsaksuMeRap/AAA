# TODO: Add comment
# 
# Author: claudio
###############################################################################


createExchangeRatesTestRepository <- function() {
	source("./unitTests/utilities/createExchangeRatesVector.R")
	
	rates <- createExchangeRatesVector()
	repositoryExchangeRates <- create_repositoryExchangeRates(rates)
	return(repositoryExchangeRates)
}