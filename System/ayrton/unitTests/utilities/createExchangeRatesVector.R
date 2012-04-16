# TODO: Add comment
# 
# Author: claudio
###############################################################################


createExchangeRatesVector <- function() {
	rates.df <- read.csv(file="./unitTests/data/repositoryExchangeRates.csv",
			header=TRUE,stringsAsFactors=FALSE)
	rates <- rates.df[,"CHFPar"]
	names(rates) <- rates.df[,"Moneta"]
	
	return(rates)
}
