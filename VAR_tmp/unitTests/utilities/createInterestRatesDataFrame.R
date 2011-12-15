# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquityDataFrame <- function() {
	interestRates.df <- read.csv(file="./unitTests/data/repositoryInterestRates.csv",
			header=TRUE,stringsAsFactors=FALSE)
	
	return(interestRates.df)
	
}
