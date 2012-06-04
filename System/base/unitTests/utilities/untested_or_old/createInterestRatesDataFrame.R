# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquityDataFrame <- function() {
	interestRates.df <- read.csv(file="./base/unitTests/data/repositoryInterestRates.csv",
			header=TRUE,stringsAsFactors=FALSE)
	
	return(interestRates.df)
	
}
