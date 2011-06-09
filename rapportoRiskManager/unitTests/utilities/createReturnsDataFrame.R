# TODO: Add comment
# 
# Author: claudio
###############################################################################


createReturnsDataFrame <- function() {
	equities.df <- read.csv(file="./unitTests/data/DBPortfolioGenerale.csv",
			header=TRUE)
	
	return(equities.df)	
}
