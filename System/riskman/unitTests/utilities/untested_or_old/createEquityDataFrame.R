# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquityDataFrame <- function() {
	equities.df <- read.csv(file="./base/unitTests/data/repositoryEquities.csv",
			header=TRUE,stringsAsFactors=FALSE)
	
	return(equities.df)
	
}
