# TODO: Add comment
# 
# Author: claudio
###############################################################################


createDBEquitiesDataFrame <- function() {
	DBEquities.df <- read.csv(file="./base/unitTests/data/repositoryDBEquities.csv",
			header=TRUE,stringsAsFactors=FALSE)
	return(DBEquities.df)
}
