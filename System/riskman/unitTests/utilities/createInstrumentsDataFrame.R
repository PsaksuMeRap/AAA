# TODO: Add comment
# 
# Author: claudio
###############################################################################


createInstrumentsDataFrame <- function() {
	instruments.df <- read.csv(file="./base/unitTests/data/repositoryInstruments.csv",
			header=TRUE,stringsAsFactors=FALSE)
	colnames(instruments.df) <- c("ID","Instrument")
	return(instruments.df)
}
