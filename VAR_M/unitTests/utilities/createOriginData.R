# TODO: Add comment
# 
# Author: claudio
###############################################################################


createOriginData <- function() {
	getRow <- function(i,df) return(df[i,,drop=TRUE])
	
	dati.df <- read.csv("./unitTests/data/origin.csv",
			header=TRUE,stringsAsFactors=FALSE)
	origin <- lapply(1:nrow(dati.df),getRow,dati.df)
	return(origin)
}
