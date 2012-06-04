# TODO: Add comment
# 
# Author: claudio
###############################################################################


createOriginData <- function() {
	dati.df <- read.csv("./base/unitTests/data/origin.csv",header=TRUE,stringsAsFactors=FALSE)
	return(dati.df)
}
