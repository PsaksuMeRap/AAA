# TODO: Add comment
# 
# Author: claudio
###############################################################################


createPositionsData <- function() {
	dati.df <- read.csv("./unitTests/data/datiPosizioni.csv",header=TRUE,stringsAsFactors=FALSE)
	return(dati.df)
}
