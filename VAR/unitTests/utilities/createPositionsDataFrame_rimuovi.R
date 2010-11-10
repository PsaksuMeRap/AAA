# TODO: Add comment
# 
# Author: claudio
###############################################################################


createOriginData <- function() {
	dati.df <- read.csv("./unitTests/data/datiPosizioni.csv",header=TRUE,stringsAsFactors=FALSE)
	return(dati.df)
}
