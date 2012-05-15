# TODO: Add comment
# 
# Author: Claudio
###############################################################################



tradesFactory <- function(fileName,directory) {
	file <- file.path(directory,fileName)
	csvTrades <- read.csv(file)
	
	if (nrow(csvTrades)==0) return(new("Trades")) 
	return(new("Trades",apply(csvTrades,1,tradeFactory)))
}