# TODO: Add comment
# 
# Author: Claudio
###############################################################################



tradesFactory <- function(messageFileName,directory) {
	fullFileName <- file.path(directory,messageFileName[["fileName"]])
	csvTrades <- read.csv(fullFileName,as.is=TRUE)
	
	nbRows <- dim(csvTrades)[[1]]
	if (nbRows==0) return(new("Trades")) 
	trades.l <- lapply(1:nbRows,function(i,csvTrades){return(as.list(csvTrades[i,]))},
			csvTrades)
	return(new("Trades",lapply(trades.l,tradeFactory)))
}