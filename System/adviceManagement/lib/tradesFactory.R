# TODO: Add comment
# 
# Author: Claudio
###############################################################################



tradesFactory <- function(fileName,directory) {
	file <- file.path(directory,fileName)
	csvTrades <- read.csv(file,as.is=TRUE)
	
	nbRows <- dim(csvTrades)[[1]]
	if (nbRows==0) return(new("Trades")) 
	trades.l <- lapply(1:nbRows,function(i,csvTrades){return(as.list(csvTrades[i,]))},
			csvTrades)
	return(new("Trades",lapply(trades.l,tradeFactory)))
}