# TODO: Add comment
# 
# Author: claudio
###############################################################################

setwd("/home/claudio/workspace/AAA/datiFuturiVix")
dati <- read.csv("vix-strategy.csv")
dati[[1]] <- as.POSIXlt(dati[[1]])

a <- as.Date(dati[,1]) == as.Date("2011-4-14")
data <- dati[a,1]
plot(data,dati[a,"LAST_PRICE"],xlab="Time",ylab="VIX")


ok <- function(data) {
	a <- as.Date(dati[,1]) == as.Date(data)
	dat <- dati[a,1]
	plot(dat,dati[a,"LAST_PRICE"],xlab="Time",ylab="VIX",main=data)
}


for (data in unique(as.character(as.Date(dati[[1]])))) {
	pdf(file=paste(data,"pdf",sep="."))
	ok(data)
	dev.off()
}