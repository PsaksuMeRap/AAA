# TODO: Add comment
# 
# Author: claudio
###############################################################################


plot.dsTimeseries <- function(x,from) {
	if (missing(from)) {		
		plot(as.Date(rownames(x$data)),x$data[,1],xlab=x$name,type="l",ylab="")
	} else {
		use <- as.Date(rownames(x$data)) >= as.Date(from)
		dates <- as.Date(rownames(x$data))[use]
		y <- x$data[use,1]
		plot(dates,y,xlab=x$name,type="l",ylab="")
	}
	
}

addToPlot <- function(x) {
	# x: a vixFuture		
	lines(as.Date(rownames(x$data)),x$data[,1],col="red")
}
