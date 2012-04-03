# TODO: Add comment
# 
# Author: claudio
###############################################################################


create_dsTimeseries <- function(name=NA_character_,
				dsCode=NA_character_, data=NA) {
	timeseries <- new.env()
	class(timeseries) <- "dsTimeseries"
	
	timeseries$name <- name
	timeseries$dsCode <- dsCode
	timeseries$data <- data
	
	timeseries$getStartDate <- function() {
		return(rownames(timeseries$data)[1])
	}
	timeseries$getEndDate <- function() {
		dates <- rownames(timeseries$data)
		nbObs <- length(dates)
		return(dates[nbObs])
	}
	
	return(timeseries)

}

plot.dsTimeseries <- function(x) {
	plot(as.Date(rownames(x$data)),x$data[,1],xlab=x$name,type="l",ylab="")
}

