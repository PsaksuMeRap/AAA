# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_timeSeries <- function(name,data=NA,kind=NA) {
	
	
	timeSeries <- new.env()
	class(timeSeries) <- c("timeSeries")
	
	timeSeries$name <- name # string
	timeSeries$data <- data # labeled vector
	timeSeries$kind <- kind # description field, optional
	
	return(timeSeries)
}

