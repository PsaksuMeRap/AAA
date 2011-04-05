# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_timeSeries <- function(name,data=NA,type=NA,freq="dailyNoWeekend") {
	
	
	timeSeries <- new.env()
	class(timeSeries) <- c("timeSeries")
	
	timeSeries$name <- name # string
	timeSeries$data <- data # labeled vector
	timeSeries$type <- type # description field, optional
	timeSeries$freq <- freq # frequency
	
	return(timeSeries)
}

