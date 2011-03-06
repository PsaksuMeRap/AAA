# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_dsTimeseries <- function() {
	data <- data.frame(pippo123=c(1.2,1.3,1.4))
	myRowNames <- c("2010-01-01","2010-01-02","2010-01-03")
	rownames(data) <- myRowNames
	dsTimeseries <- create_dsTimeseries(name="Serie di test",
			dsCode="pippo123", data=data)
	
	checkEquals(class(dsTimeseries),"dsTimeseries")
	checkEquals(dsTimeseries$name,"Serie di test")
	checkEquals(dsTimeseries$dsCode,"pippo123")
	checkEquals(rownames(dsTimeseries$data),myRowNames)
	checkEquals(dsTimeseries$data[2,1],1.3)
	
}


test.getStart_EndDate <- function() {
	data <- data.frame(pippo123=c(1.2,1.3,1.4))
	myRowNames <- c("2010-01-01","2010-01-02","2010-01-03")
	rownames(data) <- myRowNames
	dsTimeseries <- create_dsTimeseries(name="Serie di test",
			dsCode="pippo123", data=data)
	
	checkEquals(dsTimeseries$getStartDate(),"2010-01-01")
	checkEquals(dsTimeseries$getEndDate(),"2010-01-03")
	
} 