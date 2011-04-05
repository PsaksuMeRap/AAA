# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_timeSeries <- function() {
	
	name = "nome"
	data = data.frame(date=c("2010-01-02","2010-01-03"),value=c(100,100.10))
	kind = NA
	
	timeSeries <- create_timeSeries(name=name,data=data)
	
	checkEquals(class(timeSeries),"timeSeries")
	checkEquals(timeSeries$name, "nome")
	checkEquals(timeSeries$data,data)
	checkEquals(timeSeries$type,NA)
	checkEquals(timeSeries$freq,"dailyNoWeekend")
	
}
