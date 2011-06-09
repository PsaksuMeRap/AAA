# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.shouldImportDataReturns <- function() {
	source("./unitTests/utilities/createReturnsDataFrame.R")
	returns.df <- createReturnsDataFrame()
	
	timeSeries <- transformToTimeSeriesReturnDataFrame(returns.df)

	checkEquals(length(timeSeries),4)
	checkEquals(timeSeries[[1]]$name,"pippo10")
	checkEquals(names(timeSeries[[1]]$data)[2],"2010-05-04")
	checkEquals(length(timeSeries[[4]]$data),2)
	checkEquals(timeSeries[[4]]$data[[2]],-0.00694486281040763)
}
