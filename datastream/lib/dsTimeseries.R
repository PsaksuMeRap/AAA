# TODO: Add comment
# 
# Author: claudio
###############################################################################

setClass("DsTimeSeries",representation(name="character",
				dsCode="character",data="numeric"))


setGeneric("getStartDate",def=function(x) standardGeneric("getStartDate"))

setMethod("getStartDate",signature(x="DsTimeSeries"),
		function(x){
			return(names(x@data)[1])
		}
)

setGeneric("getEndDate",def=function(x) standardGeneric("getEndDate"))

setMethod("getEndDate",signature(x="DsTimeSeries"),
		function(x) {
			dates <- names(x@data)
			nbObs <- length(dates)
			return(dates[nbObs])
		}
)

setMethod("plot",
		signature(x = "DsTimeSeries"),
		function (x, y, ...) 
		{
			plot(as.Date(names(x@data)),x@data,xlab="",type="l",ylab=x@name)
		}
)


