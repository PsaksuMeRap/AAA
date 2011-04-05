# TODO: Add comment
# 
# Author: claudio
###############################################################################


verifyPositivity <- function(timeSeries,showErrorMessage=FALSE) {
	## this function verify that timeSeries values are larger than 0
	 
	isAvailable <- !is.na(timeSeries$data)
	if (sum(isAvailable) < 3) {
		if (showErrorMessage) {
			string = paste("La serie '",timeSeries$name,"' ha meno di 3 valori validi!",sep="")
			tkmessageBox(message=string,icon="error")
		}
		return(1)     
	}
	largerThenZero <- timeSeries$data[isAvailable] > 0
	if (!all(largerThenZero)) {
		if (showErrorMessage) {		
			string = paste("La serie '",timeSeries$name,"' ha valori negativi o uguali a zero!",sep="")
			tkmessageBox(message=string,icon="error")
		}
		return(1)
	}
	return(0)
}


standardizeTimeSeries <- function(timeSeries,showErrorMessage=FALSE)
{
	## timeSeries: a timeSeries
	if (class(timeSeries)!="timeSeries") {
		
	}
	average <- mean(timeSeries$data,na.rm=TRUE)
	
	## compute mean and variance of the time series
	
	isValid = !is.na(timeSeries$data)
	if (sum(isValid)==0) {
		if (showErrorMessage) {
			string = paste("The series", timeSeries$name,"does not have valid observations!")
			tkmessageBox(message=string,icon="error")
			
		}
		standirdizedSeries <- create_timeSeries(name=timeSeries$name)
		return(list(standardizedSeries=standirdizedSeries,average=NA,variance=NA))
	}
	
	variance = var(timeSeries$data,na.rm=TRUE)  
	if (variance>0) {
		newData <- (timeSeries$data[isValid] - average) / sqrt(variance)
	} else {
		newData <- timeSeries$data
		newData[isValid] <- 1
	}
	
	standirdizedSeries <- create_timeSeries(name=timeSeries$name,data=newData) 
	return(list(standardizedSeries=standirdizedSeries,average=average,variance=variance))
}

toTimeSeries <- function(df.x,timeSeriesNames) {
	# le date sono la prima colonna
	
	if (ncol(df.x)<=1) return(list())
	
	dates <- df.x[,1]
	df.x <- df.x[,-1,drop=FALSE]
	if (missing(timeSeriesNames)) timeSeriesNames <- colnames(df.x)
	
	wrap.create_timeSeries <- function(i) {
		values <- df.x[,i]
		names(values) <- dates
		return(create_timeSeries(name=timeSeriesNames[i],data=values))
	}

	nbSeries <- length(df.x)
	return(lapply(1:nbSeries,wrap.create_timeSeries))
	
}

toMatrix <- function(l.timeSeries) {
	
	nSeries <- length(l.timeSeries)
	if (length(nSeries)==0) return(matrix())
	
	dates <- unlist(lapply(l.timeSeries,function(x){return(names(x$data))}))
	dates <- sort(unique(dates))
	m.data <- matrix(NA,nrow=length(dates),ncol=nSeries)
	colnames(m.data) <- extractLists(l.timeSeries,"name")
	rownames(m.data) <- dates
	for (i in 1:nSeries) m.data[names(l.timeSeries[[i]]$data),i] <- l.timeSeries[[i]]$data
	
	return(m.data)
}

computeReturns <- function(timeSeries) {

	nbObs <- length(timeSeries$data)

	if (nbObs<=1) return(create_timeSeries(name=timeSeries$name,data=NA,type="percentage"))
	
	returns <- timeSeries$data[-1] / timeSeries$data[-nbObs] -1
	names(returns) <- names(timeSeries$data[-1])
	result <- create_timeSeries(name=timeSeries$name,data=returns,
			type="percentage",freq=timeSeries$freq)
	return(result)
}

computeLogReturns <- function(timeSeries) {
	
	nbObs <- length(timeSeries$data)
	
	if (nbObs<=1) return(create_timeSeries(name=timeSeries$name,data=NA))
	valid <- !is.na(timeSeries$data)
	logReturns <- timeSeries$data
	logReturns[valid] <- log(timeSeries$data[valid])
	returns <- logReturns[-1] - logReturns[-nbObs]
	names(returns) <- names(timeSeries$data[-1])
	result <- create_timeSeries(name=timeSeries$name,data=returns,
			type="logarithmic",freq=timeSeries$freq)
	return(result)
}


linearInterpolate <- function(timeSeries) {
	
	nbObs <- length(timeSeries$data)
	if (nbObs==0) return(0)
	
	isMissing <- is.na(timeSeries$data)
	if (!any(isMissing)) return(0)
	
	x <- 1:nbObs
	y <- timeSeries$data
	
	interpolatedValues <- approx(x,y,xout=x[isMissing],
			rule=c(2,2),ties="ordered")
	timeSeries$data[isMissing] <- interpolatedValues$y
	
}