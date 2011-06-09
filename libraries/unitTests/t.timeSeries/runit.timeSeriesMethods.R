# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.variance <- function() {
	
	# Test 1
	name = "nome"
	dates=c("2010-01-02","2010-01-03","2011-02-24")
	values <- c(100,0,50)
	names(values) <- dates
	
	timeSeries <- create_timeSeries(name=name,data=values)
	
	variance <- Var(timeSeries)
	
	checkEquals(variance,0.5*((100-50)^2 + (-50)^2))
	
	# Test 2
	name = "nome"
	dates=c("2010-01-02","2010-01-03","2011-02-23","2011-02-24")
	values <- c(100,NA,0,50)
	names(values) <- dates
	
	timeSeries <- create_timeSeries(name=name,data=values)
	
	variance <- Var(timeSeries,na.rm=TRUE)
	
	checkEquals(variance,0.5*((100-50)^2 + (-50)^2))
	
	
}

test.verifyPositivity <- function() {
	
	# Test 1
	name = "nome"
	date=c("2010-01-02","2010-01-03","2011-02-23")
	value <- c(100,100.10,0)
	names(value) <- date
	data = value
	
	timeSeries <- create_timeSeries(name=name,data=data)
	checkEquals(verifyPositivity(timeSeries),1)
	
	# Test 2
	name = "nome"
	date=c("2010-01-02","2010-01-03")
	value <- c(100,100.10)
	names(value) <- date
	data = value
	
	timeSeries <- create_timeSeries(name=name,data=data)
	checkEquals(verifyPositivity(timeSeries),1)
	
	# Test 3
	name = "nome"
	date=c("2010-01-02","2010-01-03","2010-01-04","2011-02-23")
	value <- c(100,100.10,101,150.35)
	names(value) <- date
	data = value
	
	timeSeries <- create_timeSeries(name=name,data=data)
	checkEquals(verifyPositivity(timeSeries),0)
	
}


test.standardizeTimeSeries <- function() {
	ts <- create_timeSeries(name="test",data=c(100.0,150,200.0))		
	shouldTs <- create_timeSeries(name="test",data=c(-1.0,0,1.0))
	
	result <- standardizeTimeSeries(ts)
	checkEquals(result$average,150.0)
	checkEquals(result$variance,50.0^2)
	checkEquals(result$standardizedSeries,shouldTs)
	
	# now with one empty observation
	
	ts <- create_timeSeries(name="test",data=c(100.0,NA,150,200.0))		
	shouldTs <- create_timeSeries(name="test",data=c(-1.0,NA,0,1.0))
	
	result <- standardizeTimeSeries(ts)
	checkEquals(result$average,150.0)
	checkEquals(result$variance,50.0^2)
	checkEquals(result$standardizedSeries,shouldTs)
	
	# all empty
	
	ts <- create_timeSeries(name="test",data=NA)		
	shouldTs <- create_timeSeries(name="test",data=NA)
	
	result <- standardizeTimeSeries(ts)
	checkEquals(result$average,NA)
	checkEquals(result$variance,NA)
	checkEquals(result$standardizedSeries,shouldTs)
}

utility.creaTimeSeries3 <- function() {
	dates <- c("2001-01-02","2002-01-04","2004-08-23")
	values1 <- c(1,2,3.5)
	values2 <- c(0.3,876.6,12)
	names(values1) <- dates
	names(values2) <- dates
	
	dates <- c("2001-01-02","2002-01-04","2004-08-23","2004-09-15")
	values3 <- c(0.12,76.6,2,16)
	names(values3) <- dates
	
	ts1 <- create_timeSeries(name="dati1",data=values1)
	ts2 <- create_timeSeries(name="dati2",data=values2)
	ts3 <- create_timeSeries(name="dati3",data=values3)	
	return(list(ts1,ts2,ts3))
}

test.extractDates <- function() {
	# crea le serie
	l.timeSeries <- utility.creaTimeSeries3()
	
	dates <- extractDates(l.timeSeries)
	
	shouldDates <- c("2001-01-02","2002-01-04","2004-08-23",
			"2004-09-15")
	checkEquals(dates,shouldDates)
	
}

test.toTimeSeries <- function() {
	
	# crea le serie
	l.ts <- utility.creaTimeSeries3()
	ts1 <- l.ts[[1]]
	ts2 <- l.ts[[2]]
	rm(l.ts)
	
	dates <- names(ts2$data)
	
	df.series <- data.frame(dates=dates, dati1=ts1$data, dati2=ts2$data)
	
	l.timeSeries <- toTimeSeries(df.series)

	checkEquals(length(l.timeSeries),2)
	checkEquals(l.timeSeries,list(ts1,ts2))
	
	# con i nomi
	l.timeSeries <- toTimeSeries(df.series,timeSeriesNames=c("pippo1","Clau"))
	ts1 <- create_timeSeries(name="pippo1",data=ts1$data)
	ts2 <- create_timeSeries(name="Clau",data=ts2$data)
	checkEquals(l.timeSeries,list(ts1,ts2))
}

test.toMatrix <- function() {
	# crea le serieStoriche
	l.timeSeries <- utility.creaTimeSeries3()
	m.data <- toMatrix (l.timeSeries)
	
	checkEquals(dim(m.data),c(4,3))
	checkEquals(colnames(m.data),extractFromList(l.timeSeries,"name"))
	checkEquals(m.data[,1],c(l.timeSeries[[1]]$data,"2004-09-15"=NA))
	checkEquals(m.data[,3],l.timeSeries[[3]]$data)
	
}

test.computeReturns <- function() {
	
	# crea le serie
	l.ts <- utility.creaTimeSeries3()
	ts1 <- l.ts[[1]]
	nbObs <- length(ts1$data)
	result <- computeReturns(ts1)
	
	checkEquals(length(result$data),nbObs-1)
	a <- result$data[1]
	names(a) <- NULL
	checkEquals(a,1.0)
	
	# timeSeries con singola osservazione
	ts1$data <- ts1$data[1]
	ts1$type <- "percentage"
	result <- computeReturns(ts1)
	ts1$data <- NA
	checkEquals(result,ts1)

}


test.computeLogReturns <- function() {
	
	# crea le serie
	l.ts <- utility.creaTimeSeries3()
	ts1 <- l.ts[[1]]
	nbObs <- length(ts1$data)
	result <- computeLogReturns(ts1)
	
	checkEquals(length(result$data),nbObs-1)
	a <- result$data[1]
	names(a) <- NULL
	checkEquals(a,log(2))
	
}

test.linearInterpolate <- function() {
	
	data = c(NA,4,NA,6,NA,7,NA)
	shouldData <- c(4,4,5,6,6.5,7,7)
	timeSeries <- create_timeSeries(name="test",data=data)
	
	linearInterpolate(timeSeries)
	
}

test.computeCor <- function() {
	# crea le serie e calcola la correlazione
	# utilizzando tutte le osservazioni disponibili
	# P$l.logReturns <- lapply(l.timeSeries,computeLogReturns)
}
