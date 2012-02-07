# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryInterestRates <- function() { 
    file <- paste(home,"/unitTests/t.repositories/interestRates.csv",sep="")
	interestRates.df = read.csv(file,header=TRUE,stringsAsFactors=FALSE)
	
	repository <- create_repositoryInterestRates(interestRates.df=interestRates.df) 

	checkEquals(repository$getMonthTicker(12),"1Y")
	checkEquals(repository$getMonthTicker(1),"1M")
	checkEquals(repository$getMonthTicker(0.25),"1W")
}

test.shouldFailWithInvalidMaturity <- function() {
	file <- paste(home,"/unitTests/t.repositories/interestRates.csv",sep="")
	interestRates.df = read.csv(file,header=TRUE,stringsAsFactors=FALSE)
	
	repository <- create_repositoryInterestRates(interestRates.df=interestRates.df)
	checkException(repository$getMonthTicker(0.2),silent=TRUE)
}

test.shouldFailWithEmptyDataFrame <- function() { 
	
	emptyInterestRates.df <- data.frame(
			currency=character(),
			date=character(),
			Scadenza=numeric(),
			maturity=character(),
			rate=numeric()
	)
	
	checkException(create_repositoryInterestRates(interestRates.df=emptyInterestRates.df),
			silent=TRUE)
}