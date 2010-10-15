# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryInterestRates <- function() { 
    file <- paste(home,"/unitTests/t.repositories/interestRates.csv",sep="")
	interestRates.df = read.csv(file,header=TRUE,stringsAsFactors=FALSE)
	
    emptyInterestRates.df <- data.frame(currency=character(),
			date=character(),
			Scadenza=numeric(),
			maturity=character(),
			rate=numeric()
	)
	
	repository <- create_repositoryInterestRates(interestRates.df=interestRates.df) 

	checkEquals(repository$getMonthTicker(12),"1Y")
	checkEquals(repository$getMonthTicker(1),"1M")
	checkEquals(repository$getMonthTicker(0.25),"1W")
	checkException(repository$getMonthTicker(0.2))

	checkException(create_repositoryInterestRates(interestRates.df=emptyInterestRates.df))
	
}