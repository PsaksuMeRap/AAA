# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_repositoryEquities <- function() {
	
	equities.df <- data.frame(id=c(1,10),
			equity=c("Nestle","Roche"),
			numeroValore=c("CH123","CH4939"),
			ticker=c("NESN","ROG"),stringsAsFactors=FALSE
	)
	
	repository <- create_repositoryEquities(equities.df)
	
	checkEquals(class(repository),"repositoryEquityTicker")
	
	checkEquals(repository$tickerFromId(1),"NESN")
	checkEquals(repository$tickerFromId(10),"ROG")
	checkEquals(repository$tickerFromId(3),NA_character_)
	
	# with an empty repository
	equities.df <- data.frame(id=numeric(),
			equity=character(),
			numeroValore=character(),
			ticker=character(),stringsAsFactors=FALSE
	)
	
	repository <- create_repositoryEquities(equities.df)	
	checkEquals(repository$tickerFromId(1),NA_character_)
	print(repository$tickerFromId(1))
}
