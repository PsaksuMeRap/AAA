# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_position <- function() {
	
	# crea l'equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	repositories <<- new.env()
	repositories$equities <- create_repositoryEquities(equities.df)
	rm(equities.df)
	
	# crea la posizione
	position <- create_position()
	
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	
	checkEquals(class(position),"position")
	checkEquals(position$isMemberOf("position"),TRUE)
	checkEquals(position$isMemberOf("positions"),FALSE)
	
	position$extendEquities()
	checkEquals(position$ticker,"ROG")
	rm(repositories)
}
