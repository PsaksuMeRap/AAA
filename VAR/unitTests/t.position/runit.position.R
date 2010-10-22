# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_position <- function() {
	source("./lib/position.R")
	source("./lib/repository.R")
	
	# crea l'equity repository
	source("./unitTests/utilities/createEquityDataFrame.R")
	equities.df <- createEquityDataFrame()
	
	repositories <<- new.env()
	repositories$equities <- create_repositoryEquities(equities.df)
	rm(equities.df)
	
	# crea la posizione
	position <- create_position()
	
	position$create(name="DEGUSSA AG",
			currency="CHF",
			amount=0.0,
			origin=list(ID_AAA=400)
	)
	
	checkEquals(class(position),"position")
	checkEquals(position$isInstrument("position"),TRUE)
	checkEquals(position$isInstrument("positions"),FALSE)
	
	position$extendEquities()
	checkEquals(position$ticker,"DGX.XE")
	rm(repositories)
}


test.isCurrency <- function() {
	source("./lib/position.R")		

	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=10)
	)
	
	checkEquals(position$isCurrency("EUR"),FALSE)
	checkEquals(position$isCurrency("USD"),TRUE)	
}


test.shouldCreateDataFrameFromPosition <- function() {
	source("./lib/position.R")
	
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position) <- c("equities",class(position))
	
	df <- data.frame(instrument="equities",name="test",currency="USD",amount=0.0,
			stringsAsFactors=FALSE)
	
	checkEquals(position$toDataFrame(),df)
}
