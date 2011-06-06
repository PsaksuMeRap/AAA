# TODO: Add comment
# 
# Author: claudio
###############################################################################


test.create_position <- function() {
	source("./lib/position/position.R")
	source("./lib/repository.R")
	
	allocateTestRepositories("equities")
	
	# crea la posizione
	position <- create_position()
	
	position$create(name="Deutsche Telekom Common Stock",
			currency="EUR",
			amount=0.0,
			origin=list(ID_AAA=418)
	)
	
	checkEquals(class(position),"position")
	checkEquals(position$isInstrument("position"),TRUE)
	checkEquals(position$isInstrument("positions"),FALSE)
	
	class(position) <- c("equity","position")
	extendPosition(position)
	
	checkEquals(position$ticker,"DTE.XE")
	
	deallocateTestRepositories("equities")
}


test.isCurrency <- function() {
	source("./lib/position/position.R")		

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

test.positionToString <- function() {
	source("./lib/position/position.R")		
	
	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=10)
	)
	
	string <- paste(class(position)[1],"/","USD","/",
			formatC(100,digits=2,format="f"), "/ test")

	checkEquals(position$toString(),string)	

}

test.shouldCreateDataFrameFromPosition <- function() {
	source("./lib/position/position.R")
	
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
