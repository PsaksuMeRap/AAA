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

test.fieldsToPrintDefault <- function() {
	source("./lib/position/position.R")		
	
	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=10)
	)
	

	fields <- list()
	fields$class <- class(position)[1]
	fields$currency <- position$money$currency
	fields$amount <- formatC(position$money$amount,big.mark = "'",
				decimal.mark = ".",format="f",digits=2)
	fields$name <- position$name
	
	# test 1: senza width
	width <- list();
	fieldsResult <- position$fieldsToPrintDefault(width)	
	checkEquals(fieldsResult,fields)	

	# test 2: con width$className
	width <- c(className=12)
	fields$class <- "position    "
	fieldsResult <- position$fieldsToPrintDefault(width)	
	checkEquals(fieldsResult,fields)
	
	# test 3: con width$className e width$amount
	width <- c(className=12,amount=10)
	fields$class <- "position    "
	fields$amount <- "    100.00"
	fieldsResult <- position$fieldsToPrintDefault(width)	
	checkEquals(fieldsResult,fields)
}

test.fieldsToPrint <- function() {
	source("./lib/position/position.R")		
	
	# crea la posizione
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=100,
			origin=list(ID_AAA=10)
	)
	
	
	fields <- list()
	fields$class <- class(position)[1]
	fields$currency <- position$money$currency
	fields$amount <- formatC(position$money$amount,big.mark = "'",
			decimal.mark = ".",format="f",digits=2)
	fields$name <- position$name
	
	# test 1: senza width
	fieldsResult <- position$fieldsToPrint()	
	checkEquals(fieldsResult,fields)	
	
	# test 2: con width$className
	width <- c(className=12)
	fields$class <- "position    "
	fieldsResult <- position$fieldsToPrint(width)	
	checkEquals(fieldsResult,fields)
	
	# test 3: con width$className e width$amount
	width <- c(className=12,amount=10)
	fields$class <- "position    "
	fields$amount <- "    100.00"
	fieldsResult <- position$fieldsToPrint(width)	
	checkEquals(fieldsResult,fields)
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


test.shouldVerifyIsConsistent <- function() {
	source("./lib/position/position.R")
	
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=0.0,
			origin=list(ID_AAA=10)
	)
	class(position) <- c("equities",class(position))
	
	checkEquals(position$isConsistent(),TRUE)
	
	position <- create_position()
	position$create(name="test",
			currency="USD",
			amount=NA,
			origin=list(ID_AAA=10)
	)
	class(position) <- c("equities",class(position))
	
	checkEquals(position$isConsistent(),FALSE)
	
}


