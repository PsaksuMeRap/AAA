# TODO: Add comment
# 
# Author: claudio
###############################################################################

create_riskFactor <- function(factor=NA, amount=NA) {
	riskFactor <- list()
	class(riskFactor) <- "riskFactor"

	riskFactor$factor <- factor
	riskFactor$amount <- amount
	
	return(riskFactor)
}


determine_riskFactor <- function(position) UseMethod("create_riskFactor",position)

determine_riskFactor.bond <- function(position) {
	print ("sono un bond")
}

determine_riskFactor.equity <- function(position) {
	
	riskFactors <- add(amount=position$amount,factor=position$currency)
	riskFactors <- add(amount=position$amount,factor=position$ticker)
	print ("sono un'azione")
}

determine_riskFactor.default <- function(position) {
	print("metodo di default")
}




source("./lib/position/position.R")
source("./lib/repository.R")
source("./unitTests/utilities/allocateTestRepositories.R")


allocateTestRepositories("equities")

# crea la posizione
position <- create_position()

position$create(name="Deutsche Telekom Common Stock",
		currency="EUR",
		amount=0.0,
		origin=list(ID_AAA=418)
)

class(position) <- c("pippo","equity","position")
extendPosition(position)

create_riskFactor(position)







deallocateTestRepositories("equities")






create_riskFactors <- function(position) {
	riskFactors <- data.frame(amount=NA_real_,factor=NA_character_)
	class(riskFactors) <- "riskFactor"
	
	add <- function(amount,factor) {
		riskFactors <<- rbind(riskFactors,data.frame(amount,factor))
	}    
	
	if (position$isInstrument("Conto corrente")) {
		riskFactors <- add(amount=position$amount,factor=position$currency)
	}
	if (position$isInstrument("equities")) {
		riskFactors <- add(amount=position$amount,factor=position$currency)
		riskFactors <- add(amount=position$amount,factor=position$ticker)
	}
	
	return(riskFactors)
}


