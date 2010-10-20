# TODO: Add comment
# 
# Author: claudio
###############################################################################



create_riskFactors <- function(position) {
	riskFactors <- data.frame(amount=NA_real_,factor=NA_character_)
	class(riskFactors) <- "riskFactors"
	
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


