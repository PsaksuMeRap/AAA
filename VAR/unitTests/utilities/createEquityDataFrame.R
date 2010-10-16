# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquityDataFrame <- function() {
	
	equities.df <- data.frame(id=c(1,10,766),
			equity=c("Nestle","Roche","Philips"),
			numeroValore=c("CH123","CH4939","1106818EU"),
			ticker=c("NESN","ROG","PHIL"),stringsAsFactors=FALSE
	)
	
	return(equities.df)
	
}
