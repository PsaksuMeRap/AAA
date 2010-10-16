# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEquityDataFrame <- function() {
	
	equities.df <- data.frame(id=c(1,10),
			equity=c("Nestle","Roche"),
			numeroValore=c("CH123","CH4939"),
			ticker=c("NESN","ROG"),stringsAsFactors=FALSE
	)
	
	return(equities.df)
	
}
