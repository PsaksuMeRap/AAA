# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEmptyEquityDataFrame <- function() {
	
	emptyEquities.df <- data.frame(id=numeric(),
			equity=character(),
			numeroValore=character(),
			ticker=character(),
			stringsAsFactors=FALSE
	)
	
}
