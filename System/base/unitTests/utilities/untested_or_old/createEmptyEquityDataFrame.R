# TODO: Add comment
# 
# Author: claudio
###############################################################################


createEmptyEquityDataFrame <- function() {
	
	emptyEquities.df <- data.frame(id=numeric(),
			equity=character(),
			ticker=character(),
			stringsAsFactors=FALSE
	)
	return(emptyEquities.df)
}
