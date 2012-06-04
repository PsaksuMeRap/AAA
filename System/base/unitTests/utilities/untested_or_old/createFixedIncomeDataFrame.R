# TODO: Add comment
# 
# Author: claudio
###############################################################################


createFixedIncomeDataFrame <- function() {
	FixedIncome.df <- read.csv(file="./base/unitTests/data/repositoryFixedIncome.csv",
			header=TRUE,stringsAsFactors=FALSE)
	return(FixedIncome.df)
}
