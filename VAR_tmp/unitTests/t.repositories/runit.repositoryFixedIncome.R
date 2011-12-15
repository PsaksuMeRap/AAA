# TODO: Add comment
# 
# Author: claudio
###############################################################################

test.shouldCreateRepositoryFixedIncome <- function() {
	
	source("./lib/repository.R")
	source("./unitTests/utilities/createFixedIncomeDataFrame.R")
	
	fixedIncome.df <- createFixedIncomeDataFrame()
	repository <- create_repositoryFixedIncome(fixedIncome.df)
	
	checkEquals(repository$fixedIncome.df[3,"ISIN"],NA_character_)
	checkEquals(repository$fixedIncome.df[36,"Data_iniziale_1"],"2009-06-17 00:00:00.000")
	checkEquals(repository$fixedIncome.df[14,"ID"],1260)
	
}
