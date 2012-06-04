# TODO: Add comment
# 
# Author: claudio
###############################################################################


createPoliticaInvestimentoDataFrame <- function() {
	politicaInvestimento.df <- read.csv(file="./base/unitTests/data/repositoryPoliticaInvestimento.csv",
			header=TRUE,stringsAsFactors=FALSE)
	return(politicaInvestimento.df)
}
