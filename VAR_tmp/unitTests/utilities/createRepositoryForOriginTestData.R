# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryForOriginTestData <- function() {
	
	testData <- new.env()
	
	equity1 <- list()
	equity1$Cliente <- "pippo160"
	equity1$Strumento <- "A"
	equity1$Moneta <- "CHF"
	equity1$Nome <- "Roche Holding Gs"
	equity1$ValoreMercatoMonetaCHF <- 88205
	equity1$ID_AAA <- 824
	equity1$ID_strumento <- 1
	
	testData$equity1 <- equity1
	return(testData)
}
