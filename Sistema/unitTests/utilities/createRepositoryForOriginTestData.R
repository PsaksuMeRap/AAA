# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryForOriginTestData <- function() {
	
	testData <- new.env()

	# create a valid equity
	x <- list()
	x$Cliente <- "pippo160"
	x$Strumento <- "A"
	x$Moneta <- "CHF"
	x$Nome <- "Roche Holding Gs"
	x$ValoreMercatoMonetaCHF <- 88205
	x$ID_AAA <- 824
	x$ID_strumento <- 1
	
	class(x) <- "ayrton"

	testData$equity1 <- x
	
	# create a non existing equity
	x <- list()
	x$Cliente <- "pippo160"
	x$Strumento <- "A"
	x$Moneta <- "CHF"
	x$Nome <- "Non existing"
	x$ValoreMercatoMonetaCHF <- 88205
	x$ID_AAA <- 100020202
	x$ID_strumento <- 1
	class(x) <- "ayrton"
	
	testData$noExists <- x
	
	
	# create a valid bond
	x <- list()
	x$Cliente <- "pippo3"
	x$Strumento <- "O      "
	x$Moneta <- "EUR"
	x$Nome <- "20130603 - 3.625% Pfizer 03-06-13"
	x$ValoreMercatoMonetaCHF <- 124345.632268
	x$ID_AAA <- 1218
	x$ID_strumento <- 2
	class(x) <- "ayrton"
	
	testData$bond1 <- x
	
	
	# create a valid bond
	x <- list()
	x$Cliente <- "pippo3"
	x$Strumento <- "O      "
	x$Moneta <- "EUR"
	x$Nome <- "20120410 - 1.503% EIB FRN 10-04-12"
	x$ValoreMercatoMonetaCHF <- 362217.41556
	x$ID_AAA <- 1976
	x$ID_strumento <- 2
	class(x) <- "ayrton"
	
	testData$bond2 <- x
	
	
	# create a valid bond
	x <- list()
	x$Cliente <- "pippo3"
	x$Strumento <- "O      "
	x$Moneta <- "EUR"
	x$Nome <- "20120319 - 1.869% Rabobank Nederland 19-03-12"
	x$ValoreMercatoMonetaCHF <- 241164.668888
	x$ID_AAA <- 1967
	x$ID_strumento <- 2
	class(x) <- "ayrton"
	
	testData$bond3 <- x
	
	return(testData)
}
