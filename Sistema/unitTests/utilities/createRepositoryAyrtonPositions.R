# TODO: Add comment
# 
# Author: claudio
###############################################################################


createRepositoryAyrtonPositions <- function() {
	
	testData <- new.env()

	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 15
	x@Nome <- "Roche Holding Gs"
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 824
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"

	testData$equity1 <- x
	
	# create a valid equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "A"
	x@Moneta <- "EUR"
	x@Saldo <- 1000
	x@Nome <- "Kontron AG"
	x@ValoreMercatoMonetaCHF <- 7439.7503136
	x@ID_AAA <- 1772
	x@ID_strumento <- 1
	
	class(x) <- "AyrtonPosition"
	
	testData$equity2 <- x
	
	# create a non existing equity
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo160"
	x@Strumento <- "A"
	x@Moneta <- "CHF"
	x@Saldo <- 19
	x@Nome <- "Non existing"
	x@ValoreMercatoMonetaCHF <- 88205
	x@ID_AAA <- 100020202
	x@ID_strumento <- 1
	class(x) <- "AyrtonPosition"
	
	testData$noExists <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 100000
	x@Nome <- "20130603 - 3.625% Pfizer 03-06-13"
	x@ValoreMercatoMonetaCHF <- 124345.632268
	x@ID_AAA <- 1218
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond1 <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 300000
	x@Nome <- "20120410 - 1.503% EIB FRN 10-04-12"
	x@ValoreMercatoMonetaCHF <- 362217.41556
	x@ID_AAA <- 1976
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond2 <- x
	
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo3"
	x@Strumento <- "O      "
	x@Moneta <- "EUR"
	x@Saldo <- 200000
	x@Nome <- "20120319 - 1.869% Rabobank Nederland 19-03-12"
	x@ValoreMercatoMonetaCHF <- 241164.668888
	x@ID_AAA <- 1967
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond3 <- x
	
	# create a valid bond
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "O      "
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@Nome <- "20120221 - 2% Toyota 21-02-12"
	x@ValoreMercatoMonetaCHF <- 50025
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$bond4 <- x
	
	# create a valid AccruedInterest di Fondi_obbligazionari
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo11"
	x@Strumento <- "Oacc"
	x@Moneta <- "EUR"
	x@Saldo <- 10000
	x@Nome <- "20201231 - 0% <3Y - CB-Accent Lux Sicav - Fixed Income EUR 31-12-20 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 0
	x@ID_AAA <- 825
	x@ID_strumento <- 3
	class(x) <- "AyrtonPosition"
		
	testData$proRataFondiObbligazionari <- x
	
	# create a valid AccruedInterest
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- "Oacc"
	x@Moneta <- "CHF"
	x@Saldo <- 50000
	x@Nome <- "20120221 - 2% Toyota 21-02-12 Pro-rata"
	x@ValoreMercatoMonetaCHF <- 796.818833446452
	x@ID_AAA <- 1073
	x@ID_strumento <- 2
	class(x) <- "AyrtonPosition"
	
	testData$proRata1 <- x
	
	
	# create an Unclassified security
	x <- new("AyrtonPosition")
	x@Cliente <- "pippo13"
	x@Strumento <- ""
	x@Moneta <- "CHF"
	x@Saldo <- 100.30
	x@Nome <- "Security not classified"
	x@ValoreMercatoMonetaCHF <- 123.55
	x@ID_AAA <- 1073
	x@ID_strumento <- 51
	class(x) <- "AyrtonPosition"
	
	testData$unclassified1 <- x
	
	
	return(testData)
}
