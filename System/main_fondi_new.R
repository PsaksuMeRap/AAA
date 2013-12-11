# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("RUnit")
library("tcltk")

if(.Platform$OS.type=="windows") {
	sourceCodeDir <- "\\\\usi/dfs/Utenti/O/ortellic/My Documents/workspace/AAA/System/"
	homeDir <- sourceCodeDir
} else {
	sourceCodeDir <- "/home/claudio/workspace/AAA/System/"
	homeDir <- sourceCodeDir
}

setwd(sourceCodeDir)

stringsAsFactors = FALSE
repositories <- new.env()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
## -- fine setup

## -- inizio procedura controllo - parte generale
source("./odbc/connessioni.R")

dati <- importDBPortfolioGenerale()

repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()

checkDirectory <- "/home/claudio/XP/FondiNew"
setwd(checkDirectory)
## -- fine procedura controllo - parte generale


## -- avvia la testSuite Fondi Bacep

# caso con data storica
if (FALSE) {
	fine  <- as.Date("2011-09-15")
	dates <- seq(as.Date("2011-09-1"),to=fine,by=7)
	dates <- c(dates,seq(as.Date("2011-09-2"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-09-5"),to=fine,by=7))	
	dates <- c(dates,seq(as.Date("2011-09-6"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-09-7"),to=fine,by=7))
	# dates <- as.character(dates); dates <- dates[dates!="2011-01-18"]
	# togli il 18 gennaio e 19 maggio
	
	dates <- c(as.Date("2011-09-14"),as.Date("2011-09-15"),as.Date("2011-09-16"))
	dates <- c("2012-10-01","2012-10-02","2012-10-03","2012-10-04","2012-10-05",
			"2012-10-08","2012-10-09","2012-10-10","2012-10-11","2012-10-12",
			"2012-10-15","2012-10-16","2012-10-17","2012-10-18","2012-10-19")
	for (date in as.character(dates)) {
		print(date)
		# controlla che la data sia dopo il "2011-05-31" altrimenti togli il fondo globalEconomy
		if (as.Date(date) > as.Date("2011-05-31")) fundsOwners <- c("pippo53","pippo76","pippo210") else fundsOwners <- c("pippo53","pippo76")
		if (as.Date(date) > as.Date("2012-09-01")) fundsOwners <- c(fundsOwners,"pippo100","pippo101")
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
		dati <- importDBPortfolioGeneraleByDate(date)		
		fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)
		
		fundPortfolios <- portfoliosFactory(fundPortfolios)

		AyrtonTestSuite <- testSuiteFactory(testSuiteName="Fondi OpenCapital",directories="./")
		
		results <- lapply(AyrtonTestSuite@testSuitesParsed,applyTestSuite,fundPortfolios,date)
	}
} else {
	# caso data attuale
	# fundsOwners <- c("pippo53","pippo76","pippo210","pippo100","pippo101")
	fundsOwners <- c("pippo210")
	
	fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)
	
	fundPortfolios <- portfoliosFactory(fundPortfolios)
	
	AyrtonTestSuite <- testSuiteFactory(testSuiteName="Fondi OpenCapital",directories="./")

	results <- lapply(AyrtonTestSuite@testSuitesParsed,applyTestSuite,fundPortfolios)
}

## -- terminata testSuite Fondi Bacep



## -- funzioni di utilità
whoisP <- function(names) {
	df <- read.csv("/home/claudio/workspace/Produzione/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,1]==name;return(df[isOk,2])}
	pippoNames <- sapply(names,getName,df)
	return(paste(names,"->",pippoNames))
}

whois <- function(names) {
	df <- read.csv("/home/claudio/workspace/Produzione/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,2]==name;return(df[isOk,1])}
	return(sapply(names,getName,df))
}





