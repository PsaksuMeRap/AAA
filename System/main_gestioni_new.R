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

repositories <- new.env()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
## -- fine setup

## -- inizio procedura controllo - parte generale
source("./odbc/connessioni.R")

repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()

checkDirectory <- "/home/claudio/XP/"
setwd(checkDirectory)
## -- fine procedura controllo - parte generale


## -- inizializza la testSuite clienti privati

clienti <- c(
		"pippo3",
		"pippo4",
		"pippo5",
		"pippo6",
		"pippo7",
		"pippo8",
		"pippo9", "pippo12","pippo109",
		"pippo10","pippo69",
		"pippo11","pippo72","pippo163",
		"pippo13",
		"pippo15",
		"pippo16",
		"pippo17",
		"pippo26",
		"pippo28",
		"pippo42",
		"pippo47",
		"pippo48",
		"pippo51",
		"pippo54",
		"pippo71",
		"pippo75",
		"pippo84",
		"pippo88",
		"pippo92",
		"pippo95",
		"pippo96",
		"pippo98",
		"pippo99",
		"pippo102",
		"pippo104",
		"pippo106",
		"pippo121",
		"pippo123",
		"pippo124",
		"pippo131",
		"pippo135",
		"pippo158",
		"pippo159",
		"pippo160",
		"pippo161",
		"pippo168",
		"pippo172",
		"pippo185",
		"pippo187",
		"pippo188",
		"pippo190",
		"pippo191",
		"pippo193",
		"pippo195",
		"pippo198",
		"pippo202",
		"pippo204",
		"pippo205",
		"pippo206",
		"pippo220",
		"pippo221",
		"pippo223",
		"pippo224",
		"pippo227",
		"pippo228",
		"pippo229"
)

		
# caso con data storica
if (FALSE) {
	fine <- as.Date("2011-09-15")
	dates <- seq(as.Date("2011-09-1"),to=fine,by=7)
	dates <- c(dates,seq(as.Date("2011-09-2"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-09-5"),to=fine,by=7))	
	dates <- c(dates,seq(as.Date("2011-09-6"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-09-7"),to=fine,by=7))
	# dates <- as.character(dates); dates <- dates[dates!="2011-01-18"]
	# togli il 18 gennaio e 19 maggio
	
	dates <- c(as.Date("2011-09-14"),as.Date("2011-09-15"),as.Date("2011-09-16"))
	dates <- c("2012-10-24")
	dates <- c("2013-05-06")	
	date <- dates
	
	for (date in as.character(dates)) {
		print(date)
		# controlla che la data sia dopo il "2011-05-31" altrimenti togli il fondo globalEconomy
		if (as.Date(date) > as.Date("2011-05-31")) fundsOwners <- c("pippo53","pippo76","pippo210") else fundsOwners <- c("pippo53","pippo76")
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
		dati <- importDBPortfolioGeneraleByDate(date)	
		
		portfolios <- filterLists(dati,"Cliente",value=c(fundsOwners,clienti))
		
		portfolios <- portfoliosFactory(portfolios)

		portfolios <- explodeAllPortfoliosByAllFunds(portfolios, only = c("GLOBAL ECONOMY"))
		# portfolios <- explodeAllPortfoliosByAllFunds(portfolios)
		
		portfolios <- explodeAndReplaceFundGlobalEquityAllPortfolios(portfolios,list(Eq=0.84,Fi=0.07))
		
		AyrtonTestSuite <- testSuiteFactory(testSuiteName="Clienti Ayrton",directories="./ClientiNew")
		results <- lapply(AyrtonTestSuite@testSuitesParsed,applyTestSuite,portfolios,date)
	}
	
} else {
	dati <- importDBPortfolioGenerale()	
	fundsOwners <- c("pippo53","pippo76","pippo210")

	portfolios <- filterLists(dati,"Cliente",value=c(fundsOwners,clienti))
	
	portfolios <- portfoliosFactory(portfolios)
	
	portfolios <- explodeAllPortfoliosByAllFunds(portfolios, only = c("GLOBAL ECONOMY"))
	
	## Marco devi cambiare i pesi qui!!!
	portfolios <- explodeAndReplaceFundGlobalEquityAllPortfolios(portfolios,list(Eq=0.876,Fi=0.054))
	portfolios <- explodeAndReplaceFundDynamicMultistrategyAllPortfolios(portfolios,list(Eq=0.305,Fi=0.505))
	
	AyrtonTestSuite <- testSuiteFactory(testSuiteName="Clienti Ayrton",directories="./ClientiNew")
	results <- lapply(AyrtonTestSuite@testSuitesParsed,applyTestSuite,portfolios)
}

## -- terminata testSuite Fondi Bacep


## -- funzioni di utilità
whoisP <- function(names) {
	df <- read.csv("/home/claudio/workspace/AAA/System/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,1]==name;return(df[isOk,2])}
	pippoNames <- sapply(names,getName,df)
	return(paste(names,"->",pippoNames))
}

whois <- function(names) {
	df <- read.csv("/home/claudio/workspace/AAA/System/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,2]==name;return(df[isOk,1])}
	return(sapply(names,getName,df))
}


# fine

x <- c("pippo11","pippo72","pippo132","pippo13","pippo84")
tmp <- extractFromList(dati,"Cliente") == "pippo221"

Pippo 2010 problema "Bond / CHF /  NA / 20140702 - 0.672% Westpac FRN (02.04.2015) 02-07-14 rated: Aaa"

