# TODO: Add comment
# 
# Author: Claudio
###############################################################################

## -- inizio setup
rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RODBC")
library("RUnit")
home <- "/home/claudio/eclipse/Produzione/"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")
## -- fine setup


## -- inizio procedura controllo - parte generale
source("./odbc/connessioni.R")

checkDirectory <- "/home/claudio/"
setwd(checkDirectory)
## -- fine procedura controllo - parte generale


## -- avvia la testSuite clienti privati

## inizializza la testSuite clienti privati
fondi <- c("pippo53","pippo76","pippo210")
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
		"pippo14",
		"pippo15",
		"pippo16",
		"pippo17",
		"pippo26",
		"pippo28",
		"pippo39",
		"pippo40",
		"pippo42",
		"pippo47",
		"pippo48",
		"pippo51",
		"pippo54",
		"pippo65",
		"pippo66",
		"pippo71",
		"pippo73",
		"pippo74",
		"pippo75",
		"pippo84",
		"pippo88",
		"pippo92",
		"pippo93",
		"pippo95",
		"pippo96",
		"pippo97",
		"pippo98",
		"pippo102",
		"pippo104",
		"pippo110",
		"pippo116",
		"pippo120",
		"pippo121",
		"pippo123",
		"pippo124",
		"pippo131",
		"pippo132",
		"pippo135",
		"pippo136",
		"pippo138",
		"pippo139",
		"pippo140",
		"pippo158",
		"pippo159",
		"pippo160",
		"pippo161",
		"pippo168",
		"pippo169",
		"pippo180",
		"pippo181",
		"pippo185",
		"pippo187",
		"pippo188",
		"pippo190",
		"pippo191",
		"pippo193",
		"pippo195",
		"pippo196",
		"pippo197",
		"pippo198",
		"pippo199",
		"pippo202",
		"pippo204",
		"pippo205",
		"pippo206"
)


if (FALSE) {
	dates <- seq(as.Date("2010-07-07"),to=as.Date("2011-08-03"),by=7)
	dates <- as.Date("2011-08-03")
	
	repositories$fixedIncome <- create_repositoryFixedIncome()	
	repositories$instruments <- create_repositoryInstruments()	
	repositories$equities <- create_repositoryEquities()			
	repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
	
	for (date in as.character(dates)) {
		
		dati <- importDBPortfolioGeneraleByDate(fetchDate=date)
		
		## eliminare la posizione call-geld da 2'421'000 dollari di Cuneo
		extract <- extractFromList(dati,"Cliente") == "pippo66" & 
				extractFromList(dati,"Strumento") == "L" & 
				extractFromList(dati,"Nome") == "USD-CallGeld LGT-CALL GELD"
		dati[extract] <- NULL
		
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
		
		# importa i portafogli dei fondi e dei clienti
		portfParser <- create_parserPortfolio()
		portfolios <- lapply(c(fondi,clienti),portfParser$parse,dati)
		
		# espandi i portafogli
		explodeAllPortfoliosByAllFunds(portfolios)
		
		testSuite <- create_riskmanTestSuite(name="Clienti Ayrton",dirs="./AAA/mandato/")
		results <- importAndRunRiskmanTestSuite(testSuite,portfolios,valuationDate=date)
	}
} else {
	
	dati <- importDBPortfolioGenerale()
	
	## eliminare la posizione call-geld da 2'421'000 dollari di Cuneo
	extract <- extractFromList(dati,"Cliente") == "pippo66" & 
			extractFromList(dati,"Strumento") == "L" & 
			extractFromList(dati,"Nome") == "USD-CallGeld LGT-CALL GELD"
	dati[extract] <- NULL
	
	repositories$exchangeRates <- create_repositoryExchangeRates()
	
	# importa i portafogli dei fondi e dei clienti
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(c(fondi,clienti),portfParser$parse,dati)
	
	# espandi i portafogli
	explodeAllPortfoliosByAllFunds(portfolios)
	
	AyrtonTestSuite <- create_riskmanTestSuite(name="Clienti Ayrton",dirs="./AAA/mandato")
	results <- importAndRunRiskmanTestSuite(AyrtonTestSuite,portfolios)
}

## -- termina testSuite clienti privati


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


x <- c("pippo160",
		"pippo66")
whoisP(x)
