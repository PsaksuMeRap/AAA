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

dati <- importDBPortfolioGenerale()


## eliminare la posizione call-geld da 2'421'000 dollari di Cuneo
extract <- extractFromList(dati,"Cliente") =="pippo66" & 
		extractFromList(dati,"Strumento") =="L" & 
		extractFromList(dati,"Nome") =="USD-CallGeld LGT-CALL GELD"
dati[extract] <- NULL

repositories$fixedIncome <- create_repositoryFixedIncome()
repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()
repositories$equities <- create_repositoryEquities()

checkDirectory <- "/home/claudio/"
setwd(checkDirectory)
## -- fine procedura controllo - parte generale


## -- avvia la testSuite Fondi Bacep
fondi <- c("pippo53","pippo76","pippo210")
# fondi <- c("pippo53","pippo76")
# caso data storica
if (FALSE) {
	fine <- as.Date("2011-09-13")
	dates <- seq(as.Date("2011-01-3"),to=fine,by=7)
	dates <- c(dates,seq(as.Date("2011-01-4"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-01-5"),to=fine,by=7))	
	dates <- c(dates,seq(as.Date("2011-01-6"),to=fine,by=7))
	dates <- c(dates,seq(as.Date("2011-01-7"),to=fine,by=7))
	dates <- as.character(dates); dates <- dates[dates!="2011-01-18"]
	# togli il 18 gennaio e 19 maggio
	
	#dates <- c(as.Date("2011-06-01"),as.Date("2011-06-02"),as.Date("2011-06-03"),as.Date("2011-06-06"))
    dates <- c(as.Date("2011-03-29"))
	
	for (date in as.character(dates)) {
		dati <- importDBPortfolioGeneraleByDate(date)
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)		
		portfParser <- create_parserPortfolio()
		portfolios <- lapply(fondi,portfParser$parse,dati)
		AyrtonTestSuite <- create_riskmanTestSuite(name="Fondi Bacep",dirs="./Bacep")
		results <- importAndRunRiskmanTestSuite(AyrtonTestSuite,portfolios,valuationDate=date)
	}
} else {
	# caso data attuale
	portfParser <- create_parserPortfolio()
	portfolios <- lapply(fondi,portfParser$parse,dati)
	bacepTestSuite <- create_riskmanTestSuite(name="Fondi Bacep",dirs="./Bacep")
	results <- importAndRunRiskmanTestSuite(bacepTestSuite,portfolios)
}

## -- terminata testSuite Fondi Bacep



## -- funzioni di utilità
whoisP <- function(names) {
	df <- read.csv("/home/claudio/eclipse/Produzione/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,1]==name;return(df[isOk,2])}
	pippoNames <- sapply(names,getName,df)
	return(paste(names,"->",pippoNames))
}

whois <- function(names) {
	df <- read.csv("/home/claudio/eclipse/Produzione/associazionePippo.csv",header=TRUE,sep=",",
			as.is=TRUE)
	
	getName <- function(name,df) {isOk <- df[,2]==name;return(df[isOk,1])}
	return(sapply(names,getName,df))
}
