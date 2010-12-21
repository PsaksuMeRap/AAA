# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RODBC")
library("RUnit")
home <- "/home/claudio/eclipse/AAA/VAR/"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./lib/library.R")


# inizio procedura controllo
source("./odbc/connessioni.R")


allDates <- seq(as.Date("2010-07-05"),to=as.Date("2010-12-17"),by="days")
fetch <- rep(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE),length(allDates)/7)
fetchDates <- allDates[fetch]
# fetchDate <- fetchDates[length(fetchDates)-7]

# Controlla i fondi
if (FALSE) {
	for (fetchDate in as.character(fetchDates)) {
		
		dati <- importDBPortfolioGeneraleByDate(fetchDate)
		repositories$fixedIncome <- create_repositoryFixedIncome()
		repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
		repositories$instruments <- create_repositoryInstruments()
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=fetchDate)
		repositories$equities <- create_repositoryEquities()
		
		
		clienti <- c("pippo53","pippo76")
		portfParser <- create_parserPortfolio()
		portfolios <- lapply(clienti,portfParser$parse,dati)
		
		checkDirectory <- "/home/claudio/"
		setwd(checkDirectory)
		
		# inizializza la testSuite
		bacepTestSuite <- create_riskmanTestSuite(name="Fondi Bacep",dirs="./Bacep")
		results <- importAndRunRiskmanTestSuite(bacepTestSuite,portfolios,valuationDate=fetchDate)	
	}
}


clientiAAA <- c(
		"pippo10"
		,"pippo102"
		,"pippo104"
		,"pippo109"
		,"pippo11"
		,"pippo110"
		,"pippo116"
		,"pippo118"
		,"pippo12"
		,"pippo120"
		,"pippo121"
		,"pippo123"
		,"pippo124"
		,"pippo13"
		,"pippo131"
		,"pippo132"
		,"pippo135"
		,"pippo136"
		,"pippo138"
		,"pippo139"
		,"pippo14"
		,"pippo140"
		,"pippo15"
		,"pippo158"			
		,"pippo159"
		,"pippo16"				
		,"pippo160"				
		,"pippo161"
		,"pippo163"
		,"pippo168"
		,"pippo169"
		,"pippo17"
		,"pippo180"
		,"pippo181"
		,"pippo185"
		,"pippo186"
		,"pippo187"
		,"pippo188"
		,"pippo190"
		,"pippo191"
		,"pippo193"
		,"pippo195"
		,"pippo196"
		,"pippo197"
		,"pippo198"
		,"pippo199"
		,"pippo202"
		,"pippo204"
		,"pippo205"
		,"pippo206"
		,"pippo26"
		,"pippo28"
		,"pippo3"
		,"pippo39"
		,"pippo4"
		,"pippo40"
		,"pippo42"
		,"pippo47"
		,"pippo48"
		,"pippo5"
		,"pippo51"
		,"pippo54"
		,"pippo6"
		,"pippo61"
		,"pippo62"
		,"pippo65"
		,"pippo66"
		,"pippo69"
		,"pippo7"
		,"pippo71"
		,"pippo72"
		,"pippo73"
		,"pippo74"
		,"pippo75"
		,"pippo8"
		,"pippo83"
		,"pippo84"
		,"pippo88"
		,"pippo9"
		,"pippo92"
		,"pippo93"
		,"pippo95"
		,"pippo96"
		,"pippo97"
		,"pippo98"
)



# Controlla i portafogli AAA
if (TRUE) {
	for (fetchDate in as.character(fetchDates)) {
		
		dati <- importDBPortfolioGeneraleByDate(fetchDate)
		repositories$fixedIncome <- create_repositoryFixedIncome()
		repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
		repositories$instruments <- create_repositoryInstruments()
		repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=fetchDate)
		repositories$equities <- create_repositoryEquities()
		
		clienti <- clientiAAA
		portfParser <- create_parserPortfolio()
		portfolios <- lapply(clienti,portfParser$parse,dati)
		
		checkDirectory <- "/home/claudio/"
		setwd(checkDirectory)
		
		# inizializza la testSuite
		AyrtonTestSuite <- create_riskmanTestSuite(name="Clienti Ayrton",dirs="./AAA/sforamenti giustificati")
		results <- importAndRunRiskmanTestSuite(AyrtonTestSuite,portfolios,valuationDate=fetchDate)	
	}
}





