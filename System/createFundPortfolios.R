# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
library("tcltk")
library("stringr")

if(.Platform$OS.type=="windows") {
	homeDir <- "C:/riskman"
	sourceCodeDir <- getwd()
} else {
	homeDir <- "/home/claudio/riskman"
	sourceCodeDir <- getwd()
}


stringsAsFactors = FALSE
repositories <- new.env()

source("./base/lib/library.R")
source("./ayrton/lib/library.R")
source("./riskman/lib/library.R")
source("./adviceManagement/lib/library.R")
## -- fine setup

## -- inizio procedura salvataggio portafogli - parte generale
source("./odbc/connessioni.R")

dati <- importDBPortfolioGenerale()

repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()

setwd(homeDir)
## -- fine procedura salvataggio portafogli - parte generale


mappingPippoPortfolio <- c(
		pippo53="globalEquity",
		pippo76="fixedIncome",
		pippo210="globalEconomy"
)

## caso con data storica
if (FALSE) {
	date <- c("2012-04-13")
	# controlla che la data sia dopo il "2011-05-31" altrimenti togli il fondo globalEconomy
	if (as.Date(date) > as.Date("2011-05-31")) fundsOwners <- c("pippo53","pippo76","pippo210") else fundsOwners <- c("pippo53","pippo76")
	repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)
	
	# importa i dati dalla tabella del DB
	dati <- importDBPortfolioGeneraleByDate(date)
	
	# filtra solo i fondi
	fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)
	
	# crea i portafogli
	fundPortfolios <- portfoliosFactory(fundPortfolios)
	
} else {
	# caso data attuale
	fundsOwners <- c("pippo53","pippo76","pippo210")
	fundPortfolios <- filterLists(dati,"Cliente",value=fundsOwners)	
	fundPortfolios <- portfoliosFactory(fundPortfolios)

}

# salva i portafogli in homeDir
for (portfolio in fundPortfolios) {
	portfolioName <- mappingPippoPortfolio[[portfolio@owner]]
	if (!file.exists(file.path(homeDir,portfolioName))) dir.create(file.path(homeDir,portfolioName)) 
	savePortfolio(portfolio,portfolioSubDirectory=portfolioName,directory=homeDir)
}

# zip the portfolio's directories
zipFullFileName <- file.path(homeDir,"portfolios.zip")
files <- file.path(homeDir,mappingPippoPortfolio)
ok <- zip(zipFullFileName, files, flags = "-r9X")
## -- terminato salvataggio portafogli



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





