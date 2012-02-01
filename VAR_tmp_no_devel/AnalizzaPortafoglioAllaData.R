# TODO: Add comment
# 
# Author: claudio
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

clienti <- c("pippo53","pippo76","pippo210")
date <- as.Date("2011-06-30")

dati <- importDBPortfolioGeneraleByDate(date)

repositories$fixedIncome <- create_repositoryFixedIncome()
repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$equities <- create_repositoryEquities()
repositories$exchangeRates <- create_repositoryExchangeRates(exchangeRatesDate=date)		

portfParser <- create_parserPortfolio()
portfolios <- lapply(clienti,portfParser$parse,dati)

