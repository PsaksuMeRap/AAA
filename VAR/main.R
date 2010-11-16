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

source("./lib/library.R")

stringsAsFactors = FALSE
repositories <- new.env()


# inizio procedura controllo
source("./odbc/connessioni.R")


dati <- importDBPortfolioGenerale()
repositories$fixedIncome <- create_repositoryFixedIncome()
repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()
repositories$equities <- create_repositoryEquities()
		

clienti <- c("pippo53","pippo76")
portfParser <- create_parserPortfolio()
portafogli <- lapply(clienti,portfParser$parse,dati)

home <- "/home/claudio/"
setwd(home)


