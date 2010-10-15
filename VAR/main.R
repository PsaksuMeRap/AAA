# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library("RODBC")

home <- "/home/claudio/eclipse/AAA/VAR/"

source("./odbc/connessioni.R")
source("./lib/library.R")

stringsAsFactors = FALSE
repositories <- new.env()

dati.df <- importDBPortfolioGenerale()
clienti <- unique(dati.df[,"Cliente"])
portfParser <- create_parserPortfolio()
portafogli <- lapply(clienti,portfParser$parse,dati.df)



