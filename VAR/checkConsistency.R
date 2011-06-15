# TODO: Add comment
# 
# Author: claudio
###############################################################################

rm(list=ls(all=TRUE))

library("RODBC")
home <- "/home/claudio/eclipse/AAA/VAR/"
# home <- "C:/R"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()
source("./lib/library.R")
source("./odbc/connessioni.R")

# inizio procedura controllo

# importa i dati dal DB
dati <- importDBPortfolioGenerale()

# crea i vari repositories
repositories$fixedIncome <- create_repositoryFixedIncome()
repositories$politicaInvestimento <- create_repositoryPoliticaInvestimento()
repositories$instruments <- create_repositoryInstruments()
repositories$exchangeRates <- create_repositoryExchangeRates()
repositories$equities <- create_repositoryEquities()

# definisci la lista di clienti da controllare
clienti <- c("pippo76","pippo53","pippo210")
portfParser <- create_parserPortfolio()
portfolios <- lapply(clienti,portfParser$parse,dati)

# controlla la consistenza
outputs <- lapply(portfolios,extractUnconsistentPortfolioPositions)

# crea la connessione
logFile <- "C:/R/risultato.txt"
con <- file(description=logfile,open="w")

# stampa i risultati
if (length(outputs)>0) {
	for (output in outputs) {
		if (length(output[[1]])>0) {
			cat(paste("Il portafoglio cliente",names(output),"ha problemi:"),file=logFile,sep="\n",append=TRUE)
			for (position in output[[1]]) cat(position$toString(),file=logFile,sep="\n",append=TRUE)
		}
		cat("\n",file=logFile,sep="\n",append=TRUE)
	}
}




