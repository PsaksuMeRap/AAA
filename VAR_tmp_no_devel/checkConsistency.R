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
clienti <- c("pippo76","pippo53","pippo210",
		"pippo10"
		,"pippo102"
		,"pippo104"
		,"pippo109"
		,"pippo11"
		,"pippo110"
		,"pippo116"
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
portfParser <- create_parserPortfolio()
portfolios <- lapply(clienti,portfParser$parse,dati)

# controlla la consistenza
outputs <- lapply(portfolios,extractUnconsistentPortfolioPositions)

# crea la connessione
# logFile <- "C:/R/risultato.txt"
logFile <- "risultato.txt"
con <- file(description=logFile,open="w")

# stampa i risultati
if (length(outputs)>0) {
	for (output in outputs) {
		if (length(output[[1]])>0) {
			cat(paste("Il portafoglio cliente",names(output),"ha problemi:"),file=logFile,sep="\n",append=TRUE)
			for (position in output[[1]]) cat(position$toString(),file=logFile,sep="\n",append=TRUE)
			cat("\n",file=logFile,sep="\n",append=TRUE)
		}
	}
}




