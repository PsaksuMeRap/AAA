# TODO: Add comment
# 
# Author: Claudio
###############################################################################

rm(list=ls(all=TRUE))
options(browser="firefox")
options(help_type="html")

library("RODBC")
home <- "/home/claudio/workspace/AAA/VAR/"
setwd(home)

stringsAsFactors = FALSE
repositories <- new.env()

source("./odbc/connessioni.R")

## -- funzione che restituisce il NAV dei tre fondi
connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)


getFundNavGlobalEquity <- function(date) {
	# date: la data in formato "yyyy-mm-dd"
	
	# selezione i campi Ticker, Close e TRADE_DATE
	query <- paste("SELECT * FROM [Prezzi storici azioni].dbo.Fondi ",
			"WHERE Ticker='2742261CH' AND ",
			"TRADE_DATE ='",date,"'")
	dati <- sqlQuery(connection,query,as.is=TRUE)
	print(paste(date,"GlobalEquity",dati[,"Close"]))
}

getFundNavGlobalEconomy <- function(date) {
	# date: la data in formato "yyyy-mm-dd"
	
	# selezione i campi Ticker, Close e TRADE_DATE
	query <- paste("SELECT * FROM [Prezzi storici azioni].dbo.Fondi ",
			"WHERE Ticker='11995588CH' AND ",
			"TRADE_DATE ='",date,"'")
	dati <- sqlQuery(connection,query,as.is=TRUE)
	print(paste(date,"GlobalEconom",dati[,"Close"]))
}

getFundNavEuroFixedIncome <- function(date) {
	# date: la data in formato "yyyy-mm-dd"
	
	# selezione i campi NumeroValore, Date e Price
	query <- paste("SELECT * FROM [Prezzi storici reddito fisso].dbo.PrezziStorici ",
			"WHERE NumeroValore='2490099' AND ",
			"Date ='",date,"'")
	dati <- sqlQuery(connection,query,as.is=TRUE)
	print(paste(date,"EuroFixedInc",dati[,"Price"]/100))
}

getFundNav <- function(date=defaultDate()) {

	r1 <- sapply(date,getFundNavGlobalEquity)
	print("----------")
	r2 <- sapply(date,getFundNavEuroFixedIncome)
	print("----------")	
	r3 <- sapply(date,getFundNavGlobalEconomy)

}


defaultDate <- function() {
	dataOdierna <- Sys.Date()
	if (format(Sys.Date()-1, "%a")=="Sun") return(Sys.Date()-3)
	if (format(Sys.Date()-1, "%a")=="Sat") return(Sys.Date()-2)
	return(Sys.Date()-1)
}

getFundNav("2011-11-21")

