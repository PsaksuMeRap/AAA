# TODO: Add comment
# 
# Author: Claudio Ortelli
###############################################################################

rm(list=ls(all=TRUE))
options(browser="google-chrome")
options(help_type="html")

library(RODBC)
library(tcltk)
source ("sub_db_utilities.R")
source ("utilities.R")

# Viene impostata la connessione al database "Prezzi storici azioni (VAR)"
source("connessioni.R")

## crea la funzione per la scrittura del log dei files
writeLog <- function(txt,file=log_file,append=TRUE) {
	cat (txt, file=log_file,append=append)
}

# Crea il file di log
log_file = paste("log",substr(as.character(Sys.time()),1,10), substr(as.character(Sys.time()),12,19),sep="_")
log_file = paste(log_file,".xml",sep="")
log_file = gsub("-","_",log_file,perl=TRUE)
log_file = gsub(":","_",log_file,perl=TRUE)
writeLog ("<start>Stima della matrice di varianza covarianza\n", append=FALSE)

# Registra l'ora d'inizio
writeLog (paste("<time>",date(),"</time>\n",sep=""))


# Definisci la data iniziale e finale: "yyyy-mm-dd" e' il formato da utilizzare
data.inizio = "2009-07-26"
data.fine   = "2010-07-27"

Dates <- businessDaysDaily(from=data.inizio,to=data.fine)

# importa le date nella tabella busDays
{
	# rimuovi la tabella busDays nel tempdb
	if (tableExist(myConnection=db.tempdb,tableName="busDays",databaseName="tmpdb")) {
		drop.table("busDays", db.tempdb)
		writeLog ("<sql>drop.table('busDays', db.tempdb)</sql>\n")
	}
	# crea la tabella busDays nel tempdb	
	query = paste("CREATE TABLE busDays (Data smalldatetime NOT NULL PRIMARY KEY) ")
	writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))
	
	risultato.query = sqlQuery(db.tempdb, query)
	writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
	rm(query)
	
	# inserisci le date
	queries = paste("INSERT INTO busDays (Data) VALUES('",Dates,"')",sep="")
	for (query in queries) {sqlQuery(db.tempdb, query)}
	rm(queries)
}

# esegui un loop su tutte le tabelle nel vettore tabelle
{
	prezzi.df <- data.frame(Dates=Dates)
	rownames(prezzi.df) <- Dates
	
	tabelle = c("TotalePrezziStorico","TotaleCambiStorico",
			"TotaleIndiciStorico","TotaleTassiStorico")
	

	for (tabella in tabelle) {
		# rimuovi la tabella Tickers nel tempdb
		if (tableExist(myConnection=db.tempdb,tableName="Tickers",databaseName="tmpdb")) {
			drop.table("Tickers", db.tempdb)
			writeLog ("<sql>drop.table('Tickers', db.tempdb)</sql>\n")
		}
		# crea la tabella Tickers nel tempdb	
		query = paste("CREATE TABLE Tickers (Ticker varchar(20) NOT NULL PRIMARY KEY) ")
		writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))
		
		risultato.query = sqlQuery(db.tempdb, query)
		writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
		rm(query)
		
		# popola la tabella con quei Ticker aventi osservazioni nel periodo
		query <- querySelezioneTickers(tabella,from=data.inizio,to=data.fine)
		risultato.query = sqlQuery(db.tempdb, query)
		
		
		## crea la tabella Data_tickers con i campi Data,Ticker
		{
			# rimuovi la tabella Tickers_data nel tempdb
			if (tableExist(myConnection=db.tempdb,tableName="Tickers_data",databaseName="tmpdb")) {
				drop.table("Tickers_data", db.tempdb)
				writeLog ("<sql>drop.table('Tickers_data', db.tempdb)</sql>\n")
			}
			# crea la tabella Tickers_data nel tempdb	
			query = paste(
					"CREATE TABLE Tickers_data (Ticker varchar(20) NOT NULL,",
					"Data smalldatetime NOT NULL,",
					"CONSTRAINT Tickers_data_pk PRIMARY KEY (Ticker,Data))"
			)
			writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))
			
			risultato.query = sqlQuery(db.tempdb, query)
			
			# popola la tabella Tickers_data
			query <- querySelezioneTickersData()
			risultato.query = sqlQuery(db.tempdb, query)
		}
		
		## crea il data.frame con le serie storiche della tabella "TotalePrezziStorico"
		{
			query <- queryForImporting(tabella)
			risultato.query <- sqlQuery(db.tempdb, query)
		}
		
		prezzi.df <-cbind(prezzi.df,split(risultato.query[,3],risultato.query[,1]))
		print(paste("Terminato tabella",tabella))
	} # fine for
}

# ripulisci le tabelle non più necessarie
{
	# rimuovi la tabella busDays nel tempdb
	if (tableExist(myConnection=db.tempdb,tableName="busDays",databaseName="tmpdb")) {
		drop.table("busDays", db.tempdb)
		writeLog ("<sql>drop.table('busDays', db.tempdb)</sql>\n")
	}
	# rimuovi la tabella Tickers nel tempdb
	if (tableExist(myConnection=db.tempdb,tableName="Tickers",databaseName="tmpdb")) {
		drop.table("Tickers", db.tempdb)
		writeLog ("<sql>drop.table('Tickers', db.tempdb)</sql>\n")
	}
	# rimuovi la tabella Tickers_data nel tempdb
	if (tableExist(myConnection=db.tempdb,tableName="Tickers_data",databaseName="tmpdb")) {
		drop.table("Tickers_data", db.tempdb)
		writeLog ("<sql>drop.table('Tickers_data', db.tempdb)</sql>\n")
	}
}


prezzi.df[["Dates"]] <- NULL
returns.df <- computeLogReturns(prezzi.df)

# calcola le covarianze
S <- cov(returns.df,use="pairwise.complete.obs")


{
	# rimuovi la tabella Covarianze_da_R_tmp nel tempdb
	if (tableExist(myConnection=db.tempdb,tableName="Covarianze_da_R_tmp",databaseName="tmpdb")) {
		drop.table("Covarianze_da_R_tmp", db.tempdb)
		writeLog ("<sql>drop.table('Tickers', db.tempdb)</sql>\n")
	}
	create.cov.table(table="Covarianze_da_R_tmp", channel=db.tempdb)
	put.cov(Sigma=S, table="Covarianze_da_R_tmp", channel=db.tempdb)
}


# chiudi il log
{
	## Registra data di fine
	writeLog (paste("<time>",date(),"</time>\n",sep=""))
	writeLog ("</start>\n")
}