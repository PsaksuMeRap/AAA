# TODO: Add comment
# 
# Author: claudio
###############################################################################

importDBPortfolioGenerale <- function() {
	
	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	query = paste("SELECT B.ID, A.* FROM [Sistema (prova)].dbo.DBPortfolioGenerale A",
			"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente")
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	
	DBPortfolioGenerale.df[["Cliente"]] <- NULL
	colnames(DBPortfolioGenerale.df)[1] <- "Cliente"
	getRow <- function(i,df) { 
		x <- df[i,,drop=TRUE]
		ayrtonPosition <- new("AyrtonPosition",
				Cliente=x[["Cliente"]],Strumento=x[["Strumento"]],Moneta=x[["Moneta"]],
				Saldo=x[["Saldo"]],Nome=x[["Nome"]],ValoreMercatoMonetaCHF=x[["ValoreMercatoMonetaCHF"]],
				ID_AAA=x[["ID_AAA"]],ID_strumento=x[["ID_strumento"]])
		return(ayrtonPosition)
	}
	
	if (nrow(DBPortfolioGenerale.df)==0) return(origin=new("AyrtonPositions",positions=list()))
	origin <- lapply(1:nrow(DBPortfolioGenerale.df),getRow,DBPortfolioGenerale.df)
	return(origin=new("AyrtonPositions",origin))
	return(origin)
}


importDBPortfolioGeneraleByDate <- function(fetchDate) {

	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	query = paste("SELECT B.ID, A.* FROM [Performance_TW].dbo.Valori_storici_portafogli_disaggregati A ",
			"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente ",
			"WHERE A.data='",fetchDate,"'")
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	
	DBPortfolioGenerale.df[["Cliente"]] <- NULL
	colnames(DBPortfolioGenerale.df)[1] <- "Cliente"
	getRow <- function(i,df) { 
		x <- df[i,,drop=TRUE]
		ayrtonPosition <- new("AyrtonPosition",
				Cliente=x[["Cliente"]],Strumento=x[["Strumento"]],Moneta=x[["Moneta"]],
				Saldo=x[["Saldo"]],Nome=x[["Nome"]],ValoreMercatoMonetaCHF=x[["ValoreMercatoMonetaCHF"]],
				ID_AAA=x[["ID_AAA"]],ID_strumento=x[["ID_strumento"]])
		return(ayrtonPosition)
	}
	
	if (nrow(DBPortfolioGenerale.df)==0) return(origin=new("AyrtonPositions",positions=list()))
	origin <- lapply(1:nrow(DBPortfolioGenerale.df),getRow,DBPortfolioGenerale.df)
	return(origin=new("AyrtonPositions",origin))
	return(origin)
}


importDBPortfolioGeneraleDataFrame <- function() {
	
	connection <- odbcConnect("prezzi_storici_azioni_VAR",.utente,.password)
	query = paste("SELECT B.ID, A.* FROM [Sistema (prova)].dbo.DBPortfolioGenerale A",
			"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente")
	
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	
	DBPortfolioGenerale.df[["Cliente"]] <- NULL
	colnames(DBPortfolioGenerale.df)[1] <- "Cliente"

	return(DBPortfolioGenerale.df)
}


importaDaCvs <- function(fileName,workDir,directory) {
	file <- paste(workDir,"/",directory,"/",fileName,sep="")

	# verifica che ci siano dati nel file
	dati <- scan(file=file, what=character(0), sep="\n",nlines = 7,
			quiet=TRUE,encoding="ISO-8859-1")
    dati <- iconv(dati)

	if ((length(grep(pattern="NO DATA TO RETURN",x=dati[7],useBytes=TRUE))>0 ) |
		(length(grep(pattern="NO BOND DATA",x=dati[7],useBytes=TRUE))>0)) {
		state = c(fileName,"no data")
		return(list(isin="",state=state))
	}
	
	# importa l'isin
	dati <- read.csv(file=file,as.is=TRUE,nrows=7,
			colClasses="character",header=FALSE)

	isin <- dati[5,2]
	# elimina il testo "(P)" presente nel file delle azioni
	isin <- sub(pattern="\\(P\\)$",replacement="",x=isin)
	
	# verifica che la serie non sia stata sospesa
	if (length(grep(pattern="\\(SUSP\\)",x=dati[4,2],useBytes=TRUE))>0)  {
		state = c(fileName,"suspended")
		return(list(isin="",state=state))
	}

	# importa i dati
	state = c(fileName,"ok")
	dati <- read.csv(file=file,as.is=TRUE,skip=6,
			colClasses=c("character","numeric"),header=FALSE)
	colnames(dati) <- c("Data","Prezzo")
	dati[["Data"]] <- as.character(as.Date(dati[["Data"]], "%m/%d/%Y"))

	return(list(isin=isin,state=state,dati=dati))
}


importaAzioniInBancaDati <- function(x) {
	getTickerFromIsin <- function(){
		# questa funzione esegue una query sul database "prezzi storici azioni (VAR)"
		# per determinare il ticker corrispondente all'ISIN dato
		
		query = "SELECT Ticker FROM universo_azioni WHERE MNEM ='"
		query = paste(query,x$isin,"'",sep="")
		
		risultato.query = sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		if (nrow(risultato.query)!=1) {
			return (NA_character_)
		}
		return(risultato.query[1,1])
	}

	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	ticker <- getTickerFromIsin()
	
	if (is.na(ticker)) {
		print(paste("Nella banca dati il codice mnem:",x$isin,"non esiste."))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
		
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotalePrezziStorico WHERE Ticker = '"
        query2 <- ticker
		query3 <- paste("' AND TRADE_DATE >= '",dataInizio,sep="")
		query4 <- paste("' AND TRADE_DATE <= '",dataFine,"'",sep="")
		query <- paste(query1,query2,query3,query4,sep="")
		
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
	
		# estendi il data.frame col campo Ticker
		dati <- data.frame(Ticker=ticker,dati)

		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiAzioniDaR",
				rownames=FALSE
		)
		

		# inserisci nella tabella TotalePrezziStorico
		query <- paste("INSERT INTO TotalePrezziStorico",
				"            (Ticker, TRADE_DATE, [Close])",
				"SELECT Ticker, CONVERT(nvarchar,Data,5) as TRADE_DATE,",
				"Prezzo FROM [tempdb].dbo.datiAzioniDaR "
		)
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# elimina la tabella datiAzioniDaR
		query <- "DROP TABLE datiAzioniDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",ticker))
	}
	
	return(TRUE)	
}

importaIndiciAzioniInBancaDati <- function(x) {
	getTickerFromIsin <- function(){
		# questa funzione esegue una query sul database "prezzi storici azioni (VAR)"
		# per determinare il ticker corrispondente all'ISIN dato
		
		query = "SELECT Ticker FROM universo_indici_azioni WHERE ISIN ='"
		query = paste(query,x$isin,"'",sep="")
		
		risultato.query = sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		if (nrow(risultato.query)!=1) {
			return (NA_character_)
		}
		return(risultato.query[1,1])
	}
	
	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	ticker <- getTickerFromIsin()
	
	if (is.na(ticker)) {
		print(paste("Nella banca dati l'isin:",x$isin,"non esiste."))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
		
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotaleIndiciStorico WHERE Ticker = '"
		query2 <- ticker
		query3 <- paste("' AND TRADE_DATE >= '",dataInizio,sep="")
		query4 <- paste("' AND TRADE_DATE <= '",dataFine,"'",sep="")
		query <- paste(query1,query2,query3,query4,sep="")
		
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# estendi il data.frame col campo Ticker
		dati <- data.frame(Ticker=ticker,dati)
		
		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiIndiciAzioniDaR",
				rownames=FALSE
		)
		
		
		# inserisci nella tabella TotalePrezziStorico
		query <- paste("INSERT INTO TotaleIndiciStorico",
				"            (Ticker, TRADE_DATE, [Close])",
				"SELECT Ticker, CONVERT(nvarchar,Data,5) as TRADE_DATE,",
				"Prezzo FROM [tempdb].dbo.datiIndiciAzioniDaR "
		)
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# elimina la tabella datiIndiciAzioniDaR
		query <- "DROP TABLE datiIndiciAzioniDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",ticker))
	}
	
	return(TRUE)	
}


importaCambiInBancaDati <- function(x) {
	getTickerFromIsin <- function(){
		# questa funzione esegue una query sul database "prezzi storici azioni (VAR)"
		# per determinare il ticker corrispondente all'ISIN dato

		query = "SELECT Ticker FROM universo_cambi WHERE ISIN ='"
		query = paste(query,x$isin,"'",sep="")
		
		risultato.query = sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		if (nrow(risultato.query)!=1) {
			return (NA_character_)
		}
		return(risultato.query[1,1])
	}
	
	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	ticker <- getTickerFromIsin()
	
	if (is.na(ticker)) {
		print(paste("Nella banca dati l'isin:",x$isin,"non esiste."))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
	
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotaleCambiStorico WHERE Ticker = '"
		query2 <- ticker
		query3 <- paste("' AND TRADE_DATE >= '",dataInizio,sep="")
		query4 <- paste("' AND TRADE_DATE <= '",dataFine,"'",sep="")
		query <- paste(query1,query2,query3,query4,sep="")
		
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# estendi il data.frame col campo Ticker
		dati <- data.frame(Ticker=ticker,dati)
		
		# inverti il cambio
		dati[["Prezzo"]] <- 1/dati[["Prezzo"]]
		
		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiCambiDaR",
				rownames=FALSE
		)
		
		
		# inserisci nella tabella TotalePrezziStorico
		query <- paste("INSERT INTO TotaleCambiStorico",
				"            (Ticker, TRADE_DATE, [Close])",
				"SELECT Ticker, CONVERT(nvarchar,Data,5) as TRADE_DATE,",
				"Prezzo FROM [tempdb].dbo.datiCambiDaR "
		)
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# elimina la tabella datiCambiDaR
		query <- "DROP TABLE datiCambiDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",ticker))
	}
	
	return(TRUE)	
}


importaFondiSenzaProxyInBancaDati <- function(x) {
	getTickerFromIsin <- function(){
		# questa funzione esegue una query sul database "prezzi storici azioni (VAR)"
		# per determinare il ticker corrispondente all'ISIN dato
		
		query = "SELECT Ticker FROM universo_fondi_senza_proxy WHERE ISIN ='"
		query = paste(query,x$isin,"'",sep="")
		
		risultato.query = sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		if (nrow(risultato.query)!=1) {
			return (NA_character_)
		}
		return(risultato.query[1,1])
	}
	
	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	ticker <- getTickerFromIsin()
	
	if (is.na(ticker)) {
		print(paste("Nella banca dati l'isin:",x$isin,"non esiste."))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
		
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotaleFondiSenzaProxyStorico WHERE Ticker = '"
		query2 <- ticker
		query3 <- paste("' AND TRADE_DATE >= '",dataInizio,sep="")
		query4 <- paste("' AND TRADE_DATE <= '",dataFine,"'",sep="")
		query <- paste(query1,query2,query3,query4,sep="")
		
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# estendi il data.frame col campo Ticker
		dati <- data.frame(Ticker=ticker,dati)
		
		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiFondiSenzaProxyDaR",
				rownames=FALSE
		)
		
		
		# inserisci nella tabella TotalePrezziStorico
		query <- paste("INSERT INTO TotalefondiSenzaProxyStorico",
				"            (Ticker, TRADE_DATE, [Close])",
				"SELECT Ticker, CONVERT(nvarchar,Data,5) as TRADE_DATE,",
				"Prezzo FROM [tempdb].dbo.datiFondiSenzaProxyDaR "
		)
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# elimina la tabella datiIndiciAzioniDaR
		query <- "DROP TABLE datiFondiSenzaProxyDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",ticker))
	}
	
	return(TRUE)	
}


importaMetalliPreziosiInBancaDati <- function(x) {
	getTickerFromIsin <- function(){
		# questa funzione esegue una query sul database "prezzi storici azioni (VAR)"
		# per determinare il ticker corrispondente all'ISIN dato

		query = "SELECT Ticker FROM universo_cambi WHERE ISIN ='"
		query = paste(query,x$isin,"'",sep="")
		
		risultato.query = sqlQuery(db.prezziStoriciVAR, query,as.is=TRUE)
		if (nrow(risultato.query)!=1) {
			print(paste("Errore isin non trovato:",x$isin))
			return (NA_character_)
		}
		return(risultato.query[1,1])
	}
	
	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	ticker <- getTickerFromIsin()
	
	if (is.na(ticker)) {
		print(paste("Nella banca dati l'isin:",x$isin,"non esiste."))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
		
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotaleCambiStorico WHERE Ticker = '"
		query2 <- ticker
		query3 <- paste("' AND TRADE_DATE >= '",dataInizio,sep="")
		query4 <- paste("' AND TRADE_DATE <= '",dataFine,"'",sep="")
		query <- paste(query1,query2,query3,query4,sep="")
		
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# estendi il data.frame col campo Ticker
		dati <- data.frame(Ticker=ticker,dati)
		
		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiCambiDaR",
				rownames=FALSE
		)
		
		
		# inserisci nella tabella TotalePrezziStorico
		query <- paste("INSERT INTO TotaleCambiStorico",
				"            (Ticker, TRADE_DATE, [Close])",
				"SELECT Ticker, CONVERT(nvarchar,Data,5) as TRADE_DATE,",
				"Prezzo FROM [tempdb].dbo.datiCambiDaR "
		)
		risultato.query <- sqlQuery(db.prezziStoriciVAR, query)
		
		# elimina la tabella datiCambiDaR
		query <- "DROP TABLE datiCambiDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",ticker))
	}
	
	return(TRUE)	
}

importaTassiInteresseInBancaDati <- function(x) {
	getStructureFromIsin <- function(){
		# questa funzione costruisce una lista con i dati necessari 
		# all'inserimento dei dati nella tabella totaleTassiStorico

		# determina se si tratta di un libor (BB) o di uno swap
		code  <- substr(x$isin,1,2)
		money <- substr(x$isin,3,5)
		sub6  <- substr(x$isin,6,6)
		sub7  <- substr(x$isin,7,7)
		
		# gli swap hanno quale id moneta per l'euro il codice
		# "EIB". Sostituiscilo con "EUR"
		if (money=="EIB") money <- "EUR"

		if (code == "BB") { # libor
			if (sub7 == "W") {
				Scadenza1 <- as.numeric(sub6)*0.25
				Scadenza  <- paste(sub6,"W",sep="")
				return(list(money=money,Scadenza=Scadenza,
								Scadenza1=Scadenza1))
			}
			if (sub7 == "M") {
				Scadenza1 <- as.numeric(sub6)
				Scadenza  <- paste(Scadenza1,"M",sep="")
				return(list(money=money,Scadenza=Scadenza,
								Scadenza1=Scadenza1))
			}
			sub67 <- substr(x$isin,6,7)
			Scadenza1 <- as.numeric(sub67)
			if (Scadenza1 < 12) {
				Scadenza  <- paste(Scadenza1,"M",sep="")
			} else {
				Scadenza  <- "1Y"
			}
			return(list(money=money,Scadenza=Scadenza,
							Scadenza1=Scadenza1))		
		}
		if (code == "IC") { # swap
			if (sub7=="Y") {
				Scadenza1 <- as.numeric(sub6)*12
				Scadenza  <- paste(sub6,"Y",sep="")
				return(list(money=money,Scadenza=Scadenza,
								Scadenza1=Scadenza1))
			}
			sub67 <- substr(x$isin,6,7)
			Scadenza1 <- as.numeric(sub67)*12
			Scadenza  <- paste(sub67,"Y",sep="")
			return(list(money=money,Scadenza=Scadenza,
							Scadenza1=Scadenza1))	
		}

		return(NA)
	}
	
	
	if (x$state[2] != "ok") {
		print(paste("Attenzione, lo stato della serie nel file ",
						x$state[1]," è ",x$state[2],sep=""))
		return(FALSE)
	}
	
	y <- getStructureFromIsin()
	
	if (!is.list(y)) {
		print(paste("Errore importazione tassi interesse:",x$isin))	
		return(FALSE)
	}
	
	osservazioniValide <- !is.na(x$dati[,"Prezzo"])
	
	if (any(osservazioniValide)) {
		
		# rimuovi dal DB tutte le osservazioni comprese nell'intervallo
		# data minima disponibile, data massima disponibile
		
		dati <- x$dati[osservazioniValide,]
		dataInizio <- dati[1,"Data"]
		dataFine <- dati[nrow(x$dati),"Data"]
		
		dateValide <- dati[,"Data"]
		
		query1 <- "DELETE FROM TotaleTassiStorico WHERE Moneta = '"
		query2 <- y$money
		query3 <- paste("' AND [Date] >= '",dataInizio,sep="")
		query4 <- paste("' AND [Date] <= '",dataFine,sep="")
		query5 <- paste("' AND Scadenza1 = ",y$Scadenza1,sep="")
		query <- paste(query1,query2,query3,query4,query5,sep="")
		
		risultato.query <- sqlQuery(db.tassiStoriciVAR, query)
		
		# estendi il data.frame col campo Moneta, Scadenza, Scadenza1
		dati <- data.frame(Moneta=y$money,Scadenza=y$Scadenza,Scadenza1=y$Scadenza1,dati)
		
		risultato.query <- sqlSave(
				channel = db.tempdb,
				dat     = dati,
				tablename="datiTassiDaR",
				rownames=FALSE
		)
		
		
		# inserisci nella tabella TotaleTassiStorico
		query <- paste("INSERT INTO TotaleTassiStorico",
				"            (Moneta, [Date], Scadenza, Scadenza1, Tasso)",
				"SELECT Moneta, CONVERT(nvarchar,Data,5) as [Date], Scadenza,",
				"Scadenza1, Prezzo",
				"FROM [tempdb].dbo.datiTassiDaR "
		)
		risultato.query <- sqlQuery(db.tassiStoriciVAR, query)
		
		# elimina la tabella datiCambiDaR
		query <- "DROP TABLE datiTassiDaR"
		risultato.query <- sqlQuery(db.tempdb, query)
	} else {
		print(paste("Nessuna osservazione valida per",x$isin))
	}
	
	return(TRUE)	
}
