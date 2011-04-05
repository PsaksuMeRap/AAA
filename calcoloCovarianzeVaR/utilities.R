# TODO: Add comment
# 
# Author: claudio
###############################################################################


businessDaysDaily <- function(from="2000-01-01",to="2030-12-31") {
	
	startDate <- as.Date(from)
	endDate <- as.Date(to)
	dates <- seq.Date(from=startDate,to=endDate,by="day")
	remove <- weekdays(dates) == "Saturday" | weekdays(dates) == "Sunday"
	hold <- !remove
	return(as.character(dates[hold]))
	
}

querySelezioneTickers <- function(tabella, nomeCampoData="TRADE_DATE",from="",to="") {
	if (tabella=="TotaleTassiStorico") {
		tabella <- paste("[Tassi storici (VAR)].dbo.",tabella,sep="")
		query <- paste(
				"INSERT INTO [tempdb].dbo.Tickers (Ticker) ",
				"SELECT DISTINCT Moneta + Scadenza AS Ticker FROM ",
				tabella,
				" WHERE [Date] >= '", from, "' AND [Date] <= '",to,"'",
				sep=""
		)		
		
	} else {
		tabella <- paste("[Prezzi storici azioni (VAR)].dbo.",tabella,sep="")
		query <- paste(
				"INSERT INTO [tempdb].dbo.Tickers (Ticker) ",
				"SELECT DISTINCT Ticker FROM ",
				tabella,
				" WHERE ", nomeCampoData, " >= '", from, "' AND ",
				nomeCampoData, " <= '",to,"'",sep=""
		)
	}
	
	return(query)
}

querySelezioneTickersData <- function() {
	query <- paste(
			"INSERT INTO [tempdb].dbo.Tickers_data (Ticker,Data)",
			"SELECT A.Ticker,B.Data FROM Tickers A CROSS JOIN busDays B"
	)
	return(query)
}


queryForImporting <- function(tabella,nomeCampoData="TRADE_DATE") {
	if (tabella=="TotaleTassiStorico") {
		query <- paste(
				"SELECT A.Ticker, A.Data, B.Interpolazione_tasso ",
				"FROM Tickers_data A ",
				"LEFT OUTER JOIN [Tassi storici (VAR)].dbo.TotaleTassiStorico B ",
				"ON A.Data=B.[Date] AND A.Ticker = B.Moneta + B.Scadenza ",
				"ORDER BY A.Ticker, A.Data",
				sep=""
		)
	} else {
		tabella <- paste("[Prezzi storici azioni (VAR)].dbo.",tabella,sep="")
		query <- paste(
				"SELECT A.Ticker, A.Data, B.[Close] ",
				"FROM Tickers_data A ",
				"LEFT OUTER JOIN ", tabella, " B ",
				"ON A.Data=B.",nomeCampoData, " AND A.Ticker=B.Ticker ",
				"ORDER BY A.Ticker, A.Data",
				sep=""
		)
	}
	


	return(query)
}

# queste due funzioni sono state prese dal file "funzioni utilit tmp.r del progetto
# "Costruzione indici"
{
	replaceZeroWithNA_real_ <- function(vectorOfReals) {
		# identifica 0. se in posizione "i" ci sono NA, isZero[i] sara' uguale a NA
		isZero <- vectorOfReals == 0
		# sostituisci NA con FALSE nel vettore isZero
		isZero[is.na(isZero)] <- FALSE
		if (any(isZero))  vectorOfReals[isZero] <- NA
		return(vectorOfReals)
	}
	
	computePercReturns <- function(prezzi.df) {
		matrice_prezzi <- sapply(prezzi.df,replaceZeroWithNA_real_)
		dimnames(matrice_prezzi) <- dimnames(prezzi.df)
		nbObsReturns <- nrow(matrice_prezzi) - 1
		matrice_rendimenti = (matrice_prezzi[-1,] - matrice_prezzi[1:nbObsReturns,]) / matrice_prezzi[1:nbObsReturns,]
		return(as.data.frame(matrice_rendimenti))
	}
	
	computeLogReturns <- function(prezzi.df) {
		names <- dimnames(prezzi.df)
		matrice_prezzi <- sapply(prezzi.df,replaceZeroWithNA_real_)
		dimnames(matrice_prezzi) <- dimnames(prezzi.df)
		nbObsReturns <- nrow(matrice_prezzi) - 1
		matrice_rendimenti = log(matrice_prezzi[-1,]/matrice_prezzi[1:nbObsReturns,])
		return(as.data.frame(matrice_rendimenti))
	}
}