# TODO: Add comment
# 
# Author: claudio
###############################################################################

importCashflowsAdjustedReturns <- function(startDate) {
	# startDate: a date string yyyy-mm-dd
	
	startDate <- "2010-05-02"
	connection <- odbcConnect("Performance_TW",utente,password)
	if (missing(startDate)) {
		query = paste("SELECT B.ID AS Cliente, A.Data2 AS Data, A.Return_giornaliero",
				"FROM [Performance_TW].dbo.Serie_returns_giornalieri A",
				"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente",
				"ORDER BY B.ID, A.Data2")
	} else {
		query = paste("SELECT B.ID AS Cliente, A.Data2 AS Data, A.Return_giornaliero ",
				"FROM [Performance_TW].dbo.Serie_returns_giornalieri A ",
				"INNER JOIN [Sistema (prova)].dbo.Clienti_ID B ON A.Cliente=B.Cliente ",
				paste("WHERE A.Data2>='",startDate,"' ",sep=""),
				"ORDER BY B.ID, A.Data2")	
	}
	stringsAsFactors = TRUE
	DBPortfolioGenerale.df <- sqlQuery(connection,query,as.is=TRUE)
	DBPortfolioGenerale.df[["Data"]] <- as.character(as.Date(DBPortfolioGenerale.df[["Data"]]))
	DBPortfolioGenerale.l <- split(DBPortfolioGenerale.df,DBPortfolioGenerale.df[,"Cliente"])
	
	if (length(DBPortfolioGenerale.l)==0) return(list())
	
	toTimeSeries <- function(df) {
		name <- df[1,"Cliente"]
		data <- df[,"Return_giornaliero"]
		names(data) <- df[,"Data"]
		ts <- create_timeSeries(name,data,type="percentage_returns")
		return(ts)
	}
	
	timeSeries.l <- lapply(DBPortfolioGenerale.l,toTimeSeries)

	return(timeSeries.l)
}
