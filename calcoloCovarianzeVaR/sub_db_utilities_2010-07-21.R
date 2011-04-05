sqlGetErrMsg <- function(channel)
  {
    msg <- odbcGetErrMsg(channel)
    if (length(msg)>0)
      {
        tmp <- paste(msg,collapse="\n")
        tkmessageBox(message=paste("ODBC Error:\n",tmp,type="ok",icon="error"))
        odbcClearError(channel)
        return(1)
      }
    odbcClearError(channel)
    return(0)
  }


sqlWorkingDatabase <- function(myConnection)
  { 
    ## return the name of the active database
    return(sqlQuery(channel=myConnection,query="SELECT DB_NAME()",as.is=TRUE)[1,1])
  }


tableExist <- function(myConnection,tableName,databaseName,dbOwner="[dbo]")
  { 
    if(!missing(databaseName)) ## change database
      {
        ## extract the actual database
        initialDb <- sqlWorkingDatabase(myConnection)
    
        ## remove squared brackets if exist
        database <- gsub("\\[|\\]","",databaseName)

        sqlQuery(channel=myConnection,query=paste("USE [",database,"]",sep=""))      
        sqlGetErrMsg(channel=myConnection) 
      }

    query = paste("SELECT * from dbo.sysobjects WHERE id = object_id(N'",
                  dbOwner,".[",tableName,"]') AND OBJECTPROPERTY(id, N'IsUserTable')=1",sep="")
    
    result <- sqlQuery(myConnection,query)

    if (!missing(databaseName))
      {
        sqlQuery(channel=myConnection,query=paste("USE [",initialDb,"]",sep=""))
        sqlGetErrMsg(channel=myConnection)
      }
    
    if (nrow(result)>0) return(TRUE) else return(FALSE) 
  }



# Questa subroutine rimuove la tabella specificata del db specificato
drop.table <- function (table,channel,errorText=paste("drop.table(",table,")")) {
  query  <- paste("DROP TABLE ", table, sep="")
  result <- sqlCommand(channel,query,errorText)
  return (invisible(result))
}


# this function delete the "table" from a database
delete.table <- function (table,channel,errorText=paste("delete.table(",table,")")) {
  query  <- paste("DELETE FROM ", table, sep="")
  result <- sqlCommand(channel,query,errorText)
  return (invisible(result))
}

sqlCommand <- function(channel,query,errorText="")
{
  result <- odbcQuery(channel,query)
  if (result != 1)
  {
    odbcErrMsg <- odbcGetErrMsg(channel)
    odbcClearError(channel)
    if(missing(errorText)) {msg <- odbcErrMsg} else {msg <- paste(errorText,"\n",odbcErrMsg,sep="")}
    tkmessageBox(message=msg,icon="error",type="ok")
  }
  return(result)
}

create.cov.table <- function (table,channel,errorText=paste("create.cov.table(",table,")")) {

#   query <- paste("CREATE TABLE ", table, " (Ticker1 int NOT NULL, ",
#                  "Ticker2 int NOT NULL, Covarianza float NOT NULL ",
#                  "CONSTRAINT key_", table, " PRIMARY KEY (Ticker1,Ticker2))", sep = ""
#                 )
   query <- paste("CREATE TABLE ", table, " (Ticker1 int NOT NULL, ",
                  "Ticker2 int NOT NULL, Covarianza float ",
                  "CONSTRAINT key_", table, " PRIMARY KEY (Ticker1,Ticker2))", sep = ""
                 )
   result <- sqlCommand(channel,query,errorText)
   return (invisible(result))
}


# Questa procedura sostituisce i caratteri > e < con i
# rispettivi codici xml
xml_sub = function (stringa) {

  stringa = gsub(">","&gt;",stringa)
  stringa = gsub("<","&lt;",stringa)
  return(invisible(stringa))
}


# Questa subroutine restituisce un vettore di date "yyyy-mm-dd" a
# partire da un range di Id secondo la tabella "Date_numeriche"
# del DB Prezzi storici azioni (VAR)
get_calendar_dates <- function (Id_start=1, Id_end=Id_start, channel=channel) {

   query <- paste("SELECT Data FROM Date_numeriche WHERE Id >=", Id_start, 
                  " AND Id <= ", Id_end, " ORDER BY Data", sep = ""
                 )

   result <- sqlQuery(channel, query)
   return (invisible(format(result[,"Data"])))
}

# Questa subroutine restituisce il range di date "yyyy-mm-dd" a
# sotto forma di un vettore a due componenti a partire da un range 
# di Id secondo la tabella "Date_numeriche" del DB Prezzi storici azioni (VAR)
get_range_calendar_dates <- function (Id_start=1, Id_end=Id_start, channel=channel) {

   query = paste("SELECT Data FROM Date_numeriche WHERE Id >=", Id_start, 
                  " AND Id <= ", Id_end, " ORDER BY Data", sep = ""
                 )

   result = sqlQuery(channel, query)
   fine = length(result[,"Data"])
   if (fine > 0) {
     data = format(result[c(1,fine),"Data"])
     return (invisible(data))
   } else {
     return
   }
}


# Questa subroutine restituisce un vettore di date numeriche a
# partire da un range di date di calendario secondo la tabella "Date_numeriche"
# del DB Prezzi storici azioni (VAR)
get_numeric_dates <- function (date_start="'2000-06-19'", date_end=date_start, channel=channel, dateadd=0) {

   query = paste("SELECT Id AS Id FROM Date_numeriche WHERE Data >= DATEADD(dd, ", dateadd, ", ", date_start, ") ",
                 "AND Data <= ", date_end, sep="")
   result = sqlQuery(channel, query)
   return (invisible(result[,"Id"]))
}


# Questa procedura inserisce una matrice di varianza covarianza nella
# tabella specificata nel database definito in channel.
put.cov <- function (Sigma, table="Covariances", channel=channel, delete=FALSE, drop = FALSE) {

   if (drop) {
     risultato.query = drop.table (table, channel)
     risultato.query = create.cov.table (table, channel)
   }

   if (delete & !drop) {
     risultato.query <- delete.table(table, channel)
   }

   # Crea il vettore dei tickers
   v.tickers <- as.numeric(dimnames(Sigma)[1][[1]])

   # Calcola il loro numero
   n1 <- length (v.tickers)

   # Esegui il VEC della matrice di varianza covarianza (metti le colonne di S una sotto l'altra)
   vec.Sigma <- c(Sigma)
   v.ticker1 <- rep(v.tickers,n1)
   v.ticker2 <- rep(v.tickers, rep(n1,n1))

   # Crea il dataframe
   df.data <- data.frame(Ticker1 = v.ticker1, Ticker2 = v.ticker2, Covarianza = vec.Sigma, check.names = FALSE)

   # Query di accodamento: per ogni ticker in portafoglio effettua l'inserimento in "table"
   sqlSave(channel, df.data, tablename = table, rownames=FALSE, append = TRUE, safer = TRUE)

}




# Questa subroutine costruisce una matrice di varianza covarianza partendo da un vettore
# di tickers e da un dataframe contenente la coppia di tickers e la rispettiva covarianza
constr.cov <- function (tickers, channel) {

  # Determina il numero di tickers
  nb.tickers <- length(tickers)

  data <- sqlGetResults(channel, as.is = c(TRUE,TRUE,FALSE))
  m.tickers <- as.matrix(data[,1:2])
  v.data <- as.vector(data[,3])
  rm (data)

  # Crea la matrice di varianza covarianza S ed assegnale i nomi
  S <- matrix(data = NA, nrow = nb.tickers , ncol = nb.tickers)
  dimnames (S) <- list(tickers, tickers)

  for (i in 1:length(v.data)) {
    S[m.tickers[i,1], m.tickers[i,2]] <- v.data[i]
  }

  return (invisible(S))

}




# Questa procedura ritorna la matrice di varianza covarianza specificata
# da "tabella" che si trova nel database definito in channel.
get.cov <- function (table="Covariances", channel=channel) {

  #estrai la lista delle varianze-covarianze
  risultato.query <- sqlQuery(channel.psa.VAR.local, paste("SELECT DISTINCT Ticker1 FROM ", table, " WHERE N_returns <> 0", sep=""), as.is=TRUE)
  tickers <- as.vector(risultato.query[,1])
  nb.tickers <- length(tickers)

  odbcQuery(channel, paste("SELECT Ticker1, Ticker2, Covarianza FROM ", table, " WHERE N_returns <> 0", sep=""))

  S <- constr.cov(tickers, channel)

  return (invisible(S))

}




# Procedure SQL modificate in modo da riuscire a fare insert di Variabili con Nomi
# contententi underscore
sqlwrite <- function (channel, tablename, mydata, test = FALSE, fast = TRUE, 
    nastring = NULL, verbose = FALSE) 
{
    if (!odbcValidChannel(channel)) 
        stop("first argument is not an open RODBC channel")
    coldata <- sqlColumns(channel, tablename)[6]
    colnames <- as.character(sqlColumns(channel, tablename)[4][, 
        1])
#----------------------------------------------------------#
#                        Mia modifica                      #
#----------------------------------------------------------#
    colnames <- gsub("[^A-Za-z0-9_]+", "", colnames)
#----------------------------------------------------------#
    cnames <- paste(colnames, collapse = ",")
    if (!fast) {
        data <- as.matrix(mydata)
        cc <- grep("char", tolower(as.character(coldata[, 1])))
        if (length(cc)) 
            data[, cc] <- paste("'", data[, cc], "'", sep = "")
        data[is.na(mydata)] <- if (is.null(nastring)) 
            "NULL"
        else nastring[1]
        for (i in 1:nrow(data)) {
            query <- paste("INSERT INTO", tablename, "(", cnames, 
                ") VALUES (", paste(data[i, ], collapse = ","), 
                ")")
            if (verbose) 
                cat("Query: ", query, "\n", sep = "")
            if (odbcQuery(channel, query) < 0) 
                return(-1)
        }
    }
    else {
        query <- paste("INSERT INTO", tablename, "(", cnames, 
            ") VALUES (", paste(rep("?", ncol(mydata)), collapse = ","), 
            ")")
        if (verbose) 
            cat("Query: ", query, "\n", sep = "")
        coldata <- sqlColumns(channel, tablename)[c(4, 5, 7, 
            8, 9)]
        if (any(is.na(m <- match(colnames, coldata[, 1])))) 
            return(-1)
        paramdata <- t(as.matrix(coldata))[, m]
        if (odbcUpdate(channel, query, mydata, paramdata, test = test, 
            verbose = verbose, nastring = nastring) < 0) 
            return(-1)
    }
    return(invisible(1))
}




odbcUpdate <- function (channel, query, data, names, test = FALSE, verbose = FALSE, 
    nastring = NULL) 
{
    if (!odbcValidChannel(channel)) 
        stop("first argument is not an open RODBC channel")
    vflag <- 0
    if (verbose) 
        vflag <- 1
    if (test) 
        vflag <- 2
#----------------------------------------------------------#
#                        Mia modifica                      #
#----------------------------------------------------------#
    cnames <- gsub("[^A-Za-z0-9_]+", "", as.character(colnames(data)))
#----------------------------------------------------------#

    cnames <- switch(attr(channel, "case"), nochange = cnames, 
        toupper = toupper(cnames), tolower = tolower(cnames))
    for (i in 1:ncol(data)) if (!is.numeric(data[[i]])) 
        data[[i]] <- as.character(data[[i]])
    .Call("RODBCUpdate", as.integer(channel), as.character(query), 
        data, cnames, as.integer(nrow(data)), as.integer(ncol(data)), 
        as.character(names), as.integer(vflag), PACKAGE = "RODBC")
}

