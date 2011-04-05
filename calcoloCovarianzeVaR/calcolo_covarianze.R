rm(list=ls(all=TRUE))
library(RODBC)
library(tcltk)
source ("sub_db_utilities.R")


# Viene impostata la connessione al database "Prezzi storici azioni (VAR)"

db.prezziStoriciVAR = odbcConnect("prezzi_storici_azioni_VAR", "sa", "ghD54+J*x")
db.tempdb = odbcConnect("tempdb", "sa", "ghD54+J*x")

#db.prezziStoriciVAR = odbcConnect("prezzi_storici_VAR")
#db.tempdb = odbcConnect("tempdb")

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

# Inizializzazione di alcune variabili
Lista.tickers.returns = vector("character")
tabelle = c("TotalePrezziStorico","TotaleCambiStorico","TotaleIndiciStorico","TotaleTassiStorico")
#tabelle = c("TotaleCambiStorico","TotaleIndiciStorico","TotaleFondiSenzaProxyStorico")
tabelle = c("TotalePrezziStorico")
dimensione.blocco = 100

# Modifica la data calendario in data numerica: "yyyy-mm-dd" � il formato da utilizzare
data.cal.inizio = "'2009-07-27'"
data.cal.fine   = "'2010-07-27'"

# Determina la data numerica iniziale e finale
x=get_numeric_dates(data.cal.inizio, data.cal.fine, channel = db.prezziStoriciVAR, dateadd=-1)

data.num.inizio = min(x)
data.num.fine   = max(x)
rm(x)

writeLog (paste("<date_range type='cal'><dateadd>",-1,"</dateadd>",data.cal.inizio,"-",data.cal.fine,"</date_range>\n",sep=""))
writeLog (paste("<date_range type='num'>",data.num.inizio,"-",data.num.fine,"</date_range>\n",sep=""))
x=get_range_calendar_dates (data.num.inizio, data.num.fine, channel=db.prezziStoriciVAR)
writeLog(paste("<date_range type='cal'>'",x[1],"'-'",x[2],"'</date_range>\n",sep=""))
rm(x)

# Funzione: calcola i returns
calcola.returns = function() {

  writeLog ("<todo>Costruzione tabella tickers\n")
  # Rimuovi la tabella Ticker_numerici
  drop.table("Ticker_numerici", db.prezziStoriciVAR)
  writeLog ("<sql>drop.table('Ticker_numerici', db.prezziStoriciVAR)</sql>\n")

  # Crea la tabella con l'identificativo numerico del ticker (relazione biiettiva)
  query = paste("CREATE TABLE Ticker_numerici (Id smallint NOT NULL IDENTITY PRIMARY KEY, Ticker varchar(20)) ")
  writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))

  risultato.query = sqlQuery(db.prezziStoriciVAR, query)
  writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
  rm(query)

  # Esegui un loop sulle tabelle ed inserisci i tickers delle serie storiche da processare
  # nella tabella Numeric_ticker
  for (tabella in tabelle) {
    writeLog (paste("<tabella nome='",tabella,"'>\n",sep=""))
    if (tabella == "TotalePrezziStorico") {
      condizione = "Ticker IN (SELECT Ticker FROM EquityDB WHERE NotUniversoVAR=0) AND "
    }

    if (tabella == "TotaleCambiStorico") {
      condizione = ""
    }   

    if (tabella == "TotaleIndiciStorico") {
      condizione = ""
    }

    if (tabella == "TotaleTassiStorico") {
      condizione = ""
    }

    if (tabella == "TotaleFondiSenzaProxyStorico") {
      condizione = ""
    }

    # Seleziona lista dei ticker relativi ai titoli che entrano in 
	# considerazione per il calcolo del VAR
    query = paste("INSERT INTO Ticker_numerici (Ticker) ")
    query = paste(query,"SELECT DISTINCT Ticker FROM ", tabella, " WHERE ", condizione, "(TRADE_DATE >= ", sep="")
    query = paste(query,"DATEADD(dd, - 1, ", data.cal.inizio, ")) AND (TRADE_DATE <= ", data.cal.fine, ")", sep="")
    writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))

    risultato.query = sqlQuery(db.prezziStoriciVAR, query)
    writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
    rm(query)
    writeLog ("</tabella>\n")
  }

  # rimuovi la tabella Ticker_numerici nel tmpdb
  if (tableExist(myConnection=db.tempdb,tableName="Ticker_numerici",databaseName="tmpdb")) {
     # rimuovi la tabella Ticker_numerici anche dal tmpdb
     drop.table("Ticker_numerici", db.tempdb)
     writeLog ("<sql>drop.table('Ticker_numerici', db.tempdb)</sql>\n")
  }

  query <- "SELECT * INTO Ticker_numerici FROM [Prezzi storici azioni (VAR)].dbo.Ticker_numerici"
  writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))
  risultato.query = sqlQuery(db.tempdb, query)
  writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
  writeLog ("</todo>\n")


  # Rimuovi la tabella Returns_da_R
  if (tableExist(myConnection=db.tempdb,tableName="Returns_da_R",databaseName="tmpdb")) {
	# rimuovi la tabella Returns_da_R anche dal tmpdb
    drop.table ("Returns_da_R", db.tempdb)
    writeLog ("<sql>drop.table ('Returns_da_R', db.tempdb)</sql>\n")
  }
  
  # Crea la tabella Returns_da_R
  query = paste("CREATE TABLE Returns_da_R (Ticker int,",
                 "Data int, LogReturns float NOT NULL",
                 "CONSTRAINT chiave_Returns_da_R PRIMARY KEY (Ticker, Data))"
                )


  writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))

  risultato.query = sqlQuery(db.tempdb, query)
  writeLog (paste("<sql>",risultato.query,"</sql>\n",sep=""))
  rm(query)

  # query = paste("CREATE TABLE `Returns_da_R` (`Ticker` varchar(20) NOT NULL default '',",
  #                "`Data` datetime NOT NULL default '0000-00-00 00:00:00',",
  #                "`LogReturns` float default '0',",
  #                "PRIMARY KEY  (`Ticker`,`Data`)",
  #                ") TYPE=InnoDB;"
  #               )


  writeLog ("<todo>Calcolo returns\n")
  total.LogReturns = vector(mode = "numeric")
  total.tickers    = vector(mode = "numeric")
  total.num.dates      = vector(mode = "numeric")

  # Inizio loop calcolo returns
  for (tabella in tabelle) {
    writeLog (paste("<tabella nome='",tabella,"'>\n",sep=""))
    if (tabella == "TotalePrezziStorico") {
      condizione = "Ticker IN (SELECT Ticker FROM EquityDB WHERE NotUniversoVAR=0) AND "
    }

    if (tabella == "TotaleCambiStorico") {
      condizione = ""
    }   

    if (tabella == "TotaleIndiciStorico") {
      condizione = ""
    }

    if (tabella == "TotaleTassiStorico") {
      condizione = ""
    }


    # Estrai i logPrices
    query = "SELECT A.Id AS Ticker, B.Id AS TRADE_DATE, C.Interpolazione_log_close AS LogPrice FROM " 
    query = paste(query, tabella, "C INNER JOIN Ticker_numerici A ON C. Ticker = A.Ticker INNER JOIN Date_numeriche B ")
    query = paste(query, "ON C.TRADE_DATE = B.Data WHERE (B.Id >= ", data.num.inizio, sep="")
    query = paste(query,") AND (B.Id <= ", data.num.fine, ")", sep="")
    query = paste(query," ORDER BY A.Id, B.Id", sep="")
    writeLog (paste("<query>",xml_sub(query),"</query>\n",sep=""))

    risultato.query = sqlQuery(db.prezziStoriciVAR, query)
    rm(query)

    # Poni la data numerica iniziale nel data.frame "risultato.query" = 1
    risultato.query[,2] = risultato.query[,2] - data.num.inizio + 1

    # Seleziona i tickers estratti nel loop in esecuzione
    tickers.total = unique(risultato.query[,1])
    numero.tickers.total = length(tickers.total)
    first.num.index  = min(risultato.query[,2]) 
    last.num.index   = max(risultato.query[,2])

    writeLog (paste("<date_range type='index'>",first.num.index,"-",last.num.index,"</date_range>\n",sep=""))

    if (last.num.index > 1) { # numero di date > 1
      ii = 1
      while (numero.tickers.total>0) {
        writeLog (paste("<block number='",ii,"'>",sep=""))
        ii = ii + 1
        # Determina l'ultimo ticker per un blocco di lunghezza massima di dimensione.blocco
        numero.tickers.tmp = min(numero.tickers.total, dimensione.blocco)     
        ultimo.ticker = tickers.total[numero.tickers.tmp]
        writeLog (paste("<nb_tickers>",numero.tickers.tmp,"</nb_tickers>\n",sep=""))

        # Crea il vettore di vero/falso per estrarre tutte le osservazioni fino all'ultimo.ticker
        x = risultato.query[,1]==ultimo.ticker
        y = 1:length(x)

        posizione.ultimo.ticker = max(y[x])
        x = y <= posizione.ultimo.ticker

        risultato.tmp = risultato.query[x,]
        tickers.tmp = unique(risultato.tmp[,1])

        # Ridefinisci risultato.query, tickers.total, numero.tickers.total
        risultato.query = risultato.query[!x,]
        tickers.total = unique(risultato.query[,1])
        numero.tickers.total = length(tickers.total)


        LogPrices = matrix(,nrow=last.num.index,ncol=numero.tickers.tmp)

        j = 1
        for (i in tickers.tmp) {
          x = risultato.tmp[,1]==i
          LogPrices[risultato.tmp[x,2],j] = risultato.tmp[x,3]
          j = j + 1
        }
        rm(risultato.tmp)   

        # Calcola i returns
        LogReturns.tmp = diff(LogPrices)
        rm(LogPrices)

        # Salva i risultati
        date.index = 2:last.num.index                # Crea il range di date.index
        date.num = date.index + data.num.inizio - 1  # Passa da data.index a data.num
        writeLog (paste("<date_range type='num'>",min(date.num),"-",max(date.num),"</date_range>\n",sep=""))
        x = get_range_calendar_dates (min(date.num), max(date.num), channel=db.prezziStoriciVAR)
        writeLog (paste("<date_range type='cal'>'",x[1],"'-'",x[2],"'</date_range>\n",sep=""))
        rm(x)
        rm(date.index) 

        # Passa da formato matrice a formato vettore
        LogReturns.tmp = c(LogReturns.tmp)

        # Crea il vettore globale dei tickers e delle date numeriche
        ticker.global = rep(tickers.tmp,rep(last.num.index-1,numero.tickers.tmp))
        data.num.global = rep(date.num,numero.tickers.tmp)

        # Determina le osservazioni non nulle
        x = !(is.null(LogReturns.tmp) | is.na(LogReturns.tmp))
        Dati = data.frame(Ticker = ticker.global[x], Data = data.num.global[x], LogReturns = LogReturns.tmp[x])

        # Query di accodamento: per ogni ticker in portafoglio effettua l'inserimento nella tabella returns

        sqlSave(db.tempdb, Dati, tablename = "Returns_da_R", append = TRUE, rownames = FALSE,
              colnames = FALSE, verbose = FALSE,
              safer = TRUE, addPK = FALSE, fast = TRUE, test = FALSE, nastring = NULL)
        total.LogReturns = c(total.LogReturns,LogReturns.tmp)
        total.tickers = c(total.tickers, ticker.global)
        total.num.dates = c(total.num.dates, data.num.global)
        writeLog ("</block>")
      } # end while
    } # end if
    writeLog ("</tabella>\n")
  } # end for
  writeLog ("</todo>\n")


  # Costruisci la matrice dei dati e calcola le covarianze
  data.num.inizio = min(total.num.dates)
  total.index.dates = total.num.dates - data.num.inizio + 1
  rm(total.num.dates)
  tickers = unique(total.tickers)
  
  returns = matrix(, nrow=max(total.index.dates), ncol=length(tickers), byrow=FALSE)
  
  j = 1
  for (i in tickers) {
     x = total.tickers==i
     returns[total.index.dates[x],j] = total.LogReturns[x]
     j = j + 1
  }   

  dimnames(returns) = list(NULL,tickers)
  S = cov(returns,use="pairwise.complete.obs")



  # calcola il numero di osservazioni per ciascuna coppia di serie
  nb.common.obs <- matrix(0, nrow=length(tickers), ncol=length(tickers), byrow=FALSE)
  for (i in 1:ncol(S)) {
	for (j in 1:i) {
	   nb.common.obs[i,j] <- sum( !(is.na(returns[,i]) | is.na(returns[,j]) ))
         if (i!=j) { 
            nb.common.obs[j,i] <- nb.common.obs[i,j] 
         }
      }
  }
  

  dimnames(nb.common.obs) <- list(tickers,tickers) 

  # Verifica che non ci siano NULL
  if (sum(is.null(S)) > 0) {
    print ("attenzione ci sono null in S!")
  }


  #query = paste("CREATE TABLE `Covarianza_universo_da_R` (`Ticker1` varchar(20) NOT NULL default '',",
  #                "`Ticker2` varchar(20) NOT NULL default '',",
  #                "`Data` datetime NOT NULL default '0000-00-00 00:00:00',",
  #                "`LogReturns` double NOT NULL default '-10000000',",
  #                "PRIMARY KEY  (`Ticker1`,`Ticker2`)",
  #                ") TYPE=InnoDB;"
  #               )

  put.cov (Sigma=S, table="Covarianza_universo_da_R", channel=db.tempdb, drop=TRUE)


  if (tableExist(myConnection=db.tempdb,tableName="Covarianza_numero_osservazioni",databaseName="tmpdb")) {
     # rimuovi la tabella Covarianza_numero_osservazioni dal tmpdb
     drop.table("Covarianza_numero_osservazioni", db.tempdb)
     writeLog ("<sql>drop.table('Covarianza_numero_osservazioni', db.tempdb)</sql>\n")
  }
  put.cov (Sigma=nb.common.obs, table="Covarianza_numero_osservazioni", channel=db.tempdb, drop=FALSE)
  rm(nb.common.obs)
  
  
  # esporta la tabella delle covarianze con i giusti ticker ed il numero di osservazioni nel database "Prezzi storici azioni (VAR)"
  tableLongName = "[Prezzi storici azioni (VAR)].dbo.Covarianza_universo_da_R"
  tableName = "dbo.Covarianza_universo_da_R"
  query <- paste("INSERT INTO",tableLongName,"(Ticker1, Ticker2, Covarianza, N_returns)",
           "SELECT dbo.Ticker_numerici.Ticker AS Ticker1, Ticker_numerici_1.Ticker AS Ticker2, dbo.Covarianza_universo_da_R.Covarianza AS Covarianza,",
           "dbo.Covarianza_numero_osservazioni.Covarianza AS N_returns FROM dbo.Covarianza_universo_da_R INNER JOIN", 
           "dbo.Ticker_numerici ON dbo.Covarianza_universo_da_R.Ticker1 = dbo.Ticker_numerici.Id INNER JOIN", 
           "dbo.Ticker_numerici Ticker_numerici_1 ON dbo.Covarianza_universo_da_R.Ticker2 = Ticker_numerici_1.Id INNER JOIN", 
           "dbo.Covarianza_numero_osservazioni ON dbo.Covarianza_universo_da_R.Ticker1 = dbo.Covarianza_numero_osservazioni.Ticker1 AND ", 
           "dbo.Covarianza_universo_da_R.Ticker2 = dbo.Covarianza_numero_osservazioni.Ticker2")
  
  delete.table(table=tableName,channel=db.prezziStoriciVAR)
  sqlCommand(channel=db.tempdb,query=query)
  
  return(list(returns=returns,S=S))

}



# Calcola i returns
X = calcola.returns()



data.fine.calcolo.returns = date()

# Registra data di fine
writeLog (paste("<time>",date(),"</time>\n",sep=""))
writeLog ("</start>\n")





## Inizio procedura di correzione della matrice di varianza covarianza

#S = get.cov ("Covarianza_universo_da_R", db.tempdb)
#S = S*10000

#source ("Faudrino.R")
#S.new = approx.semidef (data=S, start.val=diag(diag(S)), rho=0.5)

## create.cov.table("Covarianza_universo_pos_def", db.tempdb)

## put.cov (S, table="Covarianza_universo_pos_def", channel=db.tempdb)
