rm(list=ls(all=TRUE))
library("tcltk")
library("ayrton")
library("tkutilities")
library("odbcutilities")
source("C:/R/R_tablelist/MarkowitzSqlProcedures.R")

##require(debug)


## temporary function (to delete later)
pippo <- function() print("pippo")

update.df.clients <- function(dataFrame)
{
  df.clients <<- dataFrame
}

## create the necessary functions
update.data.sources <- function(topWindow)
{
  ## open a connection to DBMarkowitz
  odbcCon = setOdbcConnection("DBMarkowitz")
    
  ## ask for covariance update
  update.cov <- tclvalue(tkmessageBox(message="Update the covariance matrices?",
                                      icon="question", type="yesno",
                                      parent=topWindow, default="no"))
  
  ## create the feedback windows
  top <- tktoplevel()
  feedbackWin <- create_feedback(parent=top,steps=100,
                                 "Status: importing data...                   ")
  tkgrid(feedbackWin[[1]],padx=padx,pady=pady)
  tktitle(top) <- "Feedback window"
  tkgrab(top)
  
  feedbackWin[["step"]](10)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_Covarianza_universo",channel=odbcCon,
             errorText="DROP TABLE Copia_Covarianza_universo")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q1
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT Ticker1, Ticker2, Covarianza, N_returns ",
                 "INTO Copia_Covarianza_universo ",
                 "FROM [Prezzi storici azioni (VAR)].dbo.Covarianza_universo"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q1")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](15)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBTassi",channel=odbcCon,errorText="DROP TABLE Copia_DBTassi")
  ## -------------------------------------------------------------------------
  ## update.data.sources q2
  ## -------------------------------------------------------------------------
  query <- "SELECT Moneta, Scadenza INTO Copia_DBTassi FROM [Tassi storici (VAR)].dbo.DBTassi"
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q2")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](20)
  ## -------------------------------------------------------------------------
  drop.table(table="Covarianza_universo_senza_tassi",channel=odbcCon,
             errorText="DROP TABLE Covarianza_universo_senza_tassi")
  ## -------------------------------------------------------------------------
  
                                        # update.data.sources q3
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT Copia_Covarianza_universo.* ",
                 "INTO Covarianza_universo_senza_tassi ",
                 "FROM Copia_Covarianza_universo ",
                 "WHERE (Ticker1 NOT IN (SELECT Moneta + Scadenza FROM dbo.Copia_DBTassi)) ",
                 "AND (Ticker2 NOT IN (SELECT Moneta + scadenza FROM dbo.Copia_DBTassi))"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q3")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](30)
  ## -------------------------------------------------------------------------
  drop.table(table="Expected_returns_azionari",channel=odbcCon,
             errorText="DROP TABLE Expected_returns_azionari")
  ## -------------------------------------------------------------------------
  
                                        # update.data.sources q4
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * ",
                 "INTO Expected_returns_azionari ",
                 "FROM [Prezzi storici azioni (VAR)].dbo.R_Expected_returns_dopo_aggiustamento_per_consensus_2"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q4")
  ## -------------------------------------------------------------------------
  
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBMonete",channel=odbcCon,
             errorText="DROP TABLE Copia_DBMonete")
  ## -------------------------------------------------------------------------
  
                                        # update.data.sources q5
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBMonete ",
                 "FROM [Prezzi storici azioni].dbo.DBMonete "
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q5")
  ## -------------------------------------------------------------------------
    
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBMoneta",channel=odbcCon,
             errorText="DROP TABLE Copia_DBMoneta")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q6
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBMoneta ",
                 "FROM [Sistema (Prova)].dbo.DBMoneta "
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q6")
  ## -------------------------------------------------------------------------
    
  feedbackWin[["status"]](45)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_vista_per_portfolio",channel=odbcCon,
             errorText="DROP TABLE Copia_vista_per_portfolio")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q7
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_vista_per_portfolio ",
                 "FROM [Sistema (prova)].dbo.vista_per_portfolio"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q7")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](55)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBOpzioniSuAzioni",channel=odbcCon,
             errorText="DROP TABLE Copia_DBOpzioniSuAzioni")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q8
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBOpzioniSuAzioni ",
                 "FROM [Sistema (prova)].dbo.DBOpzioniSuAzioni"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q8")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](65)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBStrutturaDerivati",channel=odbcCon,
             errorText="DROP TABLE Copia_DBStrutturaDerivati")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q9
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBStrutturaDerivati ",
                 "FROM [Sistema (prova)].dbo.DBStrutturaDerivati"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q9")
  ## -------------------------------------------------------------------------
  
  
  feedbackWin[["status"]](75)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBAzioni",channel=odbcCon,
             errorText="DROP TABLE Copia_DBAzioni")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q10
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBAzioni ",
                 "FROM [Sistema (prova)].dbo.DBAzioni"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q10")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](82)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBExpected_returns_fx",channel=odbcCon,
             errorText="DROP TABLE Copia_DBExpected_returns_fx")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q11
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBExpected_returns_fx ",
                 "FROM  [Prezzi storici azioni (VAR)].dbo.DBExpected_returns_fx"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q11")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](90)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_EquityDB",channel=odbcCon,
             errorText="DROP TABLE Copia_EquityDB")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q12
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_EquityDB ",
                 "FROM [Prezzi storici azioni (VAR)].dbo.EquityDB"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q12")
  ## -------------------------------------------------------------------------
    
  feedbackWin[["status"]](95)
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_DBExpected_risk_free",channel=odbcCon,
             errorText="DROP TABLE Copia_DBExpected_risk_free")
  ## -------------------------------------------------------------------------

  ## update.data.sources q13
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_DBExpected_risk_free ",
                 "FROM [Prezzi storici azioni (VAR)].dbo.DBExpected_risk_free"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q13")
  ## -------------------------------------------------------------------------
  
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_Exp_returns_azioni_mon_investimento",channel=odbcCon,
             errorText="DROP TABLE Copia_Exp_returns_azioni_mon_investimento")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q14
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_Exp_returns_azioni_mon_investimento ",
                 "FROM  [Prezzi storici azioni (VAR)].dbo.R_Expected_returns_dopo_aggiustamento_per_consensus_2 "
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q14")
  ## -------------------------------------------------------------------------
  
  ## -------------------------------------------------------------------------
  drop.table(table="Copia_Expected_returns_per_ottimizzazione_markowitz",channel=odbcCon,
             errorText="DROP TABLE Copia_Expected_returns_per_ottimizzazione_markowitz")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q15
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT * INTO Copia_Expected_returns_per_ottimizzazione_markowitz ",
                 "FROM  [Prezzi storici azioni (VAR)].dbo.Expected_returns_per_ottimizzazione_markowitz"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q15")
  ## -------------------------------------------------------------------------
  
  feedbackWin[["status"]](97)
  ## Aggiorna la tabella Universo_markowitz
  ## -------------------------------------------------------------------------
  delete.table(table="Universo_Markowitz",channel=odbcCon,
               errorText="DELETE FROM Universo_Markowitz")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q16
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO Universo_Markowitz (ID_strumento, ID_AAA, Ticker, Company, Branche, [Specification Branche], Moneta, [Ranking settore], [Ranking moneta]) ",
                 "SELECT ID_strumento, ID AS ID_AAA, Ticker, Company, BrancheMarkowitz AS Branche, [Specification Brance], Moneta, [Ranking settore], [Ranking moneta] ",
                 "FROM dbo.Copia_EquityDB ",
                 "WHERE (ListaReturnSettoriale = 1) And (NotUniversoVAR = 0)"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q16")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q17
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO Universo_Markowitz (ID_strumento, ID_AAA,Ticker, Company, Branche, [Specification Branche], Moneta, [Ranking settore], [Ranking moneta]) ",
                 "SELECT 0 AS ID_strumento, ID AS ID_AAA, Ticker, Company, Branche, [Specification Brance], Moneta, [Ranking settore], [Ranking moneta] ",
                 "FROM DBRisk_free"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q17")
  ## -------------------------------------------------------------------------
  
  ##Aggiorna la tabella Elenco clienti
  delete.table(table="Elenco_clienti",channel=odbcCon,
               errorText="DELETE FROM Elenco_clienti")
  ## -------------------------------------------------------------------------
  
  ## update.data.sources q18
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO Elenco_clienti (Cliente, MonetaInvestimento) ",
                 "SELECT [Sistema (prova)].dbo.DBClienti.Cliente, [Sistema (prova)].dbo.DBPoliticaInvestimento.MonetaInvestimento ",
                 "FROM [Sistema (prova)].dbo.DBClienti INNER JOIN ",
                 "[Sistema (prova)].dbo.DBPoliticaInvestimento ON ",
                 "[Sistema (prova)].dbo.DBClienti.Cliente = [Sistema (prova)].dbo.DBPoliticaInvestimento.Cliente"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="update.data.sources q18")
  ## -------------------------------------------------------------------------
  
  if (identical(update.cov,"yes")) preparazione.covarianze.azionarie(refCurrency="CHF",feedback=feedbackWin,odbcCon=odbcCon)
  
  feedbackWin[["status"]](100)
  tkgrab.release(top)
  tkdestroy(top)
  
  odbcClose(odbcCon)
  tkmessageBox(message="Update procedure successful.",icon="info",type="ok",parent=topWindow)
  
}


preparazione.covarianze.azionarie <- function(refCurrency="CHF", feedback, odbcCon)
{
  feedback[["set.label"]]("Status: adjusting covariance matrices step 1 ...                ",
                          sameWidth=F)
  feedback[["status"]](10)
  refCurrencyClient = refCurrency
  
  ## fai una copia della tabella DBUniverso_per_ottimizzazione inserendo pure tutti i Tickers delle monete desiderate
  ## -------------------------------------------------------------------------
  delete.table(table="DBUniverso_per_ottimizzazione_tmp",channel=odbcCon,
               errorText="DELETE FROM DBUniverso_per_ottimizzazione_tmp")
  ## -------------------------------------------------------------------------
  
  ## preparazione.covarianze.azionarie q1
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO DBUniverso_per_ottimizzazione_tmp ",
                 "(ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company) ",
                 "SELECT ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company ",
                 "FROM DBUniverso_per_ottimizzazione"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q1")
  ## -------------------------------------------------------------------------
  
  ## preparazione.covarianze.azionarie q2
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO DBUniverso_per_ottimizzazione_tmp ",
                 "(ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company) ",
                 "SELECT 2 AS ID_strumento, ID AS ID_AAA, Moneta as Moneta, 'Moneta' as Branche, Moneta as Ticker, NULL AS Company FROM Copia_DBMonete ",
                 "WHERE NotUniversoVAR = 0 AND Moneta <> 'CHF'"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q2")
  ## -------------------------------------------------------------------------
  
  ## Crea la matrice delle varianze covarianze i cui cambi sono rispetto alla moneta CHF
  ## -------------------------------------------------------------------------
  drop.table(table="G_covarianze_azionarie_CHF",channel=odbcCon,
             errorText="DROP TABLE G_covarianze_azionarie_CHF")
  ## -------------------------------------------------------------------------
  
  ## preparazione.covarianze.azionarie q3
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT A.Ticker1, A.Ticker2, A.Covarianza, B.Moneta AS Moneta1, C.Moneta AS Moneta2 ",
                 "INTO G_covarianze_azionarie_CHF ",
                 "FROM Copia_Covarianza_universo A INNER JOIN DBUniverso_per_ottimizzazione_tmp B ON A.Ticker1 = B.Ticker ",
                 "INNER JOIN DBUniverso_per_ottimizzazione_tmp C ON A.Ticker2 = C.Ticker"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q3")
  ## -------------------------------------------------------------------------
  
  ## inizia un loop e crea tutte le matrici di varianza covarianza i cui cambi sono rispetto alla moneta di turno
  ## preparazione.covarianze.azionarie q4
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT DISTINCT Moneta ",
                 "FROM Copia_DBMonete ",
                 "WHERE (NotUniversoVAR = 0) AND (MonetaRiferimento = 1) AND (Moneta <> 'CHF')"
                 )
  noRefCurrencies <- sql.get.table(odbcCon, query=query)
  nb.noRefCurrencies <- length(noRefCurrencies[,1])
  ## -------------------------------------------------------------------------
  
  feedback[["status"]](30)
  if (nb.noRefCurrencies > 0)
    {
      count.noRefCurrencies <- 0
      delta.noRefCurrencies <- 30 / nb.noRefCurrencies
      for (refCurrency in noRefCurrencies[,1])
        {
          feedback[["status"]](floor(30+count.noRefCurrencies*delta.noRefCurrencies))
          
          ## crea la matrice delle varianze covarianze coi cambi rispetto alla nuova moneta di riferimento
          ## -------------------------------------------------------------------------
          drop.table(table="G_covarianze_azionarie_" %+% refCurrency,channel=odbcCon,
                     errorText="DROP TABLE G_covarianze_azionarie_" %+% refCurrency)
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q4_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                         "INTO G_covarianze_azionarie_" %+% refCurrency,
                         "FROM G_covarianze_azionarie_CHF"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q4_loop")
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q5_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT Covarianza FROM G_covarianze_azionarie_" %+% refCurrency,
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'",
                         "AND Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sql.get.table(odbcCon, query=query,as.is=FALSE)
          variance.refCurrency = result[1,1]
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q6_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Covarianza = -1 * Covarianza ",
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q6_loop")
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q7_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Covarianza = -1 * Covarianza ",
                         "WHERE Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="preparazione.covarianze.azionarie q7_loop")
          ## -------------------------------------------------------------------------
          
          ## seleziona tutte le monete diverse da quella di riferimento che ci sono in portafoglio
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT Moneta FROM Copia_DBMonete ",
                         "WHERE NotUniversoVAR = 0 AND Moneta <> '" %+% refCurrency %+% "'"
                         )
          portfNotChfCurr <- sql.get.table(odbcCon, query=query)
          nb.portfNotChfCurr <- length(portfNotChfCurr[,1])
          ## -------------------------------------------------------------------------
          
          if (nb.portfNotChfCurr > 0)
            {
              count.portNotChfCurr <- 0
              delta.portNotChfCurr <- delta.noRefCurrencies / nb.portfNotChfCurr
              for (currency in portfNotChfCurr[,1])
                {
                  feedback[["set.label"]](paste("Status: adjusting covariance matrices step 1",
                                                refCurrency, "-", currency), sameWidth=F)
                  feedback[["status"]](floor(30+count.noRefCurrencies*delta.noRefCurrencies+count.portNotChfCurr*delta.portNotChfCurr))
                  
                  ## preparazione.covarianze.azionarie q2.1_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT A.Ticker1, A.Ticker2, A.Moneta1, A.Moneta2, A.Covarianza AS Covarianza1,",
                                 " B.Covarianza AS Covarianza2 ",
                                 "INTO #DBTmp1 ", #
                                 "FROM G_covarianze_azionarie_" %+% refCurrency,
                                 "AS A INNER JOIN G_covarianze_azionarie_" %+% refCurrency,
                                 "AS B ON A.Ticker1 = B.Ticker1 ",
                                 "WHERE B.Ticker2 LIKE '" %+% refCurrency %+% "'"
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.1_loop")
                  ## -------------------------------------------------------------------------
                  
                  ## fai una copia della tabella G_covarianze_azionarie_" & refCurrency
                  ## da usare nella seconda query di join
                  ## preparazione.covarianze.azionarie q2.2_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT * ",
                                 "INTO #DBTmp2 ", #
                                 "FROM G_covarianze_azionarie_" %+% refCurrency
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.2_loop")
                  ## -------------------------------------------------------------------------
                  
                  ## -------------------------------------------------------------------------
                  drop.table(table="G_covarianze_azionarie_" %+% refCurrency,channel=odbcCon,
                             errorText="DELETE FROM G_covarianze_azionarie_" %+% refCurrency)
                  ## -------------------------------------------------------------------------
                  
                  ## preparazione.covarianze.azionarie q2.3_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT Ticker1, Ticker2, CASE WHEN Ticker2 LIKE '" %+% currency %+% "'",
                                 "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END",
                                 "AS Covarianza, Moneta1, Moneta2 ",
                                 "INTO G_covarianze_azionarie_" %+% refCurrency,
                                 "FROM #DBTmp1" #
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.3_loop")
                  ## -------------------------------------------------------------------------
                  
                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp1",channel=odbcCon,errorText="DROP TABLE #DBTmp1") #
                  ## -------------------------------------------------------------------------
                  
                  ## preparazione.covarianze.azionarie q2.4_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT A.Ticker1, A.Ticker2, A.Moneta1, A.Moneta2,",
                                 "A.Covarianza AS Covarianza1, B.Covarianza AS Covarianza2 ",
                                 "INTO #DBTmp ", #
                                 "FROM G_covarianze_azionarie_" %+% refCurrency,
                                 "AS A INNER JOIN #DBTmp2 AS B ON A.Ticker2 = B.Ticker2 ", #
                                 "WHERE B.Ticker1 LIKE '" %+% refCurrency %+% "'"
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.4_loop")
                  ## -------------------------------------------------------------------------
                  
                  ## -------------------------------------------------------------------------
                  drop.table(table="G_covarianze_azionarie_" %+% refCurrency,
                             channel=odbcCon,errorText="DROP TABLE G_covarianze_azionarie_" %+% refCurrency)
                  ## -------------------------------------------------------------------------
                  
                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp2",channel=odbcCon,errorText="DROP TABLE #DBTmp2") #
                  ## -------------------------------------------------------------------------
                  
                  ## preparazione.covarianze.azionarie q2.5_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT Ticker1, Ticker2, CASE WHEN Ticker1 LIKE '" %+% currency %+% "'",
                                 "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END AS Covarianza, Moneta1, Moneta2 ",
                                 "INTO G_covarianze_azionarie_" %+% refCurrency,
                                 "FROM #DBTmp" #
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.5_loop")
                  ## -------------------------------------------------------------------------
                  
                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp",channel=odbcCon,errorText="DROP TABLE #DBTmp") #
                  ## -------------------------------------------------------------------------
                  
                  ## preparazione.covarianze.azionarie q2.6_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                                 "SET Covarianza = Covarianza + " %+% variance.refCurrency,
                                 "WHERE Ticker1 LIKE '" %+% currency %+% "'",
                                 "AND Ticker2 LIKE '" %+% currency %+% "'"
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="preparazione.covarianze.azionarie q2.6_loop")
                  ## -------------------------------------------------------------------------
                  
                  count.portNotChfCurr <- count.portNotChfCurr + 1
                }
              rm(nb.portfNotChfCurr,portfNotChfCurr,currency,count.portNotChfCurr,
                 delta.portNotChfCurr)
            }
          
          ## Modifica il nome del fattore di rischio da quello della moneta di riferimento a CHF.
          ## preparazione.covarianze.azionarie q3.1
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Ticker1 = 'CHF', Moneta1='CHF' ",
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="preparazione.covarianze.azionarie q3.1")
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q3.2
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Ticker2 = 'CHF', Moneta2='CHF' ",
                         "WHERE Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="preparazione.covarianze.azionarie q3.2")
          ## -------------------------------------------------------------------------
          
          count.noRefCurrencies <- count.noRefCurrencies + 1
        }
      rm(nb.noRefCurrencies,noRefCurrencies,count.noRefCurrencies,delta.noRefCurrencies)
    }
  
  ## inizia un loop e crea tutte le matrici di varianza covarianza i cui cambi
  ## sono rispetto alla moneta di turno
  
  feedback[["status"]](60)
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT DISTINCT Moneta FROM Copia_DBMonete",
                 "WHERE (NotUniversoVAR = 0) AND (MonetaRiferimento = 1) ",
                 "OR (Moneta = 'CHF')"
                 )
  noRefCurrencies <- sql.get.table(odbcCon, query=query)
  nb.noRefCurrencies <- length(noRefCurrencies[,1])
  ## -------------------------------------------------------------------------
  ## define variable for the feedback widget
  count.noRefCurrencies <- 0
  delta.noRefCurrencies <- 30 / nb.noRefCurrencies
  
  for (refCurrency  in noRefCurrencies[,1])
    {
      ## Prepara le covarianze nella valuta di riferimento
      ## -------------------------------------------------------------------------
      query <- paste(
                     "SELECT DISTINCT Moneta1 AS Moneta FROM G_covarianze_azionarie_" %+% refCurrency,
                     "WHERE Moneta1 <> '" %+% refCurrency %+% "'"
                     )
      currencies <- sql.get.table(odbcCon, query=query)
      nb.currencies <- length(currencies[,1])
      ## -------------------------------------------------------------------------
      if (nb.currencies > 0)
        {
          count.currencies <- 0
          delta.currencies <- delta.noRefCurrencies / nb.currencies
          
          ## -------------------------------------------------------------------------
          drop.table(table="G_covarianze_azionarie_moneta_" %+% refCurrency,
                     channel=odbcCon,errorText="DROP TABLE G_covarianze_azionarie_moneta_" %+% refCurrency)
          ## -------------------------------------------------------------------------
          
          ## preparazione.covarianze.azionarie q4.1
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT * INTO G_covarianze_azionarie_moneta_" %+% refCurrency,
                         "FROM G_covarianze_azionarie_" %+% refCurrency
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="preparazione.covarianze.azionarie q4.1")
          ## -------------------------------------------------------------------------
    
          for (currency in currencies[,1])
            {
              feedback[["set.label"]](paste("Status: adjusting covariance matrices step 2",
                                            refCurrency, "-", currency), sameWidth=F)
              feedback[["status"]](floor(60+count.noRefCurrencies*delta.noRefCurrencies+count.currencies*delta.currencies))
              trasforma_matrice_covarianze(currency=currency,refCurrency=refCurrency,odbcCon=odbcCon)
              count.currencies <- count.currencies + 1
            }
          
        }
      
      count.noRefCurrencies <- count.noRefCurrencies + 1
    }
  
  rm(nb.currencies,currencies,currency)
  rm(count.noRefCurrencies,delta.noRefCurrencies)
  feedback[["status"]](90)
  AA_seleziona_matrice_covarianza(currency=refCurrencyClient,odbcCon=odbcCon)
}



AA_seleziona_matrice_covarianza <- function(currency="CHF",odbcCon=odbcCon)
{
  ## -----------------------------------------------------------------------------
  drop.table(table="G_covarianze_azionarie",channel=odbcCon,
             errorText="DROP TABLE G_covarianze_azionarie")
  ## -----------------------------------------------------------------------------
  
  ## AA_seleziona_matrice_covarianza q1
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                 "INTO G_covarianze_azionarie ",
                 "FROM G_covarianze_azionarie_" %+% currency
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="AA_seleziona_matrice_covarianza q1")
  ## -----------------------------------------------------------------------------
  
  ## -----------------------------------------------------------------------------
  drop.table(table="G_covarianze_azionarie_moneta_rif",channel=odbcCon,
             errorText="DROP TABLE G_covarianze_azionarie_moneta_rif")
  ## -----------------------------------------------------------------------------
  
  ## AA_seleziona_matrice_covarianza q2
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                 "INTO G_covarianze_azionarie_moneta_rif ",
                 "FROM G_covarianze_azionarie_moneta_" %+% currency
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="AA_seleziona_matrice_covarianza q2")
  ## -----------------------------------------------------------------------------
}



trasforma_matrice_covarianze <- function(currency="CHF",refCurrency="CHF",odbcCon=odbcCon)
{
  tableName = paste("G_covarianze_azionarie_moneta_",refCurrency,sep="")
  
  ## 1) costruisci matrice con i tickers nella moneta in questione
  ## trasforma_matrice_covarianze q0
  ## -----------------------------------------------------------------------------
  query <- paste("SELECT * INTO ##G_covarianze_azionarie_moneta_rif_tmp FROM ",tableName) #
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="trasforma_matrice_covarianze q0")
  ## -----------------------------------------------------------------------------
  
  ## correggi prima le covarianze di sinistra
  ## trasforma_matrice_covarianze q1
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO ##G_covarianze_azionarie_moneta_rif_tmp ", #
                 "SELECT A.Ticker1, A.Ticker2, B.Covarianza, A.Moneta1, A.Moneta2 ",
                 "FROM ", tableName, "A INNER JOIN ", tableName, " B ",
                 "ON A.Ticker2 = B.Ticker2 AND A.Moneta1 = B.Ticker1",
                 "WHERE (A.Moneta1 = '" %+% currency %+% "') " ,
                 "AND (A.Ticker1 <> '" %+% currency %+% "')"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="trasforma_matrice_covarianze q1")
  ## -----------------------------------------------------------------------------

  ## poi quelle di destra
  ## trasforma_matrice_covarianze q2
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO ##G_covarianze_azionarie_moneta_rif_tmp ", #
                 "SELECT A.Ticker1, A.Ticker2, B.Covarianza, A.Moneta1, A.Moneta2 ",
                 "FROM ", tableName , " A INNER JOIN " , tableName , " B ",
                 "ON A.Moneta2 = B.Ticker2 AND A.Ticker1 = B.Ticker1 ",
                 "WHERE (A.Moneta2 = '" %+% currency %+% "') ",
                 "AND (A.Ticker2 <> '" %+% currency %+% "')"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="trasforma_matrice_covarianze q2")
  ## -----------------------------------------------------------------------------
  
  ## poi aggiungi la varianza per quelle coppie <Ticker1,Ticker2> con la moneta = USD, pero' non la copia <USD,USD>
  ## trasforma_matrice_covarianze q3
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO ##G_covarianze_azionarie_moneta_rif_tmp ", #
                 "SELECT A.Ticker1, A.Ticker2, B.Covarianza, A.Moneta1, A.Moneta2 ",
                 "FROM ", tableName, " A INNER JOIN ", tableName, " B ",
                 "ON A.Moneta1 = B.Ticker1 AND A.Moneta2 = B.Ticker2 ",
                 "WHERE (A.Moneta1 = '" %+% currency %+% "') AND (A.Moneta2 = '" %+%
                 currency %+% "') AND (A.Ticker1 <> '" %+% currency %+%"') ",
                 "AND (A.Ticker2 <> '" %+% currency %+% "')"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="trasforma_matrice_covarianze q3")
  ## -----------------------------------------------------------------------------
  
  ## -----------------------------------------------------------------------------
  drop.table(table=tableName,channel=odbcCon,errorText="DROP TABLE " %+% tableName)
  ## -----------------------------------------------------------------------------
  ## trasforma_matrice_covarianze q4
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "SELECT Ticker1, Ticker2, SUM(Covarianza) AS Covarianza, Moneta1, Moneta2 ",
                 "INTO ", tableName ,
                 "FROM ##G_covarianze_azionarie_moneta_rif_tmp ", #
                 "GROUP BY Ticker1, Ticker2, Moneta1, Moneta2"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="trasforma_matrice_covarianze q4")
  ## -----------------------------------------------------------------------------
  
  ## -----------------------------------------------------------------------------
  drop.table(table="##G_covarianze_azionarie_moneta_rif_tmp",channel=odbcCon, #
             errorText="DROP TABLE ##G_covarianze_azionarie_moneta_rif_tmp") #
  ## -----------------------------------------------------------------------------
}

## SELECT RISK FREE UNIVERSE FUNCTION
select.oneYearInterest.universe <- function(parent)
{
  odbcCon = setOdbcConnection("DBMarkowitz")
  ## get the list of all available 1 year interest rates
  query <-  paste(
                  "SELECT DISTINCT Moneta AS Currency",
                  "FROM [Prezzi storici azioni (VAR)].dbo.DBMonete",
                  "WHERE Moneta IN (SELECT DISTINCT Ticker",
                  "FROM [Prezzi storici azioni (VAR)].dbo.TotaleCambiStorico)",
                  "OR Moneta LIKE 'CHF'"
                  )
  df.oneYearInterest <- sql.get.table(myConnection=odbcCon, query=query)

  ## get the list of selected 1 year interest rates
  query <- "SELECT Moneta AS Currency FROM DBRisk_free"
  df.selectedOneYearInterest <- sql.get.table(myConnection=odbcCon, query=query)
  
  onCancel <- function()
    {
      close(odbcCon)
      tkdestroy(oneYearInterestTop)
      tkfocus(parent)
    }
  
  onSave <- function()
    {
      ## get the vector of selected interest rates
      selected <- slbx.selectedOneYearInterest[["get.selected"]]()
      ## delete all previously selected interest rates
      query <- "DELETE FROM DBRisk_free"
      result <- sqlCommand(channel=odbcCon,query=query,
                           errorText="DELETE FROM DBRisk_free")
      nbSelected <- length(selected)
      if (nbSelected>0)
        {
          tmp <- paste("'",selected,"'",sep="",collapse=",")
          query <- paste("INSERT INTO DBRisk_free (ID,Ticker,Company,Branche,Moneta)",
                         "SELECT ID,'RF_' + Moneta,'Risk free','RF',Moneta FROM Copia_DBMonete",
                         "WHERE Moneta IN (",tmp,")"
                         )
          
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="insert into table DBRisk_free of new\nselected interest rates")
        }
      close(odbcCon)
      tkdestroy(oneYearInterestTop)
      tkfocus(parent)
    }
  
  ## create the main window
  oneYearInterestTop <- tktoplevel()
  tktitle(oneYearInterestTop) <- "Interest rates selection"
  tkgrab(oneYearInterestTop)
  tkfocus(oneYearInterestTop)
  slbx.selectedOneYearInterest <-
    create_selectionListBox(parent=oneYearInterestTop,
                            title="One year interest",values1=df.oneYearInterest[,"Currency"],
                            values2=df.selectedOneYearInterest[,"Currency"],mode="multiple",width=12,height=11
                            )
  f.saveCancel <- create_saveCancelButton(parent=oneYearInterestTop,onSave=onSave,onCancel=onCancel)
  
  tkgrid(slbx.selectedOneYearInterest[["frame"]],padx=padx,pady=pady)
  tkgrid(f.saveCancel,padx=padx,pady=pady)
  
}

#### SELECT EQUITY UNIVERSE FUNCTION
select.equity.universe <- function(parent)
{
  ## local variable
  externalWindow=parent
  equityWindow.Visible = TRUE
  
  odbcCon = setOdbcConnection("DBMarkowitz")
  
  ## Verify the presence of removed equities
  ## -----------------------------------------------------------------------------
  ##query <- "DELETE FROM DBUniverso_per_ottimizzazione_tmp"
  ##result <- sqlCommand(channel=odbcCon,query=query,
  ##                     errorText="DELETE FROM DBUniverso_per_ottimizzazione_tmp")
  ## -----------------------------------------------------------------------------
  
  ## populate DBUniverso_per_ottimizzazione_tmp
  ## -----------------------------------------------------------------------------
  ##query <- paste("INSERT INTO DBUniverso_per_ottimizzazione_tmp ",
  ##             "SELECT dbo.Universo_Markowitz.ID_strumento, dbo.Universo_Markowitz.ID_AAA, dbo.Universo_Markowitz.Moneta, ",
  ##                 "dbo.Universo_Markowitz.Branche, dbo.Universo_Markowitz.Ticker, dbo.Universo_Markowitz.Company, ",
  ##                 "CASE WHEN DBUniverso_per_ottimizzazione.Company IS NULL THEN 0 ELSE 1 END AS Desiderato ",
  ##             "FROM dbo.Universo_Markowitz LEFT OUTER JOIN ",
  ##             "dbo.DBUniverso_per_ottimizzazione ON dbo.Universo_Markowitz.Ticker = dbo.DBUniverso_per_ottimizzazione.Ticker ",
  ##             "WHERE dbo.Universo_Markowitz.Branche NOT LIKE 'RF'"
  ##             )
  ##result <- sqlCommand(channel=odbcCon,query=query,
  ##                   errorText="Populate the DBUniverso_per_ottimizzazione_tmp")
  ## -----------------------------------------------------------------------------
  
  ## populate DBUniverso_per_ottimizzazione_tmp
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "CREATE TABLE #DBUniverso_per_ottimizzazione_tmp", #
                 "(ID_strumento INT NOT NULL,ID_AAA INT NOT NULL,",
                 "Moneta nvarchar(3) NOT NULL,Branche nvarchar(50) NOT NULL,",
                 "Ticker varchar(20),Company nvarchar(100),Desiderato bit)"
                 ##,",CONSTRAINT PK_DBUniverso_per_ottimizzazione_tmp PRIMARY KEY",
                 ##"(ID_strumento,ID_AAA,Moneta,Branche))"
                 )
  ## create the vector of odbc types because MS SQL does not return the types of
  ## temporary tables "##" tables.
  DBUniversoSqlType = c("int","int","nvarchar","nvarchar","varchar","nvarchar","bit")
  names(DBUniversoSqlType) <- c("ID_strumento","ID_AAA","Moneta","Branche","Ticker",
                                "Company","Desiderato")
  result <- sqlCommand(channel=odbcCon,query=query)
  
  query <- paste("INSERT INTO #DBUniverso_per_ottimizzazione_tmp", 
                 "SELECT dbo.Universo_Markowitz.ID_strumento, dbo.Universo_Markowitz.ID_AAA, dbo.Universo_Markowitz.Moneta, ",
                 "dbo.Universo_Markowitz.Branche, dbo.Universo_Markowitz.Ticker, dbo.Universo_Markowitz.Company, ",
                 "CASE WHEN DBUniverso_per_ottimizzazione.Company IS NULL THEN 0 ELSE 1 END AS Desiderato ",
                 "FROM dbo.Universo_Markowitz LEFT OUTER JOIN ",
                 "dbo.DBUniverso_per_ottimizzazione ON dbo.Universo_Markowitz.Ticker = dbo.DBUniverso_per_ottimizzazione.Ticker ",
                 "WHERE dbo.Universo_Markowitz.Branche NOT LIKE 'RF'"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="Populate the #DBUniverso_per_ottimizzazione_tmp") #
  ## -----------------------------------------------------------------------------
  
  
  

  ## select the list of delete Tickers
  ## -----------------------------------------------------------------------------
  query <- paste(
                 "SELECT * FROM DBUniverso_per_ottimizzazione ",
                 "WHERE Ticker NOT IN ",
                 "(SELECT Ticker FROM Universo_Markowitz)"
                 )
  removed <- tcl.get.table(odbcCon, tableName="DBUniverso_per_ottimizzazione",
                           query=query,requiredFields=list("Moneta","Branche","Ticker","Company"))
  nb.removed <- length(removed$dataFrame[,1])
  ## -----------------------------------------------------------------------------
  
  ## -----------------------------------------------------------------------------

  
  alertForRemoved <- function()
    {
      ## crate toplevel window
      windowEquityRemoved <- tktoplevel()
      tktitle(windowEquityRemoved) <- "Removed equities"
      
      wEquities <- create_odbcTablelist(parent=windowEquityRemoved,data=removed,width=100,
                                        withScrollBarY=TRUE)
      
      closeWindow <- function()
        {
          tkdestroy(windowEquityRemoved)
          constructEquityWindow()
        }
      saveData <- function()
        {
          saveDataFrame(removed[["dataFrame"]])
        }
      
      wFrameEquityRemoved <- tkframe(windowEquityRemoved)
      wCloseButton <- create_button(parent=wFrameEquityRemoved,text="Close",command=closeWindow)
      wSaveButton <- create_button(parent=wFrameEquityRemoved,text="Save List",command=saveData)
      tkgrid(wEquities[["frame"]],padx=padx,pady=pady)
      tkgrid(wCloseButton[["button"]],wSaveButton[["button"]],padx=padx,pady=pady)
      tkgrid(wFrameEquityRemoved,padx=padx,pady=pady)
      tkgrab(windowEquityRemoved)
      tkmessageBox(message="Some equities have been removed from the database!",
                   icon="info",type="ok", parent=windowEquityRemoved)
      equityWindow.Visible <<- FALSE
    }
  
  if (nb.removed > 0)
    {
      alertForRemoved()
    }
  

  ## Function constructEquityWindow
  constructEquityWindow <- function()
    {
      
      ## open the form selection equity universe
      ## create main window
      equityWindow <- tktoplevel()
      tkgrab(equityWindow)
      tktitle(equityWindow) <- "Equity universe selection"
      
      ## get the equity universe
      ## ----------------------------------------------------------------------------
      query <- "ORDER BY Moneta, Branche, Company"
      equityUniverse <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="#DBUniverso_per_ottimizzazione_tmp", #
                                      requiredFields=list("Moneta","Branche","Ticker","Company",Required="Desiderato"),
                                      orderBy=query,updateDb=T,pkFieldNames=c("ID_strumento","ID_AAA","Moneta","Branche"),
                                      odbcFieldTypes=DBUniversoSqlType
                                      )
      ## ----------------------------------------------------------------------------
      
      ## close function for the universe selection window
      OnCancel <- function()
        {
          tkdestroy(equityWindow)
          tkfocus(externalWindow)
          close(odbcCon)
        }
      
      onSave <- function()
        {
          ## Update the table DBUniverso_per_ottimizzazione
          
          wTablelist[["finishediting"]]()
          query <- "DELETE FROM DBUniverso_per_ottimizzazione"
          result <- sqlCommand(channel=odbcCon,query=query)
          query <- paste(
                         "INSERT INTO DBUniverso_per_ottimizzazione (ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company)",
                         "SELECT ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company",
                         "FROM #DBUniverso_per_ottimizzazione_tmp", #
                         "WHERE Desiderato = 1"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          query <- "UPDATE [Prezzi storici azioni]..EquityDB SET InReturnSettoriale = 0"
          result <- sqlCommand(channel=odbcCon,query=query)
            
          query <- paste(
                         "UPDATE [Prezzi storici azioni]..EquityDB SET InReturnSettoriale = 1",
                         "WHERE Ticker IN (SELECT Ticker FROM DBUniverso_per_ottimizzazione)"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          tkmessageBox(message="Selection saved",
                       icon="info",type="ok", parent=equityWindow)
          
          ## insert the new selection into DBUniverso_per_ottimizzazione_storico
          ## select the date of the last insert into DBUniverso_per_ottimizzazione
          query <- "SELECT [Date] AS Data FROM DBUniverso_per_ottimizzazione_storico ORDER BY [Date] DESC"
          result <- sql.get.table(myConnection=odbcCon,query=query)

          currentDate <- Sys.Date()
          if (dim(result)[1] > 0)
            {
              if (as.character(currentDate) == substr(result[1,1],1,10))
                {
                  answer <- tclvalue(
                                     tkmessageBox(
                                                  message=paste("At the present date a selection is still",
                                                    "available \nin the historical database.",
                                                    "Do you want to replace it?"
                                                    ),
                                                  icon="question",type="yesno",default="yes",
                                                  parent=equityWindow
                                                  )
                                     )
                  if (answer=="no")
                    {
                      tkmessageBox(message="The historical selection will not be updated!",
                                   icon="info",type="ok", parent=equityWindow)
                    }
                  else
                    {
                      query <- paste("DELETE FROM DBUniverso_per_ottimizzazione_storico WHERE [Date] ='",
                                     currentDate, "'",sep=""
                                     )
                      result <- sqlCommand(channel=odbcCon,query=query)
                      
                      query <- paste("INSERT INTO DBUniverso_per_ottimizzazione_storico SELECT '",
                                     currentDate,"', ID_strumento, ID_AAA, Moneta, Branche, Ticker, ",
                                     "Company FROM DBUniverso_per_ottimizzazione",sep=""
                                     )
                      result <- sqlCommand(channel=odbcCon,query=query)
                      tkmessageBox(message="Data in DBUnverso_per_ottimizzazione_storico have been replaced",
                                   icon="info",type="ok", parent=equityWindow)
                    }
                }
              else
                {
                  query <- paste(
                                 "INSERT INTO DBUniverso_per_ottimizzazione_storico ",
                                 "SELECT '",currentDate,"', ID_strumento, ID_AAA, Moneta, Branche, ",
                                 "Ticker, Company FROM DBUniverso_per_ottimizzazione",sep=""
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query)
                  tkmessageBox(message="Data have been inserted",
                               icon="info",type="ok", parent=equityWindow)
                }
            }
          else
            {
              query <- paste(
                             "INSERT INTO DBUniverso_per_ottimizzazione_storico ",
                             "SELECT '",currentDate,"', ID_strumento, ID_AAA, Moneta, Branche, ",
                             "Ticker, Company FROM DBUniverso_per_ottimizzazione",sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              tkmessageBox(message="Data have been inserted",
                           icon="info",type="ok", parent=equityWindow)
            }
          
        }
      
      ## create the tk tablelist widget
      wTablelist <-
        create_odbcTablelist(parent=equityWindow,data=equityUniverse,withScrollBarX=TRUE,
                             withScrollBarY=TRUE,width=120,height=40,editable=list(Desiderato="checkbutton"),
                             colFormats=c(rep("no",4),"logical"),updateDb=T)
      
      wFrameUniverse <- tkframe(equityWindow)
      wSaveButton <- create_button(parent=wFrameUniverse,text="Save",command=onSave)
      wCancelButton <- create_button(parent=wFrameUniverse,text="Cancel",command=OnCancel)
      l.navigator <- create_label(parent=equityWindow,value="./Equity universe selection/")
      
      tkgrid(wTablelist[["frame"]],padx=padx,pady=pady)
      tkgrid(wFrameUniverse,padx=padx,pady=pady)
      
      tkgrid(wSaveButton[["button"]],wCancelButton[["button"]],padx=padx,pady=pady)
      tkgrid(l.navigator[[1]],padx=padx,pady=pady,sticky="w")
      tkfocus(equityWindow)
    }
  ## end function constructEquityWindow
  
  if (equityWindow.Visible) constructEquityWindow()
}


constraints <- function() return()


### ------------------------------------------------------------------------ ###
## this is the function associated with the button "Select universe" in the client labelframe
selectClientUniverse <- function (parent)
{
  ## verify the validity of the selected client
  selectedClientId = env[["selectedClientId"]]    
  selectedClient = env[["selectedClient"]]
  selectedCurrency = env[["selectedCurrency"]]
  
  if (selectedClientId=="")
    {
      tkmessageBox(message="No client selected",
                   icon="error",type="ok",parent=parent)          
      return()
    }
  
  ## destroy the parentWindow
  tkdestroy(parent)
  
  ## setup the odbc connection
  odbcCon = setOdbcConnection("DBMarkowitz")
  
  ## get the data to use for the construction of the windows
  ## 1) get the list of currencies
  query <- paste("SELECT DISTINCT Moneta AS Currency FROM DBUniverso_per_ottimizzazione",
                 "WHERE Moneta IN",
                 "(SELECT Moneta from [Prezzi storici azioni].dbo.DBMonete where NotUniversoVAR=0)",
                 "ORDER BY Moneta"
                 )
  
  df.equityCurrencies <- sql.get.table(myConnection=odbcCon, query=query)
  
  
  ## 2) get the list of sectors
  query <- "SELECT DISTINCT Branche AS Sector FROM DBUniverso_per_ottimizzazione ORDER BY Branche"
  df.availableSectors <- sql.get.table(myConnection=odbcCon, query=query)
  
  ## 3) get the list of <currency,sectors> pairs
  query <- "SELECT DISTINCT Moneta AS Currency, Branche AS Sector FROM DBUniverso_per_ottimizzazione ORDER BY Moneta,Branche"
  df.availableCurrencySectors <- sql.get.table(myConnection=odbcCon, query=query)
  
  ## 4) get the list of risk free interest rates
  query <- "SELECT Moneta AS Currency FROM DBRisk_free ORDER BY Moneta"
  ##tcl.df.riskFreeRates <- tcl.get.table(odbcConnection=odbcCon,tableName="DBRisk_free",
  ##query=query,requiredFields=list(Currency="Moneta"),updateDb=F)
  df.riskFreeRates <- sql.get.table(myConnection=odbcCon, query=query)
  
  ## verify the existence of an old selection of risk free interest rates
  query <- paste("SELECT Moneta AS Currency, Branche AS Sector ",
                 "FROM A_risk_free_selezionati ",
                 "WHERE Cliente LIKE '",selectedClient,"'",sep=""
                 )
  df.selectedRiskFree <- sql.get.table(myConnection=odbcCon, query=query)
  
  ## verify the existence of an old selection of <currency,sector>
  query <- paste("SELECT Moneta AS Currency, Branche AS Sector ",
                 "FROM A_monete_e_settori_selezionati ",
                 "WHERE Cliente LIKE '",selectedClient,"'",sep=""
                 )
  df.selectedCurrencySectors <- sql.get.table(myConnection=odbcCon, query=query)
  
  wasSelectedRiskFree <- nrow(df.selectedRiskFree) > 0
  wasSelectedCurrencySectors <- nrow(df.selectedCurrencySectors) > 0
  
  if (wasSelectedRiskFree)
    {
      deleteRiskFree <-
        tclvalue(tkmessageBox(
                              message="Previously selected risk free rates.\nDo you want to delete them?",
                              icon="question",type="yesno",default="no")
                 )
    }
  else
    {
      deleteRiskFree <- "yes"
    }
  
  if (wasSelectedCurrencySectors)
    {
      deleteCurrencySectors <-
        tclvalue(tkmessageBox(
                              message="Previously selected <currency,sector> pairs.\nDo you want to delete them?",
                              icon="question",type="yesno",default="no")
                 )
    }
  else
    {
      deleteCurrencySectors <- "yes"
    }
  
  showSectors <- function()
    {
      ## get the list of selected equity currencies
      selectedCurrencies <- slbx.currencies[["get.selected"]]()
      if (length(selectedCurrencies) == 0)
        {
          tkmessageBox(message="Please select a currency",icon="error",type="ok",
                       parent=window.SectorsCurrencies)
          return()
        }
      isSelected <- is.element(df.availableCurrencySectors[,"Currency"],selectedCurrencies)
      requiredSectors <- df.availableCurrencySectors[isSelected,"Sector"]
      if (length(requiredSectors) == 0)
        {
          tkmessageBox(message="No sectors associated to the selected currencies",
                       icon="error",type="ok",parent=window.SectorsCurrencies)
          return()
        }
      requiredSectors <- unique(requiredSectors)
      requiredSectors <- requiredSectors[order(requiredSectors)]
      
      tkconfigure(b.showCurrencySectors[["button"]],state="normal")
      slbx.sectors[["reset"]](widget=2)
      slbx.sectors[["set.values"]](newValues=requiredSectors,setOrder=TRUE,widget=1)
    }
  
  showCurrencySectors <- function()
    {
      ## get the list of selected currencies
      selectedCurrencies <- slbx.currencies[["get.selected"]]()
      ## get the list of selected sectors
      selectedSectors <- slbx.sectors[["get.selected"]]()
      if (length(selectedCurrencies)==0 | length(selectedSectors)==0)
        {
          tkmessageBox(message="Please select at least a currency and a sector",
                       icon="error",type="ok",parent=window.SectorsCurrencies)
          return()
        }
      isSelectedCurrency <- is.element(df.availableCurrencySectors[,"Currency"],selectedCurrencies)
      isSelectedSector <- is.element(df.availableCurrencySectors[,"Sector"],selectedSectors)
      isRequired <- isSelectedCurrency & isSelectedSector
      df.requiredCurrencySectors <- df.availableCurrencySectors[isRequired,]
      nb.rows <- nrow(df.requiredCurrencySectors)
      if (nb.rows == 0)
        {
          tkmessageBox(message="No <currency,sectors> pairs associated to the selection",
                       icon="error",type="ok",parent=window.SectorsCurrencies)
          return()
        }
      dimnames(df.requiredCurrencySectors)[[1]] <- 1:nb.rows
      ## delete old selection in both dataframes
      sltl.currencySectors[["reset"]]()
      sltl.currencySectors[["reset"]](widget=2)
      sltl.currencySectors[["insert.data.frame"]](newValues=df.requiredCurrencySectors)
    }
  
  ## create the function associated with the button "Equities selection"
  equitiesSelection <- function()
    { 
      ## se non serve cancella // parent=window.SectorsCurrencies
      ## 0) Check the validity of the sectorsCurrencies selection, if empty display a message
      selectedCurrencySectors <- sltl.currencySectors[["get.selected"]]()
      nb.rows <- nrow(selectedCurrencySectors)
      if (nb.rows == 0)
        {
          tkmessageBox(message="No <currency,sectors> pairs associated to the selection",
                       icon="error",type="ok",parent=window.SectorsCurrencies)
          return()
        }
      
      
      ## create the window
      window.EquitiesSelection <- tktoplevel()
      tktitle(window.EquitiesSelection) <- "Client's equities selection"
      tkgrab(window.EquitiesSelection)
      
      
      client <- env[["selectedClient"]]
      
      ## Remove the old selection for of the desired client
      query <- paste("DELETE FROM A_monete_e_settori_selezionati WHERE Cliente LIKE '", client, "'",sep="")
      result <- sqlCommand(channel=odbcCon,query=query)
      
      query <- paste("INSERT INTO A_monete_e_settori_selezionati VALUES ('",selectedCurrencySectors[,1],"','",selectedCurrencySectors[,2],"','",client,"')",sep="",collapse=" ; ")
      result <- sqlCommand(channel=odbcCon,query=query)
      
      ## tkmessageBox(message="<Currency,Sectors> successfully inserted.",icon="info",type="ok",parent=window.SectorsCurrencies)
      
      ## Remove the table tabella A_selezione_singolo_titolo
      result <- sqlCommand(channel=odbcCon,query="DELETE A_selezione_singolo_titolo")
      
      ## Create the table A_selezione_singolo_titolo again from the view "Selezione_singolo_titolo_per_cliente" which is
      ## a join between the tables DBUniverso_per_ottimizzazione and A_monete_e_settori_selezionati
      query <- paste("INSERT INTO A_selezione_singolo_titolo SELECT dbo.Selezione_singolo_titolo_per_cliente.Moneta, dbo.Selezione_singolo_titolo_per_cliente.Branche, dbo.Selezione_singolo_titolo_per_cliente.Ticker, ",
                     "dbo.Selezione_singolo_titolo_per_cliente.Company, CASE WHEN dbo.A_titoli_selezionati.Company IS NULL THEN 0 ELSE 1 END AS Desiderato ",
                     "FROM dbo.Selezione_singolo_titolo_per_cliente LEFT OUTER JOIN ",
                     "dbo.A_titoli_selezionati ON dbo.Selezione_singolo_titolo_per_cliente.Cliente = dbo.A_titoli_selezionati.Cliente AND ",
                     "dbo.Selezione_singolo_titolo_per_cliente.Ticker = dbo.A_titoli_selezionati.Ticker ",
                     "WHERE dbo.Selezione_singolo_titolo_per_cliente.Cliente LIKE '", client ,"' ",
                     "ORDER BY dbo.Selezione_singolo_titolo_per_cliente.Moneta, dbo.Selezione_singolo_titolo_per_cliente.Branche, ",
                     "dbo.Selezione_singolo_titolo_per_cliente.Company ",sep=""
                     )
      result <- sqlCommand(channel=odbcCon,query=query)
      
      ## open the window containing the selected equities
      A_selezioneSqlType = c("nvarchar","nvarchar","nvarchar","nvarchar","bit")
      names(A_selezioneSqlType) <- c("Moneta","Branche","Ticker","Company","Desiderato")
      ## ----------------------------------------------------------------------------
      query <- "ORDER BY Moneta, Branche, Company"
      clientEquityUniverse <- tcl.get.table(odbcConnection=odbcCon,
                                            tableName="A_selezione_singolo_titolo",
                                            requiredFields=list(Currency="Moneta",Sector="Branche","Ticker",
                                              "Company",Required="Desiderato"),
                                            orderBy=query,updateDb=T,pkFieldNames=c("Ticker"),
                                            odbcFieldTypes=A_selezioneSqlType)
      ## ----------------------------------------------------------------------------
      
      
      
      ## create the tk tablelist widget
      tbl.clientEquityUniverse <- create_odbcTablelist(parent=window.EquitiesSelection,data=clientEquityUniverse,withScrollBarX=TRUE,
                                                       withScrollBarY=TRUE,width=120,height=40,editable=list(Desiderato="checkbutton"),
                                                       colFormats=c(rep("no",4),"logical"),updateDb=T)
      
      tkgrid(tbl.clientEquityUniverse[["frame"]],padx=padx,pady=pady)
      
      ## create the functions used in the buttonframe
      onSave <- function()
        {
          ## terminate the editing of the table
          tbl.clientEquityUniverse[["finishediting"]]()
            
          ## get the number of desired equities
          query = "SELECT COUNT(*) AS nr_desiderati FROM A_selezione_singolo_titolo WHERE Desiderato = 1"
          result <- sql.get.table(myConnection=odbcCon, query=query)
            
          if (result[1,"nr_desiderati"] == 0)
            {
              tkmessageBox(message="No equities selected!",icon="error",type="ok",parent=window.EquitiesSelection)
              return()
            }
            
          query = paste("DELETE FROM A_titoli_selezionati WHERE Cliente LIKE '",env[["selectedClient"]],"'",sep="")
          result <- sqlCommand(channel=odbcCon,query=query)
            
          query <- paste("INSERT INTO A_titoli_selezionati (Cliente, Moneta, Branche, Ticker, Company) ",
                         "SELECT '",env[["selectedClient"]], "', Moneta, Branche, Ticker, Company ",
                         "FROM A_selezione_singolo_titolo ",
                         "WHERE Desiderato = 1",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
            
          ## remove the pairs <currency,sector>, i.e. <moneta, settore>, from A_monete_e_settori_selezionati
          ## and fill it with the selected <currency,sector> pairs.
          query = paste("DELETE FROM A_monete_e_settori_selezionati WHERE Cliente LIKE '",env[["selectedClient"]],"'",sep="")
          result <- sqlCommand(channel=odbcCon,query=query)
          query <- paste("INSERT INTO A_monete_e_settori_selezionati (Moneta, Branche, Cliente) ",
                         "SELECT Moneta, Branche, '",env[["selectedClient"]],"' ",
                         "FROM A_titoli_selezionati ",
                         "WHERE Cliente LIKE '",env[["selectedClient"]],"' ",
                         "GROUP BY Moneta, Branche",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          tkmessageBox(message="Selection saved!",icon="info",type="ok",parent=window.EquitiesSelection)
        }
      onCancel <- function()
        {
          tkdestroy(window.EquitiesSelection)
          tkfocus(window.SectorsCurrencies)
        }
      onCheckAll <- function()
        {
          nb.rows = nrow(clientEquityUniverse[["dataFrame"]])
          if (nb.rows > 0)
            {
              clientEquityUniverse[["dataFrame"]][,5] <<- "1"
              tbl.clientEquityUniverse[["remove.rows"]](index=1:nb.rows)
              tbl.clientEquityUniverse[["insert.data.frame"]](dataFrame=clientEquityUniverse[["dataFrame"]])
              query <- "UPDATE A_selezione_singolo_titolo SET Desiderato = 1"
              result <- sqlCommand(channel=odbcCon,query=query)
            }
          ##if (nb.rows > 0)
          ##  {
          ##    tableListID = tbl.clientEquityUniverse[["tablelist"]]$ID
          ##    .Tcl(paste(tableListID," cellconfigure ", 0:(nb.rows-1),",4 -text 1",sep="",collapse=";"))
          ##    print (nb.rows)
          ##    print (paste(tableListID," cellconfigure ", 0:(nb.rows-1),",4 -text 1",sep="",collapse=";"))
          ##  }
        }
      onUncheckAll <- function()
        {
          nb.rows = nrow(clientEquityUniverse[["dataFrame"]])
          if (nb.rows > 0)
            {
              clientEquityUniverse[["dataFrame"]][,5] <<- "0"
              tbl.clientEquityUniverse[["remove.rows"]](index=1:nb.rows)
              tbl.clientEquityUniverse[["insert.data.frame"]](dataFrame=clientEquityUniverse[["dataFrame"]])
              query <- "UPDATE A_selezione_singolo_titolo SET Desiderato = 0"
              result <- sqlCommand(channel=odbcCon,query=query)
            }
        }
      
      ## create the button frame
      f.buttonFrame <- tkframe(parent=window.EquitiesSelection)
      
      ## create the save botton
      b.save <- create_button(parent=f.buttonFrame,text="Save",command=onSave)
      
      ## create the cancel botton
      b.cancel <- create_button(parent=f.buttonFrame,text="Cancel",command=onCancel)
      
      ## create the "check all" botton
      b.checkAll <- create_button(parent=f.buttonFrame,text="Check all",command=onCheckAll)
      
      ## create the "uncheck all" botton
      b.uncheckAll <- create_button(parent=f.buttonFrame,text="Uncheck all",command=onUncheckAll)
      
      ## create the navigator bar
      l.navigator <- create_label(parent=window.EquitiesSelection,value="./Client's equities and risk free interest rates selection/Equities selection")
      tkgrid(b.save[["button"]],b.cancel[["button"]],b.checkAll[["button"]],b.uncheckAll[["button"]],padx=padx,pady=pady)
      tkgrid(f.buttonFrame)
      tkgrid(l.navigator[["label"]],padx=padx,pady=pady,sticky="nw")
      tkfocus(window.EquitiesSelection)
      
    }
  
  
  ## 0) construct the main window
  window.SectorsCurrencies <- tktoplevel()
  tktitle(window.SectorsCurrencies) <- "Client's universe selection"
  tkgrab(window.SectorsCurrencies)
  tkfocus(window.SectorsCurrencies)
  
  ## 1) construct the left frame
  f.left <- tkframe(parent=window.SectorsCurrencies)
  ## tkconfigure(f.left,relief="groove",borderwidth=2)
  ## 1.1) the name of the client and the currency
  l.client <- create_label(parent=f.left,value=paste(selectedClient, selectedCurrency,sep=" - "))
  
  
  ## 2) construct the upper right frame (Equities selection)
  lf.right.up <- tkwidget(window.SectorsCurrencies,"labelframe",text="Selection from the equity universe",labelanchor="n")
  
  ## 2.1) construct the tablelist with the currency of the equity universe
  if (deleteCurrencySectors=="yes")
    {
      selectedCurrencies <- NULL
    }
  else
    {
      selectedCurrencies <- unique(df.selectedCurrencySectors[,"Currency"])
    }
  slbx.currencies <- create_selectionListBox(parent=lf.right.up,title="Currency",
                                             values1=df.equityCurrencies[,"Currency"],values2=selectedCurrencies,mode="multiple",
                                             width=12,height=11,grabWindow=window.SectorsCurrencies)

  ## 2.2) construct the button "Show sectors"
  b.showSectors <- create_button(parent=lf.right.up,text="Show sectors",command=showSectors)
  
  ## 2.3) construct the tablelist with the sectors
  if (deleteCurrencySectors=="yes")
    {
      slbx.sectors <-
        create_selectionListBox(parent=lf.right.up,title="Sector",
                                values1=NULL,values2=NULL,mode="multiple",
                                width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  else
    {
      selectedSectors <- unique(df.selectedCurrencySectors[,"Sector"])
      slbx.sectors <-
        create_selectionListBox(parent=lf.right.up,title="Sector",
                                values1=df.availableSectors[,"Sector"],values2=selectedSectors,
                                mode="multiple",width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  
  
  ## 2.4) construct the button "Show <currency,sectors>"
  b.showCurrencySectors <- create_button(parent=lf.right.up,text="Show <currency,sectors>",command=showCurrencySectors)
  
  ## 2.5) construct the tablelist with the currency,sector pairs
  if (deleteCurrencySectors=="yes")
    {
      df1 <- data.frame(Currency=NA,Sector=NA)[-1,]
      df2 <- df1
    }
  else
    {
      df1 <- df.availableCurrencySectors
      df2 <- df.selectedCurrencySectors
    }
  
  f.currencySectors <- tkframe(lf.right.up)
  sltl.currencySectors <-
    create_selectionTablelist(parent=f.currencySectors,dataFrame1=df1,
                              title="Currency and sector",dataFrame2=df2,width=30,height=15,
                              alignCols=rep("left",2),alignColsLabel=rep("center",2),withScrollBarY=TRUE,
                              multiSortColumn=T,selectmode="multiple",rTypes=rep("character",2),
                              grabWindow=window.SectorsCurrencies
                              )
  rm(df1,df2)
  
  ## 2.6) construct the frame containing the equities selection and Cancel buttons
  f.selectionCancel <- tkframe(f.currencySectors)
  ## 2.7) construct the button "Equities selection"
  b.equitiesSelection <- create_button(parent=f.selectionCancel,text="Select equities",command=equitiesSelection)
  
  ## 2.8) construct the button "Cancel"
  onCancel <- function() {tkdestroy(window.SectorsCurrencies); start_window(updateSelection=TRUE)}
  b.cancel <- create_button(parent=f.selectionCancel,text="Cancel",command=onCancel)
  
  ## 3.1) construct the lower left subframe (Risk free interest rate and other rates)
  lf.left.low <- tkwidget(window.SectorsCurrencies,"labelframe",text="Interest rates universe",labelanchor="n")
  
  ## construct the tablelist with the currencies of the interest rates universe
  ##tl.interestRates <- create_odbcTablelist(
  ##                       parent=lf.left.low,data=tcl.df.riskFreeRates,width=20,height=8,
  ##                       alignCols="left",alignColsLabel="center",withScrollBarY=TRUE,
  ##                       sortColumn=T,selectmode="multiple",updateDb=F
  ##                       )
  if (deleteRiskFree=="yes")
    {
      sltl.interestRates <-
        create_selectionListBox(parent=lf.left.low,title="Interest rates",
                                values1=df.riskFreeRates[,"Currency"],values2=NULL,mode="multiple",
                                width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  else
    {
      sltl.interestRates <-
        create_selectionListBox(parent=lf.left.low,title="Interest rates",
                                values1=df.riskFreeRates[,"Currency"],values2=df.selectedRiskFree[,"Currency"],
                                mode="multiple",width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  
  ## 3.2) create the onSave and onCancel functions used in the Risk free interest rates by the saveCancelButton defined below
  onSave <- function()
    {
      ## save the interest rates
      selectedRiskFreeIntRates <- sltl.interestRates[["get.selected"]]()
      
      if (is.null(selectedRiskFreeIntRates))
        {
          tkmessageBox(message="No risk free interest rates selected",
                       icon="error",type="ok",parent=window.SectorsCurrencies)
          return()
        }
      
      ## remove the previously selected risk free interest rates
      query <- paste("DELETE FROM A_risk_free_selezionati WHERE Cliente='",env[["selectedClient"]],"'",sep="")
      result <- sqlCommand(channel=odbcCon,query=query)
      
      ## insert the selected interest rates
      query <- paste("INSERT INTO A_risk_free_selezionati VALUES ('",selectedRiskFreeIntRates,"', 'Risk free', '",env[["selectedClient"]],"')",sep="",collapse=";")
      
      result <- sqlCommand(channel=odbcCon,query=query)
      tkmessageBox(message="Selected risk free interest rates saved",
                   icon="info",type="ok",parent=window.SectorsCurrencies)
    }
  b.saveCancel <- create_saveCancelButton(parent=lf.left.low,onSave=onSave,onCancel=onCancel)
  
  
  ## 3.3) construct the navigator label
  l.navigator <- create_label(parent=window.SectorsCurrencies,value="./Client's equities and risk free interest rates selection/")
  
  
  ## fill the left frame
  tkgrid.configure(f.left,columnspan=2)
  ## place the client name
  tkgrid(tklabel(parent=f.left,text="Client:"),l.client[["label"]],
         padx=padx,pady=pady,sticky="n")
  
  ## place the upper subframe
  tkgrid(lf.right.up,padx=padx,pady=pady,sticky="n",columnspan=2)
  ## place the currencies, sectors and currencySectors tablelist
  tkgrid(slbx.currencies[["frame"]],slbx.sectors[["frame"]], padx=padx,
         pady=pady,sticky="n")
  
  tkgrid(f.currencySectors,padx=padx,pady=pady,sticky="n",row=0,column=2,rowspan=3)
  tkgrid(sltl.currencySectors[["frame"]])
  tkgrid(b.equitiesSelection[["button"]],b.cancel[["button"]],padx=padx)
  tkgrid(f.selectionCancel,padx=padx,pady=pady)
  
  tkgrid(b.showSectors[["button"]],padx=padx,row=1,column=0,sticky="n")
  tkgrid(b.showCurrencySectors[["button"]],padx=padx,row=1,column=1,sticky="n")
  
  ## disable some tablelist and buttons
  ##tkconfigure(b.showCurrencySectors[["button"]],state="disabled")
  
  ## place the lower subframe
  tkgrid(lf.left.low,padx=padx,pady=pady,sticky="nw")
  ## place the interest rate tablelist
  tkgrid(sltl.interestRates[["frame"]],padx=padx,pady=pady)
  ## place the saveCancel button
  tkgrid(b.saveCancel)
  
  ## place the navigator label
  tkgrid(l.navigator[["label"]],padx=padx,pady=pady,sticky="nw")
  return()
}


## this function opens the windows leading to the optimization procedure
optimizePortfolio <- function(parent)
{
  ## verify the validity of the selected client
  selectedClientId = env[["selectedClientId"]]    
  selectedClient = env[["selectedClient"]]
  selectedCurrency = env[["selectedCurrency"]]
 
  if (selectedClientId=="")
    {
      tkmessageBox(message="No client selected",
                   icon="error",type="ok",parent=parent)          
      return()
    }
  
  ## destroy the parentWindow
  tkdestroy(parent)
  
  ## create and prepare the feedback windows
  top <- tktoplevel()
  feedbackWin <- create_feedback(parent=top,steps=100,
                                 "Status: Preparing optimization...           ")
  tkgrid(feedbackWin[[1]],padx=padx,pady=pady)
  tktitle(top) <- "Feedback window"
  tkgrab(top)
  tkfocus(top)

  feedbackWin[["status"]](10)
  
  
  ## setup the odbc connection
  odbcCon = setOdbcConnection("DBMarkowitz")
  
  query <- "DELETE FROM Expected_return_cliente_tmp"
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="Function optimizePortfolio:\nDELETE FROM Expected_return_cliente_tmp")
  query <- paste("INSERT INTO Expected_return_cliente_tmp ",
                 "SELECT Moneta, BrancheMarkowitz, Company, Ticker, [Expected Return] AS [Exp Ret Inv], 0 AS [Expected Return], Ticker1 ",
                 "FROM (SELECT A.Moneta, 'FX' AS BrancheMarkowitz, '' AS Company, A.Moneta AS Ticker, A.Return_fx_vs_MonetaInvestimento AS [Expected Return],A.Moneta AS Ticker1 FROM I_expected_return_fx_vs_moneta_investimento A INNER JOIN Elenco_clienti B ON A.MonetaInvestimento = B.MonetaInvestimento AND A.Moneta <> B.MonetaInvestimento WHERE (B.Cliente ='",
                 selectedClient,"') UNION SELECT Moneta, 'Moneta' AS BrancheMarkowitz, '' AS Company, Ticker, Consensus_AAA AS [Expected Return], 'RF_' + Moneta AS Ticker1 FROM I_expected_return_rf UNION SELECT A.Moneta, A.BrancheMarkowitz, B.Company, A.Ticker, A.Exp_return_azione, A.Ticker AS Ticker1 FROM Expected_returns_azionari A LEFT OUTER JOIN Copia_EquityDB B ON A.Ticker = B.Ticker) DERIVEDTBL",
                 sep=""
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="Function optimizePortfolio:\nINSERT INTO Expected_return_cliente_tmp ...")
  query <- paste("CREATE VIEW Step1_tmp AS ",
                 "(SELECT A.*, B.Expected_return AS Expected_return_mon_rif ",
                 "FROM Expected_return_cliente_tmp A LEFT OUTER JOIN ",
                 "S_Expected_returns_moneta_riferimento B ON A.Ticker1 = B.Ticker ",
                 "WHERE (B.Moneta_riferimento = '",selectedCurrency,"'))",
                 sep=""
                 )
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="Function optimizePortfolio:\nINSERT INTO Expected_return_cliente_tmp ...")
  
  query <- "UPDATE Step1_tmp set [Expected Return] = Expected_return_mon_rif"
  result <- sqlCommand(channel=odbcCon,query=query,
                       errorText="Function optimizePortfolio:\nUPDATE Step1_tmp ...")
    
  query <- "DROP VIEW Step1_tmp"
  result <- sqlCommand(channel=odbcCon,query=query,
                         errorText="Function optimizePortfolio:\nDROP VIEW Step1_tmp")
  
  optimizationCurrency <- env[["optimizationCurrency"]]
  if (selectedCurrency !=  optimizationCurrency)
    {
      assign("optimizationCurrency",selectedCurrency,envir=env) 
      optimizationCurrency <- selectedCurrency
      AA_seleziona_matrice_covarianza(currency=optimizationCurrency,odbcCon=odbcCon)
    }
    
  feedbackWin[["status"]](20)
  ## prepare the data for optimization
  preparazione_dati_ottimizzazione_step_1(client=selectedClient,moneta_investimento=selectedCurrency,channel=odbcCon)

  feedbackWin[["status"]](70)
  ## prepare the covariance of the sectors
  preparazione_covarianze_settoriali(client=selectedClient,moneta_riferimento=selectedCurrency,channel=odbcCon)
  
  feedbackWin[["status"]](100)
  
  ## check the availability of all covariances
  query <- "SELECT COUNT(*) AS number FROM E_covarianze_settoriali WHERE Covarianza IS NULL"
  nbRows <- select.count(odbcCon,query=query)
    
  if (nbRows>0)
    {
      MsgBox ("Ci sono covarianze settoriali nulle nella matrice !")
        tkmessageBox(message="Some sector covariances in E_covarianze_settoriali are missing.\nOptimization stopped.",
                     icon="error",type="ok",parent=top)
      tkgrab.release(top)
      tkdestroy(top)
      
      return()
    }
    rm(nbRows)
  
  
  ## check the availability of all sectorial returns expectations
  query <- paste("SELECT COUNT(*) AS number FROM E_expected_returns_per_cliente_finali ",
                 "WHERE cliente LIKE '",selectedClient,"' AND Expectation IS NULL",
                 sep=""
                 )
  nbRows <- select.count(odbcCon,query=query)
  if (nbRows>0)
    {
      tkmessageBox(message="Table E_expected_returns_per_cliente_finali contains NULL Expactations.\nOptimization stopped.",
                   icon="error",type="ok",parent=top)
      tkgrab.release(top)
      tkdestroy(top)
      
      return()
    }
  rm(nbRows)
  
  ##If (fs.FileExists("c:\R\error.txt")) Then
  ##    Set f = fs.opentextfile("c:\R\error.txt", ForReading = 1)
  ##    errore = f.ReadLine
  ##    MsgBox "Attenzione, la procedura in R non si è conclusa correttamente!"
  ##    MsgBox errore
  ##Else
  ##    MsgBox "Ottimizzazione terminata"
  ##End If
  
  ##Exit Sub
  
  ##Messaggio_errore:
  ##    MsgBox "Errore nella procedura Markowitz.R: ottimizzazione fallita!"
  ##    If (fs.FileExists("c:\R\error.txt")) Then
  ##        Set f = fs.opentextfile("c:\R\error.txt")
  ##        errore = f.ReadLine
  ##        MsgBox errore
  ##        f.Close
  ##    End If
  
  
  close(odbcCon)
  ## destroy the feedbackWindow
  tkgrab.release(top)
  tkdestroy(top)
  

  ## setup the odbc connection
  ##odbcCon = setOdbcConnection("DBMarkowitz")
  ##close(odbcCon)
  
  optimizePortfolio.topWindow <- tktoplevel()
  name <- as.character(optimizePortfolio.topWindow)
  .Tcl(paste("focus",name))
  tktitle(optimizePortfolio.topWindow) <- "Optimization"
  
  
  ## create the frame containing the parametrisation, i.e. client, varLimit and VarConfidenceLevel
  f.parametrization <- create_labelFrame(parent=optimizePortfolio.topWindow,text="Parametrization")
  ## create the label client
  l.client <- create_label(parent=f.parametrization[["labelFrame"]],value=
                           paste("Client:  ",selectedClient," - ",selectedCurrency,sep=""))
  
  ## create the label VaR Limit
  l.varLimit <- create_label(parent=f.parametrization[["labelFrame"]],value="VAR limit:")
  ##e.varLimit <- create_entry(parent=f.parametrization[["labelFrame"]],value=paste(env[["selectedVarLimit"]]*100,"%",sep=""),
  ##                           width="10",dataFormat=list(type="%",option=-1))
  e.varLimit <- create_entry(parent=f.parametrization[["labelFrame"]],value=env[["selectedVarLimit"]],
                             width="10")
  
  ## crete the label VaR confidence
  l.varConfidenceLevel <- create_label(parent=f.parametrization[["labelFrame"]],
                                       value="VAR confidence level:")
  e.varConfidenceLevel <- create_entry(parent=f.parametrization[["labelFrame"]],
                                       value=env[["selectedVarConfidenceLevel"]],width="10")
  ## create the frame containing the buttons
  f.buttons <- tkframe(optimizePortfolio.topWindow)
  ## create the button "Messages"
  b.messages <- create_button(parent=f.buttons,text="Messages",width="15",command=function(){})
  ## create the button "Show graphics"
  b.graphics <- create_button(parent=f.buttons,text="Graphics",width="15",command=function(){})
  ## create the button "Correlations"
  b.correlations <- create_button(parent=f.buttons,text="Correlations",width="15",command=function(){})
  ## create the button "Update optimal weights"
  b.udateOptimalWeights <- create_button(parent=f.buttons,text="Update opt. weights",width="15",command=function(){})
  ## create the button "Reset weights"
  b.resetWeights <- create_button(parent=f.buttons,text="Reset weights",width="15",command=function(){})
  ## create the button "Optimize again"
  b.optimizeAgain <- create_button(parent=f.buttons,text="Optimize again",width="15",command=function(){})
  
  
  ## insert the labelframe parametrisation
  tkgrid(f.parametrization[["labelFrame"]],padx=padx,pady=pady)
  ## insert the client label
  tkgrid(l.client[["label"]],sticky="w",padx=padx,pady=pady,columnspan=2)
  ## insert the VAR limit
  tkgrid(l.varLimit[["label"]],e.varLimit[["entry"]],sticky="w",padx=padx,pady=pady)
  ## insert the VAR Confidence Level
  tkgrid(l.varConfidenceLevel[["label"]],e.varConfidenceLevel[["entry"]],sticky="w",padx=padx,pady=pady)
  
  ## insert the buttons frame
  tkgrid(f.buttons,padx=padx,pady=pady)
  tkgrid(b.messages[["button"]],b.graphics[["button"]],padx=padx,pady=pady)
  tkgrid(b.correlations[["button"]],b.udateOptimalWeights[["button"]],padx=padx,pady=pady)
  tkgrid(b.resetWeights[["button"]],b.optimizeAgain[["button"]],padx=padx,pady=pady)
  tkgrab(optimizePortfolio.topWindow)
  
  return()
}



start_window <- function(updateSelection=FALSE)
{
  ## create functions used later
  ## update values in the global variable (env)
  updateGlobal <- function()
    {
      assign("selectedClientId",cb.client[["get.id"]](),envir=env)
      if (env[["selectedClientId"]]>0)
        {
          tmp <- env[["df.clients"]][[env[["selectedClientId"]],1]]
          assign("selectedClient",tmp,envir=env)
          tmp <- env[["df.clients"]][[env[["selectedClientId"]],2]]
          assign("selectedCurrency",tmp,envir=env)
          #tmp <- as.numeric(chop(e.varLimit[["get.value"]]()))/100
           
          assign("selectedVarLimit",e.varLimit[["get.value"]](),envir=env)
          #tmp <- as.numeric(chop(e.varConfidenceLevel[["get.value"]]()))/100
          assign("selectedVarConfidenceLevel",e.varConfidenceLevel[["get.value"]](),envir=env)  
        }
    }
  
  update.data.sourcesLocal <- function()
    {
      update.data.sources(topWindow)
    }
  
  select.equity.universeLocal <- function()
    {
      select.equity.universe(topWindow)
    }
  
  select.oneYearInterest.universeLocal <- function()
    {
      select.oneYearInterest.universe(topWindow)
    }
  
  selectClientUniverseLocal <- function()
    {
      updateGlobal()
      ## verify that a client has been selected
      if (env[["selectedClientId"]] == 0)
        {
          tkmessageBox(message="No client selected!",icon="error",type="ok",parent=topWindow)
          return()
        }
      selectClientUniverse(topWindow)
    }
  
  optimizePortfolioLocal <- function()
    {
      updateGlobal()
      ## verify that a client has been selected
      if (env[["selectedClientId"]] == 0)
        {
          tkmessageBox(message="No client selected!",icon="error",type="ok",parent=topWindow)
          return()
        }
      optimizePortfolio(topWindow)
    }
  
  remove.client <- function()
    {
      id <- cb.client[["get.id"]]()
      client <- env[["df.clients"]][id,1]
      ## remove the client from the table in the database
      query <- paste(
                     "DELETE FROM Elenco_clienti WHERE Cliente='",
                     client,"'",sep=""
                     )
      odbcCon = setOdbcConnection("DBMarkowitz")
      result <- sqlCommand(channel=odbcCon,query=query,
                           errorText=query)
      
      if (result == 1)
        {
          assign("df.clients",sql.get.table(odbcCon, "Elenco_clienti"),envir=env)
          if (nrow(env[["df.clients"]])!=0)
            {
              cb.client[["modify.values"]](paste(env[["df.clients"]][,1],env[["df.clients"]][,2],sep=" - "))
            }
          else
            {
              cb.client[["modify.values"]](vector(mode="character"))              
            }
          ## set the client id equal to the empty string      
          cb.client[["set.selection"]]("")
          assign("selectedClientId","",envir=env)
          assign("selectedClient","",envir=env)
          assign("selectedCurrency","",envir=env)
        }
      close(odbcCon)
    }
  
  add.client <- function()
    {
      onOk <- function()
        {
          ## get the new values
          client <- entryClient[["get.value"]]()
          currency <- comboCurrency[["get.selection"]]()
            
          ## check the validity of the Currency
          odbcCon = setOdbcConnection("DBMarkowitz")
            
          ## select the list of valid currencies
          query <- paste("SELECT COUNT(*) AS Moneta FROM Copia_DBMonete WHERE MonetaRiferimento = 1 ",
                         "AND Moneta LIKE '",currency,"'",
                         sep=""
                         )
            
          nbCurrency <- select.count(channel=odbcCon,query=query)
          if (nbCurrency != 1)
            {
              tkmessageBox(message="No reference currency available or wrong code!\nLook in Copia_DBMonete!",
                           icon="error",type="ok",parent=top)
              return()
              close(odbcCon)
            }
            
          ## remove leading and trailing spaces
          client <- sub(' *$','',sub('^ *','',client))
            
          ## check for a non empty name
          bad <- (length(client)==0 )
          if (!bad) bad <- (client == "")
          if (bad)
            {
              tkmessageBox(message="Null strings are not accepted!",icon="error",type="ok",parent=top)
              close(odbcCon)
              return()
            }
            
          ## refresh the name in case of removed leading or trailing spaces
          entryClient[["set.value"]](client)
            
            
          ## insert them into the database
          query <- paste(
                         "INSERT INTO Elenco_clienti (Cliente,MonetaInvestimento) VALUES (",
                         "'",client,"','",currency,"')",sep=""
                         )

          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText=query)
          if (result == 1)
            {
              ## update the widget
              assign("df.clients",sql.get.table(odbcCon, "Elenco_clienti"),envir=env)
              cb.client[["modify.values"]](paste(env[["df.clients"]][,1],env[["df.clients"]][,2],sep=" - "))
              cb.client[["set.selection"]](paste(client,currency,sep=" - "))
              updateGlobal()
            }
          close(odbcCon)
          tkdestroy(top)
        }
      onCancel <- function()
        {
          tkdestroy(top)
        }
      
      top <- tktoplevel()
      tkgrab(top)
      label <- create_label(parent=top,value="New client and currency:")
      entryClient <- create_entry(parent=top,width="18")
      comboCurrency <- create_combo(parent=top,values=c("CHF","EUR","USD"),width=5)
      buttonFrame <- create_okCancelButton(parent=top,onOk=onOk,onCancel=onCancel)
      
      tkgrid(label[["label"]],padx=padx,pady=pady)
      tkgrid(entryClient[["entry"]],padx=padx,pady=pady)
      tkgrid(comboCurrency[["combo"]],padx=padx,pady=pady)
      tkgrid(buttonFrame,padx=padx,pady=pady)
      tkfocus(entryClient[["entry"]])
    }
  
  ## ###########################################################################
  ## create the function used for the constraints setup
  constraintSetup <- function()
    {
      query <- "DELETE FROM B_monete_e_settori_selezionati"
      result <- sqlCommand(channel=odbcCon,query=query)
      
      query <- "DELETE FROM B_monete_selezionate"
      result <- sqlCommand(channel=odbcCon,query=query)
      
      query <- "DELETE FROM B_settori_selezionati"
      result <- sqlCommand(channel=odbcCon,query=query)
      
      query <- "DELETE FROM B_Restrizioni_multiple"
      result <- sqlCommand(channel=odbcCon,query=query)
      
      query <- "DELETE FROM B_risk_free_selezionati"
      result <- sqlCommand(channel=odbcCon,query=query)
      
      
      
      client <- env[["selectedClient"]]
      
      query = paste("SELECT * FROM C_Restrizioni_monete_e_settori WHERE Cliente LIKE '",client,"'",sep="")
      currencySectorConstr <- sql.get.table(odbcCon, query=query)
      
      if (nrow(currencySectorConstr) == 0)
        {
          
          ## populate the table B_moneta_e_settori_selezionati
          query <- paste("INSERT INTO B_monete_e_settori_selezionati (Moneta, Branche)",
                         "SELECT DISTINCT Moneta, Branche FROM A_titoli_selezionati ",
                         "WHERE Cliente LIKE '",client,"' ",
                         "ORDER BY Moneta, Branche",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          ## populate the table B_monete_selezionate
          ##1) the currencies coming from risk_free_selezionati
          query <- paste("INSERT INTO B_monete_selezionate (Moneta)",
                         "SELECT DISTINCT Moneta FROM A_risk_free_selezionati ",
                         "WHERE Cliente LIKE '",client,"' ",
                         "ORDER BY Moneta",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          ##2) the currencies from the selected sectors
          query <- paste("INSERT INTO B_monete_selezionate (Moneta)",
                         "SELECT DISTINCT Moneta FROM A_titoli_selezionati ",
                         "WHERE Cliente LIKE '",client,"' ",
                         "AND Moneta NOT IN (SELECT DISTINCT Moneta FROM A_risk_free_selezionati ",
                         "WHERE Cliente LIKE '",client,"')")
          result <- sqlCommand(channel=odbcCon,query=query)
          
          ## populate the table B_settori_selezionati
          query <- paste("INSERT INTO B_settori_selezionati (Branche)",
                         "SELECT DISTINCT Branche FROM A_titoli_selezionati ",
                         "WHERE Cliente LIKE '",client,"' ",
                         "ORDER BY Branche",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
        }
      else
        {
          answer <- tclvalue(tkmessageBox(message="There are old restrictions. Remove them?",icon="question",type="yesno",default="no"))
          
          if (answer == "yes")
            {
              ## populate the table B_moneta_e_settori_selezionati
              query <- paste("INSERT INTO B_monete_e_settori_selezionati (Moneta, Branche)",
                             "SELECT DISTINCT Moneta, Branche FROM A_titoli_selezionati ",
                             "WHERE Cliente LIKE '",client,"' ",
                             "ORDER BY Moneta, Branche",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              
              ## populate the table B_monete_selezionate
              ## 1) The currencies from the risk_free_selezionati
              query <- paste("INSERT INTO B_monete_selezionate (Moneta)",
                             "SELECT DISTINCT Moneta FROM A_risk_free_selezionati ",
                             "WHERE Cliente LIKE '",client,"' ",
                             "ORDER BY Moneta",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## 2) The currencies from the selected sectors
              query <- paste("INSERT INTO B_monete_selezionate (Moneta)",
                             "SELECT DISTINCT Moneta FROM A_titoli_selezionati ",
                             "WHERE Cliente LIKE '",client,"' ",
                             "AND Moneta NOT IN (SELECT DISTINCT Moneta FROM A_risk_free_selezionati ",
                             "WHERE Cliente LIKE '",client,"')",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## populate the table B_settori_selezionati
              query <- paste("INSERT INTO B_settori_selezionati (Branche)",
                             "SELECT DISTINCT Branche FROM A_titoli_selezionati ",
                             "WHERE Cliente LIKE '",client,"' ",
                             "ORDER BY Branche",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
            }
          else
            {
              ## populate the table B_moneta_e_settori_selezionati
              query <- paste("INSERT INTO B_monete_e_settori_selezionati (Moneta, Branche, Min, Min_suggerito, Max, Max_suggerito) ",
                             "SELECT DISTINCT A_monete_e_settori_selezionati.Moneta, A_monete_e_settori_selezionati.Branche, ",
                             "C_Restrizioni_monete_e_settori.[Min], C_Restrizioni_monete_e_settori.Min_suggerito, ",
                             "C_Restrizioni_monete_e_settori.[Max], C_Restrizioni_monete_e_settori.Max_suggerito ",
                             "FROM C_Restrizioni_monete_e_settori RIGHT OUTER JOIN ",
                             "A_monete_e_settori_selezionati ON C_Restrizioni_monete_e_settori.Cliente = A_monete_e_settori_selezionati.Cliente AND ",
                             "C_Restrizioni_monete_e_settori.Branche = A_monete_e_settori_selezionati.Branche AND ",
                             "C_Restrizioni_monete_e_settori.Moneta = A_monete_e_settori_selezionati.Moneta ",
                             "WHERE A_monete_e_settori_selezionati.Cliente LIKE '",client,"'",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## populate the table riempimento della tabella B_monete_selezionate
              ## 1) Le monete provenienti dai risk_free_selezionati
              query <- paste("INSERT INTO B_monete_selezionate (Moneta, Min, Min_suggerito, Max, Max_suggerito) ",
                             "SELECT DISTINCT A_risk_free_selezionati.Moneta, C_Restrizioni_monete.[Min], C_Restrizioni_monete.Min_suggerito, ",
                             "C_Restrizioni_monete.[Max] , C_Restrizioni_monete.Max_suggerito ",
                             "FROM C_Restrizioni_monete RIGHT OUTER JOIN ",
                             "A_risk_free_selezionati ON C_Restrizioni_monete.Moneta = A_risk_free_selezionati.Moneta AND ",
                             "C_Restrizioni_monete.Cliente = A_risk_free_selezionati.Cliente ",
                             "WHERE A_risk_free_selezionati.Cliente LIKE '",client,"'",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## 2) The currencies from the selected sectors
              query <- paste("INSERT INTO B_monete_selezionate (Moneta, Min, Min_suggerito, Max, Max_suggerito) ",
                             "SELECT DISTINCT A_monete_e_settori_selezionati.Moneta, C_Restrizioni_monete.[Min], C_Restrizioni_monete.Min_suggerito, ",
                             "C_Restrizioni_monete.[Max] , C_Restrizioni_monete.Max_suggerito ",
                             "FROM C_Restrizioni_monete RIGHT OUTER JOIN ",
                             "A_monete_e_settori_selezionati ON C_Restrizioni_monete.Moneta = A_monete_e_settori_selezionati.Moneta AND ",
                             "C_Restrizioni_monete.Cliente = A_monete_e_settori_selezionati.Cliente ",
                             "WHERE A_monete_e_settori_selezionati.Cliente LIKE '",client,"' ",
                             "AND A_monete_e_settori_selezionati.Moneta NOT IN (SELECT DISTINCT Moneta FROM A_risk_free_selezionati ",
                             "WHERE Cliente LIKE '",client,"')",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## populate the table B_settori_selezionati
              query <- paste("INSERT INTO B_settori_selezionati (Branche, Min, Min_suggerito, Max, Max_suggerito) ",
                             "SELECT DISTINCT A_monete_e_settori_selezionati.Branche, C_Restrizioni_settore.[Min], ",
                             "C_Restrizioni_settore.Min_suggerito , C_Restrizioni_settore.[Max], C_Restrizioni_settore.Max_suggerito ",
                             "FROM C_Restrizioni_settore RIGHT OUTER JOIN ",
                             "A_monete_e_settori_selezionati ON C_Restrizioni_settore.Branche = A_monete_e_settori_selezionati.Branche AND ",
                             "C_Restrizioni_settore.Cliente = A_monete_e_settori_selezionati.Cliente ",
                             "WHERE (A_monete_e_settori_selezionati.Cliente LIKE '",client,"')",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
              
              ## Verify the existence of multiple restrictions.
              ## If necessary execute a loop over the old restrictions and select only the pairs <currency,sector>
              ## still in the client's universe
              
              query <- paste("SELECT MAX(NumeroRestrizione) AS Max_numero_restrizioni FROM C_restrizioni_multiple WHERE Cliente LIKE '",client,"'",sep="")
              nb.multiple.restr = select.count(channel=odbcCon,query=query)
              
              if (nb.multiple.restr > 0)
                {
                  index = 1
                  index_multiple_restr = 1
                  for (index in 1:nb.multiple.restr)
                    {
                      query <- paste("SELECT TipoRestrizione, ValoreRestrizione FROM A_titoli_selezionati ",
                                     "INNER JOIN C_Restrizioni_multiple ON A_titoli_selezionati.Cliente = C_Restrizioni_multiple.Cliente AND ",
                                     "A_titoli_selezionati.Moneta = C_Restrizioni_multiple.Moneta AND ",
                                     "A_titoli_selezionati.Branche = C_Restrizioni_multiple.Branche ",
                                     "WHERE (A_titoli_selezionati.Cliente LIKE '",client,"') AND (dbo.C_Restrizioni_multiple.NumeroRestrizione = ",index,")",
                                     sep=""
                                     )
                      result <- sql.get.table(odbcCon, query=query)
                      
                      ## if the recordset is empty do nothing
                      if (nrow(result) != 0)
                        {
                          ## store the values of TipoRestrizione and ValoreRestrizione and use them in the INSERT.
                          ## Questo è necessario in quanto il LEFT OUTER JOIN tra le due tabelle fa apparire NULL per
                          ## quei <moneta,settore> che non hanno una corrispondenza nella tabella di destra,
                          ## cioe' per quei <moneta,settore> che sono "nuovi" nell'universo selezionato dal cliente
                          typeConstraint = result[1,"TipoRestrizione"]
                          valueConstraint = result[1,"ValoreRestrizione"]
                          query <- paste("INSERT INTO B_Restrizioni_multiple (NumeroRestrizione, Moneta, Branche, TipoRestrizione, ValoreRestrizione, Selezionato) ",
                                         "SELECT DISTINCT ",index_multiple_restr,", C_Restrizioni_multiple.Moneta, C_Restrizioni_multiple.Branche, C_Restrizioni_multiple.TipoRestrizione, ",
                                         "C_Restrizioni_multiple.ValoreRestrizione, CASE WHEN C_Restrizioni_multiple.Selezionato IS NULL ",
                                         "THEN 0 ELSE C_Restrizioni_multiple.Selezionato END ",
                                         "FROM C_Restrizioni_multiple INNER JOIN A_titoli_selezionati ON A_titoli_selezionati.Cliente = C_Restrizioni_multiple.Cliente AND ",
                                         "A_titoli_selezionati.Moneta = C_Restrizioni_multiple.Moneta And A_titoli_selezionati.Branche = dbo.C_Restrizioni_multiple.Branche ",
                                         "WHERE A_titoli_selezionati.Cliente LIKE '",client,"' AND (C_Restrizioni_multiple.NumeroRestrizione = ",index,
                                         "OR C_Restrizioni_multiple.NumeroRestrizione IS NULL)",
                                         sep=""
                                         )
                          result <- sqlCommand(channel=odbcCon,query=query)
                          index_multiple_restr = index_multiple_restr + 1
                        }
                      
                      index = index + 1
                    } ## end for
                } ## end if
            }
        }
      
      ## Check the constraints with respect to the risk free selection
      query <- paste("SELECT * FROM C_Restrizioni_risk_free WHERE Cliente LIKE '",client,"'",sep="")
      result <- sql.get.table(odbcCon, query=query)
      
      ## if there are not restrictions
      if (nrow(result) == 0)
        {
          query <- paste("INSERT INTO B_risk_free_selezionati (Moneta, Branche)",
                         "SELECT DISTINCT Moneta, Branche FROM A_risk_free_selezionati ",
                         "WHERE Cliente LIKE '",client,"' ",
                         "ORDER BY Moneta, Branche",
                         sep=""
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
        }
      else
        {
          answer <- tclvalue(tkmessageBox(message="There are old risk free restrictions. Remove them?",icon="question",type="yesno",default="no"))
          
          if (answer == "yes")
            {
              ## populate the table with the content of A_risk_free_selezionati of the selected client
              query <- paste("INSERT INTO B_risk_free_selezionati (Moneta, Branche)",
                             "SELECT DISTINCT Moneta, Branche FROM A_risk_free_selezionati ",
                             "WHERE Cliente LIKE '",client,"' ",
                             "ORDER BY Moneta, Branche",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
            }
          else
            {
              query <- paste("INSERT INTO B_risk_free_selezionati (Moneta, Branche, Min, Min_suggerito, Max, Max_suggerito) ",
                             "SELECT A_risk_free_selezionati.Moneta, A_risk_free_selezionati.Branche, C_Restrizioni_risk_free.[Min], ",
                             "C_Restrizioni_risk_free.Min_suggerito , C_Restrizioni_risk_free.[Max], C_Restrizioni_risk_free.Max_suggerito ",
                             "FROM A_risk_free_selezionati LEFT OUTER JOIN C_Restrizioni_risk_free ON A_risk_free_selezionati.Cliente = C_Restrizioni_risk_free.Cliente AND ",
                             "A_risk_free_selezionati.Moneta = C_Restrizioni_risk_free.Moneta AND A_risk_free_selezionati.Branche = C_Restrizioni_risk_free.Branche ",
                             "WHERE (A_risk_free_selezionati.Cliente LIKE '",client,"')",
                             sep=""
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
            }
        }
    }

 
  create.constraints.window <- function()
    {
    
      ## verify that the env[["selectedClientId"]] != 0 
      updateGlobal()
      
      ## verify that a client has been selected
      if (env[["selectedClientId"]] == 0)
        {
          tkmessageBox(message="No client selected!",icon="error",type="ok",parent=topWindow)
          return()
        }
        
      ## destroy the  main window
      tkdestroy(topWindow)
      
      ## open a connection to DBMarkowitz
      odbcCon = setOdbcConnection("DBMarkowitz")
      
      ## Define function used by the multiple constraints buttons
      verifyMultipleConstrSelection <- function()
        {
          selectedValue <- cb.constraintId[["get.selection"]]()
          if (selectedValue == "")
            {
              tkmessageBox(message="No constraint selected",icon="error",type="ok")
              return (FALSE)
            }
          return (TRUE)     
        }
      
      onNew <- function()
        {
          onCancel <- function()
            {
              tkdestroy(topWindow)
              tkgrab(window.constraints)
              tkfocus(window.constraints)
              return()
            }
        
          onInsert <- function()
            {
              ## finish editing
              tbl.values[["finishediting"]]()
              constrValue = entry.value[["get.value"]]()
            
              ## verify that the entry field
              if (constrValue=="")
                {
                  tkmessageBox(message="Invalid constraint value",icon="error",type="ok")
                  return ()
                }
            
              ## get the number of constraint
              if (tbl.multipleConstr[["get.nbrows"]]() > 0)
                {
                
                  nb.constraint = max(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]])) + 1
                }
              else
                {
                  nb.constraint = 1
                }
  
            
              ## verify that at least one is selected
              isRequired <- values[["dataFrame"]][,"Required"] == 1
              if (sum(isRequired)==0)
                {
                  tkmessageBox(message="The selection is empty",icon="error",type="ok")
                  return ()
                }
              
              ## construct the query
              equality <- radio.equality[["get.selection"]]()
              query <- paste("INSERT INTO B_Restrizioni_multiple ",
                             "(NumeroRestrizione,Moneta,Branche,TipoRestrizione,ValoreRestrizione,Selezionato) ",
                             "VALUES(",nb.constraint,",'",values[["dataFrame"]][,"Currency"],"',",
                             "'",values[["dataFrame"]][,"Sector"],"',","'",equality,"',",constrValue,",",
                             values[["dataFrame"]][,"Required"],")",
                             sep="",collapse=";"
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
                                               
              ## update the combobox with the number of constraints and the tcl widget
              cb.constraintId[["modify.values"]](values=1:nb.constraint)
              dataFrameToAdd <- cbind(Id=nb.constraint,values[["dataFrame"]][isRequired,c("Currency","Sector")],
                                      Type=I(equality),Value=I(constrValue)
                                      )
  
              nbNewRows = tbl.multipleConstr[["get.nbrows"]]()
              tbl.multipleConstr[["add.rows"]](dataFrameToAdd)
              tkdestroy(topWindow)
              tkfocus(window.constraints)
              return()
            } ## end onInsert
        
          onCheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "1"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }
          
          onUncheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "0"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }
        
          query <- paste("SELECT Moneta, Branche, Selezionato",
                         "FROM B_monete_e_settori_selezionati ",
                         "ORDER BY Moneta, Branche"
                         )
                      
          valuesSqlType <- c("nvarchar","nvarchar","bit")
          names(valuesSqlType) <- c("Moneta","Branche","Selezionato")
          values <- tcl.get.table(odbcConnection=odbcCon,tableName="B_monete_e_settori_selezionati",query=query,
                                  requiredFields=list(Currency="Moneta",Sector="Branche",Required="Selezionato"),
                                  updateDb=F,odbcFieldTypes=valuesSqlType)
                            
          topWindow <- tktoplevel()
          tktitle(topWindow) <- "New multiple constraint"
        
        
          ## create the tk tablelist widget
          frame.values <- tkframe(parent=topWindow)
          tbl.values <- create_odbcTablelist(parent=frame.values,data=values,withScrollBarX=TRUE,
                                             withScrollBarY=TRUE,width=40,editable=list(Selezionato="checkbutton"),
                                             height=30,colFormats=c("no","no","logical"))


          ## create the check and uncheck buttons
          frame.checkButtons <- tkframe(parent=topWindow)
          b.checkAll <- create_button(parent=frame.checkButtons,text="Check all",command=onCheckAll)
          b.uncheckAll <- create_button(parent=frame.checkButtons,text="Uncheck all",command=onUncheckAll)
        
         
          ## create the labeled radio widget
          radio.equality <- create_labeledRadio(parent=topWindow,values=c(">=","<="," ="))
          entry.value <- create_entry(parent=topWindow,value="",width="6")
        
          ## create the insert and cancel buttons
          frame.buttons <- tkframe(parent=topWindow)
          b.insert <- create_button(parent=frame.buttons,text="Insert",command=onInsert)
          b.cancel <- create_button(parent=frame.buttons,text="Cancel",command=onCancel)
          
          ## insert in the window
          ## the tablelist
          tkgrid(tbl.values[["frame"]],padx=padx,pady=pady)
          tkgrid(frame.values,padx=padx,pady=pady,columnspan=2)
        
          ## the checkAll buttons
          tkgrid(b.checkAll[["button"]],b.uncheckAll[["button"]],padx=padx,pady=pady)
          tkgrid(frame.checkButtons,padx=padx,pady=pady,columnspan=2)
        
          ## the radio and entry 
          tkgrid(radio.equality[["frame"]],padx=padx,pady=pady,row=2,sticky="e")
          tkgrid(entry.value[["entry"]],padx=padx,pady=pady,row=2,column=1,sticky="w")
        
          ## the insert and cancel buttons
          tkgrid(b.insert[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
          tkgrid(frame.buttons,padx=padx,pady=pady,columnspan=2)
        
          ## set the grab and focus
          tkgrab(topWindow)
          tkfocus(topWindow)
        
        }
      ## end onNew
    
      onModify <- function()
        {
          if (!verifyMultipleConstrSelection()) return ()

          ## get the number of the restriction
          constraintId <- cb.constraintId[["get.selection"]]()
        
          onCancel <- function()
            {
              tkdestroy(topWindow)
              tkgrab(window.constraints)
              tkfocus(window.constraints)
              return()
            }

          onUpdate <- function()
            {
              ## finish editing
              tbl.values[["finishediting"]]()
              constrValue = entry.value[["get.value"]]()

              ## verify that the entry field
              if (constrValue=="")
                {
                  tkmessageBox(message="Invalid constraint value",icon="error",type="ok")
                  return ()
                }
  
            
              ## verify that at least one is selected
              isRequired <- values[["dataFrame"]][,"Required"] == 1
              if (sum(isRequired)==0)
                {
                  tkmessageBox(message="The selection is empty",icon="error",type="ok")
                  return ()
                }

              ## remove previous data
              query <- paste("DELETE FROM B_Restrizioni_multiple ",
                             "WHERE NumeroRestrizione =",constraintId
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
            
              ## insert the new values 
              equality <- radio.equality[["get.selection"]]()
              query <- paste("INSERT INTO B_Restrizioni_multiple ",
                             "(NumeroRestrizione,Moneta,Branche,TipoRestrizione,ValoreRestrizione,Selezionato) ",
                             "VALUES(",constraintId,",'",values[["dataFrame"]][,"Currency"],"',",
                             "'",values[["dataFrame"]][,"Sector"],"',","'",equality,"',",constrValue,",",
                             values[["dataFrame"]][,"Required"],")",
                             sep="",collapse=";"
                             )
              result <- sqlCommand(channel=odbcCon,query=query)
                                                 
              ## sort the tbl.multipleConstr widget accordint to the NumeroRestrizione, Moneta, Branche columns
              ## tbl.multipleConstr[["SortByColumns"]](index=c(1,2,3))
            
              ## remove all rows with the selected constraintId
              colConstraintId <- tbl.multipleConstr[["get.columns"]](1)
              toRemove <- colConstraintId[,1] == constraintId
              idToRemove <- (1:length(toRemove))[toRemove]
              tbl.multipleConstr[["remove.rows"]](index=idToRemove)
 
              ## insert the new values for the corresponding constraintId
              toInsert <- values[["dataFrame"]][,"Required"] == 1
              newValues <- data.frame(Id=I(constraintId),values[["dataFrame"]][toInsert,1:2],Type=I(equality),Value=I(constrValue))           
              tbl.multipleConstr[["add.rows"]](newRows=newValues)
            
              ## adjust the ordering 
              tbl.multipleConstr[["SortByColumns"]](index=c(1,2,3))
              tbl.multipleConstr[["resetSort"]]()
            
              tkdestroy(topWindow)
              tkfocus(window.constraints)
              return()
            } ## end onUpdate
        
          onCheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "1"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }
          onUncheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "0"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }
        
        
          query <- paste("SELECT Moneta, Branche, Selezionato",
                         "FROM B_Restrizioni_multiple",
                         "WHERE NumeroRestrizione =",constraintId,
                         "ORDER BY A.Moneta, A.Branche"
                         )
                      
          valuesSqlType <- c("nvarchar","nvarchar","bit")
          names(valuesSqlType) <- c("Moneta","Branche","Selezionato")
          values <- tcl.get.table(odbcConnection=odbcCon,tableName="B_monete_e_settori_selezionati",query=query,
                                  requiredFields=list(Currency="Moneta",Sector="Branche",Required="Selezionato"),
                                  updateDb=F,odbcFieldTypes=valuesSqlType)
                           
          topWindow <- tktoplevel()
          tktitle(topWindow) <- paste("Update multiple constraint",constraintId)
        
        
          ## create the tk tablelist widget
          frame.values <- tkframe(parent=topWindow)
          tbl.values <- create_odbcTablelist(parent=frame.values,data=values,withScrollBarX=TRUE,
                                             withScrollBarY=TRUE,width=40,editable=list(Selezionato="checkbutton"),
                                             height=30,colFormats=c("no","no","logical"))


          ## create the check and uncheck buttons
          frame.checkButtons <- tkframe(parent=topWindow)
          b.checkAll <- create_button(parent=frame.checkButtons,text="Check all",command=onCheckAll)
          b.uncheckAll <- create_button(parent=frame.checkButtons,text="Uncheck all",command=onUncheckAll)
        
         
          ## create the labeled radio and entry widgets after having selected the
          ## actual selected values
          query <- paste("SELECT TipoRestrizione AS TypeOfConstraint, ValoreRestrizione AS constraintValue",
                         "FROM B_Restrizioni_multiple",
                         "WHERE NumeroRestrizione =",constraintId
                         )
          result <- sql.get.table(odbcCon, query=query)
          radio.equality <- create_labeledRadio(parent=topWindow,values=c(">=","<="," ="))
          radio.equality[["set.selection"]](value=result[1,"TypeOfConstraint"])
          entry.value <- create_entry(parent=topWindow,value=result[1,"constraintValue"],width="6")
        
          ## create the insert and cancel buttons
          frame.buttons <- tkframe(parent=topWindow)
          b.update <- create_button(parent=frame.buttons,text="Update",command=onUpdate)
          b.cancel <- create_button(parent=frame.buttons,text="Cancel",command=onCancel)
          
          ## insert in the window
          ## the tablelist
          tkgrid(tbl.values[["frame"]],padx=padx,pady=pady)
          tkgrid(frame.values,padx=padx,pady=pady,columnspan=2)
        
          ## the checkAll buttons
          tkgrid(b.checkAll[["button"]],b.uncheckAll[["button"]],padx=padx,pady=pady)
          tkgrid(frame.checkButtons,padx=padx,pady=pady,columnspan=2)
        
          ## the radio and entry 
          tkgrid(radio.equality[["frame"]],padx=padx,pady=pady,row=2,sticky="e")
          tkgrid(entry.value[["entry"]],padx=padx,pady=pady,row=2,column=1,sticky="w")
        
          ## the insert and cancel buttons
          tkgrid(b.update[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
          tkgrid(frame.buttons,padx=padx,pady=pady,columnspan=2)
        
          ## set the grab and focus
          tkgrab(topWindow)
          tkfocus(topWindow)
        
        }
      ## end OnModify
    
      
      onRemove <- function()
        {
          ## verify the selection
          if (!verifyMultipleConstrSelection()) return ()
        
          ## remove from the database
          constraintId <- cb.constraintId[["get.selection"]]()
          query <- paste("DELETE FROM B_Restrizioni_multiple WHERE NumeroRestrizione = ",constraintId)
          result <- sqlCommand(channel=odbcCon,query=query)

          ## use the length function in order to determine if the result of the
          ## expression is numeric(0)
          nb.constraint <- length(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]]))
          if (nb.constraint != 0) nb.constraint <- max(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]]))
        
          ## update the number of constraints
          if (nb.constraint > 1)
            {
              cb.constraintId[["modify.values"]](values=1:(nb.constraint-1))
            }
          else
            {
              cb.constraintId[["modify.values"]](values=NULL)
            }
          cb.constraintId[["set.selection"]](value="")

          ## update the tk widget
          if (nb.constraint > 0)
            {
              ## get the tablelist column of Id
              columnId <- tbl.multipleConstr[["get.columns"]](1)
              toRemove <- columnId[,1] == constraintId
            
              ## select the rows to keep
              newId <- as.numeric(as.vector(columnId[!toRemove,1]))
            
              ## compute the indices to remove
              tmp <- 1:length(toRemove)
              toRemove <- tmp[toRemove]
            
              ## remove from the tablelist widget
              tbl.multipleConstr[["remove.rows"]](toRemove)
            
              ## if remaining rows adjust the Id
              if (length(newId)>0)
                {
                  ## determine which row has Id > the those removed
                  gtConstraintId <- newId > constraintId
                  if (sum(gtConstraintId) > 0)
                    {
                      newId[gtConstraintId] <- newId[gtConstraintId] - 1
                      tbl.multipleConstr[["set.column"]](1,newId)
                    }
                } 
            }
          return()
        } ## end onRemove
    
      ## end function used by the multiple constraints buttons
    
         
      ## 0) construct the main window
      window.constraints <- tktoplevel()
      tktitle(window.constraints) <- "Constraints selection"
    
      ## Construct the widgets
      ## 0.1) construct the label with the client name    
      l.client <- create_label(parent=window.constraints,value =
                           paste("Client:  ",env[["selectedClient"]]," - ",
                                 env[["selectedCurrency"]],sep="")
                                 )
 
      ## 1) <risk free> constraints
      ## 1.1) get the data
      RiskFreeSqlType <- c("nvarchar","nvarchar","float","float")
      names(RiskFreeSqlType) <- c("Moneta","Branche","Min","Max")
      query <- "ORDER BY Moneta"
      riskFreeConstr <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="B_risk_free_selezionati",
                                      requiredFields=list(Currency="Moneta","Branche","Min","Max"),
                                      orderBy=query,updateDb=T,pkFieldNames=c("Moneta","Branche"),
                                      odbcFieldTypes=RiskFreeSqlType)



      ## 1.2) create the tk tablelist widget
      lf.riskFree <- create_labelFrame(parent=window.constraints,text="Risk free constraints",labelanchor="n")
      tbl.riskFreeConstr <- create_odbcTablelist(parent=lf.riskFree[["labelFrame"]],data=riskFreeConstr,withScrollBarX=TRUE,
                                                 alignCols=c("center","center","right","right"),withScrollBarY=TRUE,width=40,
                                                 height=10,editable=list(Min="Entry",Max="Entry"),
                                                 colFormats=c("no","no","percent2","percent2"),updateDb=T)



      ## 2) <currency> constraints
      ## 2.1) get the data
      currencySqlType <- c("nvarchar","float","float")
      names(currencySqlType) <- c("Moneta","Min","Max")
      query <- "ORDER BY Moneta"
      currencyConstr <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="B_monete_selezionate",
                                      requiredFields=list(Currency="Moneta","Min","Max"),
                                      orderBy=query,updateDb=T,pkFieldNames=c("Moneta"),
                                      odbcFieldTypes=currencySqlType)

      ## 2.2) create the tk tablelist widget
      lf.currency <- create_labelFrame(parent=window.constraints,text="Currency constraints",labelanchor="n")
      tbl.currencyConstr <- create_odbcTablelist(parent=lf.currency[["labelFrame"]],data=currencyConstr,withScrollBarX=TRUE,
                                                 alignCols=c("center","right","right"),withScrollBarY=TRUE,width=40,
                                                 height=10,editable=list(Min="Entry",Max="Entry"),
                                                 colFormats=c("no","percent2","percent2"),updateDb=T)

    
      ## 3) <sector> constraints
      ## 3.1) get the data
      sectorSqlType <- c("nvarchar","float","float")
      names(sectorSqlType) <- c("Branche","Min","Max")
      query <- "ORDER BY Branche"
      sectorConstr <- tcl.get.table(odbcConnection=odbcCon,
                                    tableName="B_settori_selezionati",
                                    requiredFields=list(Sector="Branche","Min","Max"),
                                    orderBy=query,updateDb=T,pkFieldNames=c("Branche"),
                                    odbcFieldTypes=sectorSqlType)

      ## 3.2) create the tk tablelist widget
      lf.sector <- create_labelFrame(parent=window.constraints,text="Sector constraints",labelanchor="n")
      tbl.sectorConstr <- create_odbcTablelist(parent=lf.sector[["labelFrame"]],data=sectorConstr,withScrollBarX=TRUE,
                                               alignCols=c("center","right","right"),withScrollBarY=TRUE,width=40,
                                               height=10,editable=list(Min="Entry",Max="Entry"),
                                               colFormats=c("no","percent2","percent2"),updateDb=T)    
      ## 4) <currency,sector> constraints
      ## 4.1) get the data
      currencySectorSqlType <- c("nvarchar","nvarchar","float","float")
      names(currencySectorSqlType) <- c("Moneta","Branche","Min","Max")
      query <- "ORDER BY Branche"
      currencySectorConstr <- tcl.get.table(odbcConnection=odbcCon,
                                            tableName="B_monete_e_settori_selezionati",
                                            requiredFields=list(Currency="Moneta",Sector="Branche","Min","Max"),
                                            orderBy=query,updateDb=T,pkFieldNames=c("Moneta","Branche"),
                                            odbcFieldTypes=currencySectorSqlType)

      ## 4.2) create the tk tablelist widget
      lf.currencySector <- create_labelFrame(parent=window.constraints,text="<Currency,Sector> constraints",labelanchor="n")
      tbl.currencySectorConstr <- create_odbcTablelist(parent=lf.currencySector[["labelFrame"]],data=currencySectorConstr,withScrollBarX=TRUE,
                                                       alignCols=c("center","center","right","right"),withScrollBarY=TRUE,width=50,
                                                       height=40,editable=list(Min="Entry",Max="Entry"),
                                                       colFormats=c("no","no","percent2","percent2"),updateDb=T)
    
      ## 5) <multiple> constraints
      ## 5.1) get the data
      multipleConstraintsSqlType <- c("int","nvarchar","nvarchar","nvarchar","float")
      names(multipleConstraintsSqlType) <- c("NumeroRestrizione","Moneta","Branche","TipoRestrizione","ValoreRestrizione")
      query <- "WHERE Selezionato=1 ORDER BY NumeroRestrizione,Moneta,Branche"
      multipleConstr <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="B_Restrizioni_multiple",
                                      requiredFields=list(Id="NumeroRestrizione",Currency="Moneta",Sector="Branche",
                                        Type="TipoRestrizione",Value="ValoreRestrizione"),
                                      orderBy=query,updateDb=F,odbcFieldTypes=multipleConstraintsSqlType)

      ## 5.2) create the tk tablelist widget
      lf.multipleConstr <- create_labelFrame(parent=window.constraints,text="Multiple constraints",labelanchor="n")
      tbl.multipleConstr <- create_odbcTablelist(parent=lf.multipleConstr[["labelFrame"]],data=multipleConstr,withScrollBarX=TRUE,
                                                 alignCols=c("center","center","center","center","right"),withScrollBarY=TRUE,width=60,
                                                 height=40,colFormats=c("no","no","no","no","percent2"))
    
      ## 5.3) create the button frame and the buttons
      f.multipleConstrButtons <- tkframe(lf.multipleConstr[["labelFrame"]])
      b.New <- create_button(parent=f.multipleConstrButtons,text="New",command=onNew)
      b.Modify <- create_button(parent=f.multipleConstrButtons,text="Modify",command=onModify)
      b.Remove <- create_button(parent=f.multipleConstrButtons,text="Remove",command=onRemove)
      ## determine the number of multiple constraints
      query <- "SELECT MAX(NumeroRestrizione) AS maxid FROM B_Restrizioni_multiple"
      result <- sql.get.table(odbcCon, query=query)
      if (is.na(result[1,1]))
        {
          cb.constraintId <- create_combo(parent=window.constraints,startValue=NA,editable=FALSE,autocomplete=FALSE)
        }
      else
        {
          cb.constraintId <- create_combo(parent=window.constraints,values=c(1:as.numeric(result[1,1])),
                                          startValue=NA,editable=FALSE,autocomplete=FALSE) 
        }
      ## fill the button frame
      tkgrid(b.New[["button"]],cb.constraintId[["combo"]],
             b.Modify[["button"]],b.Remove[["button"]],padx=padx,pady=pady)      
    
    
      ## 6) create the last button frame with the Save and Cancel buttons
      ## 6.1 create the onSave and onCancel functions
      onSave <- function()
        {
          tables = c("C_Restrizioni_monete_e_settori","C_Restrizioni_monete",
                     "C_Restrizioni_settore","C_Restrizioni_multiple",
                     "C_Restrizioni_risk_free"
                     )
          query <- paste("DELETE FROM ",tables," WHERE Cliente LIKE '",env[["selectedClient"]],"'",sep="",collapse=";")
          result <- sqlCommand(channel=odbcCon,query=query)


          query <- paste("INSERT INTO C_Restrizioni_monete_e_settori ", 
                         "SELECT '",env[["selectedClient"]],"', Moneta, ", 
                         "Branche, Min, Min_suggerito, Max, Max_suggerito  ", 
                         "FROM B_monete_e_settori_selezionati ", 
                         "ORDER BY Moneta, Branche"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)


          query <- paste("INSERT INTO C_Restrizioni_monete ", 
                         "SELECT '",env[["selectedClient"]],"', Moneta, ", 
                         "Min, Min_suggerito, Max, Max_suggerito  ", 
                         "FROM B_monete_selezionate ", 
                         "ORDER BY Moneta"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          
          query <- paste("INSERT INTO C_Restrizioni_settore ", 
                         "SELECT '",env[["selectedClient"]],"', Branche, ", 
                         "Min, Min_suggerito, Max, Max_suggerito  ", 
                         "FROM B_settori_selezionati ", 
                         "ORDER BY Branche"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)


          query <- paste("INSERT INTO C_Restrizioni_multiple ", 
                         "SELECT '",env[["selectedClient"]],"', NumeroRestrizione, ", 
                         "Moneta, Branche, TipoRestrizione, ValoreRestrizione, Selezionato  ", 
                         "FROM B_Restrizioni_multiple"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)


          query <- paste("INSERT INTO C_Restrizioni_risk_free ", 
                         "SELECT '",env[["selectedClient"]],"', Moneta, ", 
                         "Branche, Min, Min_suggerito, Max, Max_suggerito  ", 
                         "FROM B_risk_free_selezionati ", 
                         "ORDER BY Moneta, Branche"
                         )
          result <- sqlCommand(channel=odbcCon,query=query)
          
          
          tkmessageBox(message="Data have been saved!",icon="info",type="ok",parent=window.constraints) 
          tkdestroy(window.constraints)
          start_window(updateSelection=TRUE)          
                                                    
        }
      
      onCancel <- function() {tkdestroy(window.constraints); start_window(updateSelection=TRUE)}
        
      
      f.mainButtons <- tkframe(window.constraints)
      b.Save <- create_button(parent=f.mainButtons,text="Save",command=onSave)
      b.Cancel <- create_button(parent=f.mainButtons,text="Cancel",command=onCancel)
      tkgrid(b.Save[["button"]],b.Cancel[["button"]],padx=padx,pady=pady)
      
      ## setup the widgets
      tkgrid(l.client[["label"]],padx=padx,pady=pady,columnspan=3,sticky="we")
      tkgrid(tbl.riskFreeConstr[["frame"]],padx=padx,pady=pady)
      tkgrid(lf.riskFree[["labelFrame"]],padx=padx,pady=pady,column=0,row=1,sticky="n")

      tkgrid(tbl.currencyConstr[["frame"]],padx=padx,pady=pady)
      tkgrid(lf.currency[["labelFrame"]],padx=padx,pady=pady,column=0,row=2,sticky="ns")
    
      tkgrid(tbl.sectorConstr[["frame"]],padx=padx,pady=pady)
      tkgrid(lf.sector[["labelFrame"]],padx=padx,pady=pady,column=0,row=3,sticky="s")

      tkgrid(tbl.currencySectorConstr[["frame"]],padx=padx,pady=pady)
      tkgrid(lf.currencySector[["labelFrame"]],padx=padx,pady=pady,column=1,row=1,rowspan=3,sticky="ns")
        
      tkgrid(tbl.multipleConstr[["frame"]],padx=padx,pady=pady,sticky="n")
      tkgrid(lf.multipleConstr[["labelFrame"]],padx=padx,pady=pady,column=2,row=1,rowspan=3,sticky="n")
      tkgrid(f.multipleConstrButtons,padx=padx,pady=pady)
    
      tkgrid(f.mainButtons,padx=padx,pady=pady,columnspan=3,sticky="we")
        
      ## focus on the constraints windows
      tkgrab(window.constraints)
      tkfocus(window.constraints)

    }   
  ## ###########################################################################    
  ## end function used for the constraints setup   
  ## ###########################################################################

  ## create top window
  topWindow <- tktoplevel()
  tktitle(topWindow) <- "Mean-Variance optimization"

  ## create the first labelframe containing the Data sources
  lf.dataSources <- create_labelFrame(parent=topWindow,text="Data sources")
  ## create the two buttons
  b.updateDatasources <- create_button(parent=lf.dataSources[["labelFrame"]],text="Update",command=update.data.sourcesLocal)
  b.showDataSources <- create_button(parent=lf.dataSources[["labelFrame"]],text="Show",command=pippo)

  ## create the select universe labelframe
  lf.universeSelection <- create_labelFrame(parent=topWindow,text="Select universe")
  b.selectEquities <- create_button(parent=lf.universeSelection[["labelFrame"]],text="Equities",command=select.equity.universeLocal)
  b.selectRiskFree <- create_button(parent=lf.universeSelection[["labelFrame"]],text="Risk free",command=select.oneYearInterest.universeLocal)

  ## create the labelframe of the client
  lf.client <- create_labelFrame(parent=topWindow,text="Client")

  ## create a subframe containing the label, name, Add button e Remove button
  f.subclient <- tkframe(lf.client[["labelFrame"]])
  l.select <- create_label(parent=f.subclient,value="Select client")
  cb.client <- create_combo(parent=f.subclient,values=paste(env[["df.clients"]][,1],env[["df.clients"]][,2],sep=" - "),
                            width=20)

  ## create the label VaR Limit
  l.varLimit <- create_label(parent=lf.client[["labelFrame"]],value="VAR limit:")
  e.varLimit <- create_entry(parent=lf.client[["labelFrame"]],value="0.10",width="10")

  ## create the label VaR confidence
  l.varConfidenceLevel <- create_label(parent=lf.client[["labelFrame"]],value="VAR confidence level:")
  e.varConfidenceLevel <- create_entry(parent=lf.client[["labelFrame"]],value="0.05",width="10")

  ## create the add and remove button
  b.addClient <- create_button(parent=lf.client[["labelFrame"]],text="Add",command=add.client)
  b.removeClient <- create_button(parent=lf.client[["labelFrame"]],text="Remove",command=remove.client)

  ## create the buttons select - universe, constraints and Optimize portfolio, respectively
  b.selectClientUniverse <- create_button(parent=lf.client[["labelFrame"]],text="Select universe",
                                          command=selectClientUniverseLocal)
  b.selectConstraints <- create_button(parent=lf.client[["labelFrame"]],text="Select constraints",
                                       command=create.constraints.window)
  b.optimizePortfolio <- create_button(parent=lf.client[["labelFrame"]],text="Optimize portfolio",
                                       command=optimizePortfolioLocal)

  ## create the navigator label
  l.navigator <- create_label(parent=topWindow,value="./")

  ## update the selected values of the widgets
  if (updateSelection)
    {
      cb.client[["set.selection"]](paste(env[["selectedClient"]],env[["selectedCurrency"]],sep=" - "))
      e.varLimit[["set.value"]](env[["selectedVarLimit"]])
      e.varConfidenceLevel[["set.value"]](env[["selectedVarConfidenceLevel"]])
    }

  ## INSERT OF THE CREATED WIDGETS
  ## insert objects in the labelframe dataSources
  tkgrid(b.updateDatasources[["button"]],padx=padx,pady=pady)
  tkgrid(b.showDataSources[["button"]],padx=padx,pady=pady)

  ## insert objects in the labelframe universeSelection
  tkgrid(b.selectEquities[["button"]],padx=padx,pady=pady)
  tkgrid(b.selectRiskFree[["button"]],padx=padx,pady=pady)

  ## insert objects in the frame x
  tkgrid(l.select[[1]],cb.client[[1]],b.addClient[[1]],b.removeClient[[1]],sticky="w",padx=padx,pady=pady)
  ## insert the frame x
  tkgrid(f.subclient,columnspan=3,sticky="w")

  ## insert the varLimt fields
  tkgrid(l.varLimit[["label"]],e.varLimit[["entry"]],sticky="w",padx=padx,pady=pady)

  ## insert the varConfidenceLevel fields
  tkgrid(l.varConfidenceLevel[["label"]],e.varConfidenceLevel[["entry"]],sticky="w",padx=padx,pady=pady)

  ## insert the buttons select - universe, constraints and Optimize portfolio, respectively
  tkgrid(b.selectClientUniverse[["button"]],b.selectConstraints[["button"]],b.optimizePortfolio[["button"]],padx=padx,pady=pady)

  ## insert the three labelframes
  tkgrid(lf.dataSources[[1]],lf.universeSelection[[1]],lf.client[[1]],sticky="n",padx=padx,pady=pady)

  ## insert the navigator label
  tkgrid(l.navigator[[1]],padx=padx,pady=pady,sticky="w")
      
  ## update the value in the global variable (env)
  ## updateSelectedClient <- function(env=env)
  ##  {
  ##    env[["selectedClientId"]] <- cb.client[["get.id"]]()
  ##    env[["selectedClient"]] <- df.clients[[env[["selectedClientId"]],1]]
  ##    env[["selectedCurrency"]] <- df.clients[[env[["selectedClientId"]],2]]
  ##  }
  ## update the value in the global variable (env)
  ## updateSelectedVarLimit <- function(env=env)
  ##  {
  ##    env[["selectedVarLimit"]] <- as.numeric(chop(e.varLimit[["get.value"]]()))/100
  ##  }
  ## update the value in the global variable (env)
  ## updateSelectedVarConfidenceLevel <- function(env=env)
  ##  {
  ##    env[["selectedVarConfidenceLevel"]] <-  as.numeric(chop(e.varConfidenceLevel[["get.value"]]()))/100
  ##  }
  ##tkbind(cb.client[["combo"]],"<FocusOut>",updateSelectedClient)
  ##tkbind(e.varLimit[["entry"]],"<FocusOut>",updateSelectedVarLimit)
  ##tkbind(e.varConfidenceLevel[["entry"]],"<FocusOut>",updateSelectedVarConfidenceLevel)

}

                                        ## program's start

                                        ## set the default windows options
padx=10
pady=5
## define the global variables

env <- topenv(new.env())

env[["selectedClientId"]] <- 0
env[["selectedClient"]] <- ""
env[["selectedCurrency"]] <- ""
env[["selectedVarLimit"]] <- 0.10
env[["selectedVarConfidenceLevel"]] <- 0.05
env[["optimizationCurrency"]] <-  ""

## open a connection to DBMarkowitz
odbcCon = setOdbcConnection("DBMarkowitz")

## get the list of clients
env[["df.clients"]] <- sql.get.table(odbcCon, "Elenco_clienti")
odbcClose(odbcCon) ## close the connection

start_window()
