setupEquityCovariances <- function(refCurrency="CHF", feedback, odbcCon)
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

  ## setupEquityCovariances q1
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO DBUniverso_per_ottimizzazione_tmp ",
                 "(ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company) ",
                 "SELECT ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company ",
                 "FROM DBUniverso_per_ottimizzazione"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q1")
  ## -------------------------------------------------------------------------

  ## setupEquityCovariances q2
  ## -------------------------------------------------------------------------
  query <- paste(
                 "INSERT INTO DBUniverso_per_ottimizzazione_tmp ",
                 "(ID_strumento, ID_AAA, Moneta, Branche, Ticker, Company) ",
                 "SELECT 2 AS ID_strumento, ID AS ID_AAA, Moneta as Moneta, 'Moneta' as Branche, Moneta as Ticker, NULL AS Company FROM Copia_DBMonete ",
                 "WHERE NotUniversoVAR = 0 AND Moneta <> 'CHF'"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q2")
  ## -------------------------------------------------------------------------

  ## Crea la matrice delle varianze covarianze i cui cambi sono rispetto alla moneta CHF
  ## -------------------------------------------------------------------------
  drop.table(table="G_covarianze_azionarie_CHF",channel=odbcCon,
             errorText="DROP TABLE G_covarianze_azionarie_CHF")
  ## -------------------------------------------------------------------------

  ## setupEquityCovariances q3
  ## -------------------------------------------------------------------------
  query <- paste(
                 "SELECT A.Ticker1, A.Ticker2, A.Covarianza, B.Moneta AS Moneta1, C.Moneta AS Moneta2 ",
                 "INTO G_covarianze_azionarie_CHF ",
                 "FROM Copia_Covarianza_universo A INNER JOIN DBUniverso_per_ottimizzazione_tmp B ON A.Ticker1 = B.Ticker ",
                 "INNER JOIN DBUniverso_per_ottimizzazione_tmp C ON A.Ticker2 = C.Ticker"
                 )
  result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q3")
  ## -------------------------------------------------------------------------

  ## inizia un loop e crea tutte le matrici di varianza covarianza i cui cambi sono rispetto alla moneta di turno
  ## setupEquityCovariances q4
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

          ## setupEquityCovariances q4_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT Ticker1, Ticker2, Covarianza, Moneta1, Moneta2 ",
                         "INTO G_covarianze_azionarie_" %+% refCurrency,
                         "FROM G_covarianze_azionarie_CHF"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q4_loop")
          ## -------------------------------------------------------------------------

          ## setupEquityCovariances q5_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT Covarianza FROM G_covarianze_azionarie_" %+% refCurrency,
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'",
                         "AND Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sql.get.table(odbcCon, query=query,as.is=FALSE)
          variance.refCurrency = result[1,1]
          ## -------------------------------------------------------------------------

          ## setupEquityCovariances q6_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Covarianza = -1 * Covarianza ",
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q6_loop")
          ## -------------------------------------------------------------------------

          ## setupEquityCovariances q7_loop
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Covarianza = -1 * Covarianza ",
                         "WHERE Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,errorText="setupEquityCovariances q7_loop")
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

                  ## setupEquityCovariances q2.1_loop
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
                                       errorText="setupEquityCovariances q2.1_loop")
                  ## -------------------------------------------------------------------------

                  ## fai una copia della tabella G_covarianze_azionarie_" & refCurrency
                  ## da usare nella seconda query di join
                  ## setupEquityCovariances q2.2_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT * ",
                                 "INTO #DBTmp2 ", #
                                 "FROM G_covarianze_azionarie_" %+% refCurrency
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="setupEquityCovariances q2.2_loop")
                  ## -------------------------------------------------------------------------

                  ## -------------------------------------------------------------------------
                  drop.table(table="G_covarianze_azionarie_" %+% refCurrency,channel=odbcCon,
                             errorText="DELETE FROM G_covarianze_azionarie_" %+% refCurrency)
                  ## -------------------------------------------------------------------------

                  ## setupEquityCovariances q2.3_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT Ticker1, Ticker2, CASE WHEN Ticker2 LIKE '" %+% currency %+% "'",
                                 "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END",
                                 "AS Covarianza, Moneta1, Moneta2 ",
                                 "INTO G_covarianze_azionarie_" %+% refCurrency,
                                 "FROM #DBTmp1" #
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="setupEquityCovariances q2.3_loop")
                  ## -------------------------------------------------------------------------

                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp1",channel=odbcCon,errorText="DROP TABLE #DBTmp1") #
                  ## -------------------------------------------------------------------------

                  ## setupEquityCovariances q2.4_loop
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
                                       errorText="setupEquityCovariances q2.4_loop")
                  ## -------------------------------------------------------------------------

                  ## -------------------------------------------------------------------------
                  drop.table(table="G_covarianze_azionarie_" %+% refCurrency,
                             channel=odbcCon,errorText="DROP TABLE G_covarianze_azionarie_" %+% refCurrency)
                  ## -------------------------------------------------------------------------

                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp2",channel=odbcCon,errorText="DROP TABLE #DBTmp2") #
                  ## -------------------------------------------------------------------------

                  ## setupEquityCovariances q2.5_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "SELECT Ticker1, Ticker2, CASE WHEN Ticker1 LIKE '" %+% currency %+% "'",
                                 "THEN Covarianza1 + Covarianza2 ELSE Covarianza1 END AS Covarianza, Moneta1, Moneta2 ",
                                 "INTO G_covarianze_azionarie_" %+% refCurrency,
                                 "FROM #DBTmp" #
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="setupEquityCovariances q2.5_loop")
                  ## -------------------------------------------------------------------------

                  ## -------------------------------------------------------------------------
                  drop.table(table="#DBTmp",channel=odbcCon,errorText="DROP TABLE #DBTmp") #
                  ## -------------------------------------------------------------------------

                  ## setupEquityCovariances q2.6_loop
                  ## -------------------------------------------------------------------------
                  query <- paste(
                                 "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                                 "SET Covarianza = Covarianza + " %+% variance.refCurrency,
                                 "WHERE Ticker1 LIKE '" %+% currency %+% "'",
                                 "AND Ticker2 LIKE '" %+% currency %+% "'"
                                 )
                  result <- sqlCommand(channel=odbcCon,query=query,
                                       errorText="setupEquityCovariances q2.6_loop")
                  ## -------------------------------------------------------------------------

                  count.portNotChfCurr <- count.portNotChfCurr + 1
                }
              rm(nb.portfNotChfCurr,portfNotChfCurr,currency,count.portNotChfCurr,
                 delta.portNotChfCurr)
            }

          ## Modifica il nome del fattore di rischio da quello della moneta di riferimento a CHF.
          ## setupEquityCovariances q3.1
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Ticker1 = 'CHF', Moneta1='CHF' ",
                         "WHERE Ticker1 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="setupEquityCovariances q3.1")
          ## -------------------------------------------------------------------------

          ## setupEquityCovariances q3.2
          ## -------------------------------------------------------------------------
          query <- paste(
                         "UPDATE G_covarianze_azionarie_" %+% refCurrency,
                         "SET Ticker2 = 'CHF', Moneta2='CHF' ",
                         "WHERE Ticker2 LIKE '" %+% refCurrency %+% "'"
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="setupEquityCovariances q3.2")
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

          ## setupEquityCovariances q4.1
          ## -------------------------------------------------------------------------
          query <- paste(
                         "SELECT * INTO G_covarianze_azionarie_moneta_" %+% refCurrency,
                         "FROM G_covarianze_azionarie_" %+% refCurrency
                         )
          result <- sqlCommand(channel=odbcCon,query=query,
                               errorText="setupEquityCovariances q4.1")
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
