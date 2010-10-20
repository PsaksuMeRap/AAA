## this function opens the windows leading to the optimization procedure
optimizePortfolio <- function(parent)
{
  ## verify the validity of the selected client
  selectedClientId <- env[["selectedClientId"]]
  selectedClient <- env[["selectedClient"]]
  selectedCurrency <- env[["selectedCurrency"]]
  selectedVarConfidenceLevel <- env[["selectedVarConfidenceLevel"]]
  
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

  ## destroy the feedbackWindow
  tkgrab.release(top)
  tkdestroy(top)
  
  ## optimize the portfolio
  optimResults <- optimization.markowitz(client=selectedClient,referenceCurrency=selectedCurrency,
                         varConfidenceLevel=selectedVarConfidenceLevel,odbcCon)
  close(odbcCon)

  ## view the results
  optimizationAcceptance(optimResults=optimResults)
  
  return()
}
