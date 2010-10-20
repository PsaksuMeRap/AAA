updateDataSources <- function(topWindow)
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

  if (identical(update.cov,"yes")) setupEquityCovariances(refCurrency="CHF",feedback=feedbackWin,odbcCon=odbcCon)

  feedbackWin[["status"]](100)
  tkgrab.release(top)
  tkdestroy(top)

  odbcClose(odbcCon)
  tkmessageBox(message="Update procedure successful.",icon="info",type="ok",parent=topWindow)

}