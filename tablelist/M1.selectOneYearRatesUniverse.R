## SELECT RISK FREE UNIVERSE FUNCTION
selectOneYearRatesUniverse <- function(parent)
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
