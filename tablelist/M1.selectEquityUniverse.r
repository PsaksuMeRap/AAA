#### SELECT EQUITY UNIVERSE FUNCTION
selectEquityUniverse <- function(parent)
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
                 "CREATE TABLE #DBUniverso_per_ottimizzazione_tmp", 
                 "(ID_strumento INT NOT NULL,ID_AAA INT NOT NULL,",
                 "Moneta nvarchar(3) NOT NULL,Branche nvarchar(50) NOT NULL,",
                 "Ticker varchar(20),Company nvarchar(100),Desiderato bit)"
                 ##,",CONSTRAINT PK_DBUniverso_per_ottimizzazione_tmp PRIMARY KEY",
                 ##"(ID_strumento,ID_AAA,Moneta,Branche))"
                 )
  ## create the vector of odbc types because MS SQL does not return the types of
  ## temporary tables "#" tables.
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
                       errorText="Populate the #DBUniverso_per_ottimizzazione_tmp")
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
                                      tableName="#DBUniverso_per_ottimizzazione_tmp", 
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
