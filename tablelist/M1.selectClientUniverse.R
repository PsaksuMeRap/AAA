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
      ## A_selezioneSqlType = c("nvarchar","nvarchar","nvarchar","nvarchar","bit") not required any more
      ## names(A_selezioneSqlType) <- c("Moneta","Branche","Ticker","Company","Desiderato") not required any more
      ## ----------------------------------------------------------------------------
      query <- "ORDER BY Moneta, Branche, Company"
      clientEquityUniverse <- tcl.get.table(odbcConnection=odbcCon,
                                            tableName="A_selezione_singolo_titolo",
                                            requiredFields=list(Currency="Moneta",Sector="Branche","Ticker",
                                              "Company",Required="Desiderato"),
                                            orderBy=query,updateDb=T,pkFieldNames=c("Ticker")                                              
                                            #orderBy=query,updateDb=T,pkFieldNames=c("Ticker"),  not required any more
                                            #odbcFieldTypes=A_selezioneSqlType                   not required any more
                                           )
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
  ## create the tabnotebook widget
  tbn.notebook <- create_tabnotebook(parent=window.SectorsCurrencies,label=c("Equities","Interest rates"),height=550,width=1000,
                                        tabpos="n",tabbackground="white",background="systemButtonFace")


  ## 1) construct the frame containing the client name and the currency
  f.clientCurrency <- tkframe(parent=window.SectorsCurrencies)
  
  ## 1.1) the name of the client and the currency
  l.client <- create_label(parent=f.clientCurrency,value=paste(selectedClient, selectedCurrency,sep=" - "))


  ## 2) construct the upper right frame (Equities selection)
  f.equities <- tkframe(tbn.notebook[["frame1"]])
  
  ## 2.1) construct the tablelist with the currency of the equity universe
  f.currencies <- tkframe(f.equities)
  if (deleteCurrencySectors=="yes")
    {
      selectedCurrencies <- NULL
    }
  else
    {
      selectedCurrencies <- unique(df.selectedCurrencySectors[,"Currency"])
    }
  slbx.currencies <- create_selectionListBox(parent=f.currencies,title="Currency",
                                             values1=df.equityCurrencies[,"Currency"],values2=selectedCurrencies,mode="multiple",
                                             width=12,height=11,grabWindow=window.SectorsCurrencies)

  ## 2.2) construct the button "Show sectors"
  b.showSectors <- create_button(parent=f.currencies,text="Show sectors",command=showSectors)

  ## 2.3) construct the tablelist with the sectors
  f.sectors <- tkframe(f.equities)
  if (deleteCurrencySectors=="yes")
    {
      slbx.sectors <-
        create_selectionListBox(parent=f.sectors,title="Sector",
                                values1=NULL,values2=NULL,mode="multiple",
                                width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  else
    {
      selectedSectors <- unique(df.selectedCurrencySectors[,"Sector"])
      slbx.sectors <-
        create_selectionListBox(parent=f.sectors,title="Sector",
                                values1=df.availableSectors[,"Sector"],values2=selectedSectors,
                                mode="multiple",width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }


  ## 2.4) construct the button "Show <currency,sectors>"
  b.showCurrencySectors <- create_button(parent=f.sectors,text="Show <currency,sectors>",command=showCurrencySectors)

  ## 2.5) construct the tablelist with the currency,sector pairs
  if (deleteCurrencySectors=="yes")
    {
      #df1 <- data.frame(Currency=NA,Sector=NA)[-1,]
      df1 <- data.frame(Currency=character(0),Sector=character(0))
      df2 <- df1
    }
  else
    {
      df1 <- df.availableCurrencySectors
      df2 <- df.selectedCurrencySectors
    }

  sltl.currencySectors <-
    create_selectionTablelist(parent=f.equities,dataFrame1=df1,
                              title="Currency and sector",dataFrame2=df2,width=30,height=15,
                              alignCols=rep("left",2),alignColsLabel=rep("center",2),withScrollBarY=TRUE,
                              multiSortColumn=T,selectmode="multiple",rTypes=rep("character",2),
                              grabWindow=window.SectorsCurrencies
                              )
  rm(df1,df2)

  ## 2.6) construct the frame containing the equities selection and Cancel buttons
  f.selectionCancel <- tkframe(tbn.notebook[["frame1"]])
  ## 2.7) construct the button "Equities selection"
  b.equitiesSelection <- create_button(parent=f.selectionCancel,text="Select equities",command=equitiesSelection)

  ## 2.8) construct the button "Cancel"
  onCancel <- function() {tkdestroy(window.SectorsCurrencies); start_window(updateSelection=TRUE)}
  b.cancel <- create_button(parent=f.selectionCancel,text="Cancel",command=onCancel)

  ## 3.1) construct the lower left subframe (Risk free interest rate and other rates)
  f.left.low <- tkframe(tbn.notebook[["frame2"]])

  ## construct the tablelist with the currencies of the interest rates universe
  ##tl.interestRates <- create_odbcTablelist(
  ##                       parent=f.left.low,data=tcl.df.riskFreeRates,width=20,height=8,
  ##                       alignCols="left",alignColsLabel="center",withScrollBarY=TRUE,
  ##                       sortColumn=T,selectmode="multiple",updateDb=F
  ##                       )
  if (deleteRiskFree=="yes")
    {
      sltl.interestRates <-
        create_selectionListBox(parent=f.left.low,title="Interest rates",
                                values1=df.riskFreeRates[,"Currency"],values2=NULL,mode="multiple",
                                width=18,height=11,grabWindow=window.SectorsCurrencies
                                )
    }
  else
    {
      sltl.interestRates <-
        create_selectionListBox(parent=f.left.low,title="Interest rates",
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
  b.saveCancel <- create_saveCancelButton(parent=f.left.low,onSave=onSave,onCancel=onCancel)


  ## 3.3) construct the navigator label
  l.navigator <- create_label(parent=window.SectorsCurrencies,value="./Client's equities and risk free interest rates selection/")


  ## fill the left frame
  tkgrid.configure(f.clientCurrency)
  ## place the client name
  tkgrid(tklabel(parent=f.clientCurrency,text="Client:"),l.client[["label"]],
         padx=padx,pady=pady,sticky="n")

  ## place the upper subframe
  tkgrid(tbn.notebook[["tabnotebook"]])
  
  tkgrid(f.equities,padx=padx,pady=pady)
  ## place the currencies, sectors and currencySectors tablelist
  tkgrid(slbx.currencies[["frame"]])
  tkgrid(b.showSectors[["button"]])

  
  tkgrid(slbx.sectors[["frame"]])
  tkgrid(b.showCurrencySectors[["button"]])
  
  tkgrid(f.currencies,f.sectors,sltl.currencySectors[["frame"]], padx=padx,
         pady=pady,sticky="n")
  tkgrid(b.equitiesSelection[["button"]],b.cancel[["button"]],padx=padx)
  tkgrid(f.selectionCancel,pady=40)

  
  ## disable some tablelist and buttons
  ##tkconfigure(b.showCurrencySectors[["button"]],state="disabled")

  ## place the lower subframe
  tkgrid(f.left.low,padx=padx,pady=pady)
  ## place the interest rate tablelist
  tkgrid(sltl.interestRates[["frame"]],padx=padx,pady=pady)
  ## place the saveCancel button
  tkgrid(b.saveCancel)

  ## place the navigator label
  tkgrid(l.navigator[["label"]],padx=padx,pady=pady,sticky="nw")
  tbn.notebook[["select.tab"]]()
  return()
}