rm(list=ls(all=TRUE))
library("ayrton")
library("tkutilities")
library("odbcutilities")
source("C:/R/R_tablelist/MarkowitzSqlProcedures.R")

## temporary function (to delete later)
notImplemented <- function() tkmessageBox(message="Feature not implemented yet",
                                          icon="error",type="ok")



## create the necessary functions
source("C:/R/R_tablelist/M1.updateDataSources.R",local=TRUE)

source("C:/R/R_tablelist/M1.setupEquityCovariances.R",local=TRUE)

## select risk freee universe function
source("C:/R/R_tablelist/M1.selectOneYearRatesUniverse.R",local=TRUE)

## select equity universe function
source("C:/R/R_tablelist/M1.selectEquityUniverse.R",local=TRUE)

## this is the function associated with the button "Select universe" in the client labelframe
source("C:/R/R_tablelist/M1.selectClientUniverse.R",local=TRUE)

## this function opens the windows leading to the optimization procedure
source("C:/R/R_tablelist/M1.optimizePortfolio.R",local=TRUE)

## this function optimize the portfolio
source("C:/R/R_tablelist/M2.optimization.R")

## this function opens the windows used to view the optimization's result and
## to accept or reject them 
source("C:/R/R_tablelist/M2.optimizationAcceptance.R")

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
  
  updateDataSourcesLocal <- function()
    {
      updateDataSources(topWindow)
    }
  
  selectEquityUniverseLocal <- function()
    {
      selectEquityUniverse(topWindow)
    }
  
  selectOneYearRatesUniverseLocal <- function()
    {
      selectOneYearRatesUniverse(topWindow)
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
          assign("df.clients",get.table(odbcCon, "Elenco_clienti"),envir=env)
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
              assign("df.clients",get.table(odbcCon, "Elenco_clienti"),envir=env)
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
  

  ## create the function used for the constraints setup
  source("C:/R/R_tablelist/M1.constraintSetup.R",local=TRUE)
  
  ## create top window
  topWindow <- tktoplevel()
  tktitle(topWindow) <- "Mean-Variance optimization"

  ## create the first labelframe containing the Data sources
  lf.dataSources <- create_labelFrame(parent=topWindow,text="Data sources")
  ## create the two buttons
  b.updateDatasources <- create_button(parent=lf.dataSources[["labelFrame"]],text="Update",command=updateDataSourcesLocal)
  b.showDataSources <- create_button(parent=lf.dataSources[["labelFrame"]],text="Show",command=notImplemented)

  ## create the select universe labelframe
  lf.universeSelection <- create_labelFrame(parent=topWindow,text="Select universe")
  b.selectEquities <- create_button(parent=lf.universeSelection[["labelFrame"]],text="Equities",command=selectEquityUniverseLocal)
  b.selectRiskFree <- create_button(parent=lf.universeSelection[["labelFrame"]],text="Risk free",command=selectOneYearRatesUniverseLocal)

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
                                       command=create.constraintsWindow)
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
env[["df.clients"]] <- get.table(odbcCon, "Elenco_clienti")
## close the connection
odbcClose(odbcCon)
start_window()
