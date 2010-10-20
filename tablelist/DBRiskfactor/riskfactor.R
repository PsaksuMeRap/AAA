rm(list=ls(all=TRUE))
library("ayrton")
library("tkutilities")
library("odbcutilities")


# open odbc connection
odbcCon = setOdbcConnection("risk")

## set the default windows options
padx=10
pady=5

## define the global variables
# temporary function (to be deleted)
pippo <- function() print("pippo")

###Ricordarsi di riportare i cambiamenti alle funzioni
###get.row e get.selection di create_tablelist anche a
###create_odbcTablelist!!  Ad oggi 29/01/07 non ancora fatto

# create_newTimeSeriesWidget(parent=topWindow)

# select.fieldsFromTable(myConnection=odbcCon,selectedDatabase=cb.database[["get.selection"]](),
# selectedTable=cb.table[["get.selection"]]())


source("C:/R/R_tablelist/DBtimeseries/widgets.R")

onNewTimeseries <- function()
  {
    ok <- create_newRiskfactorWidget(myConnection=odbcCon)
    if (ok==0)
      {
        ## get the new values
        riskfactorWidget[["update.data"]]()
        riskfactorWidget[["refresh"]]()
      }
    tkfocus(topWindow)
    tkgrab(topWindow)
    return()
  }
onRemoveTimeseries <- function()
  {
    ## select the list of riskfactor to be removed

    selection <- timeSeriesWidget[["get.selection"]]()

    ## if selection is not empty get the list of <database>,<table> pairs and
    ## for each of them extract the list of fields in the table. Let the user specify
    ## which of them uniquely determine the riskfactor in the table
    if (nrow(selection) == 0) return()

    ## get the unique list of pairs <database>,<table> from which remove the series
    uniqueDbTable <- unique(selection[,c("Database","Table")])

    availableIdentifiers <- c("Ticker","NumeroValore","ID","ID_Strumento")
    for (i in nrow(uniqueDbTable))
      {
        database <- uniqueDbTable[i,"Database"]
        tableName <- uniqueDbTable[1,"Table"]

        ## remove the riskfactor data in the table
        identifiers <- tableUniqueIdentifiers(myConnection=odbcCon,database=database,
                           tableName=tableName)

        if (length(identifiers)==0)
          {
            tkmessageBox(message="No identifier selected, series not removed",type="ok",icon="info",parent=topWindow)
            next
          }

        if (!all(is.element(identifiers,availableIdentifiers)))
          {
            tkmessageBox(message=paste("< ",tableName,"> The selected identifiers are not available: data must be removed manually!",sep=""),
                         type="ok",icon="info",parent=topWindow)
            next
          }

        ## determine the entries with the actual <database>,<table>
        isCurrentDatabase <- selection[,"Database"] == database
        isCurrentTable <- selection[,"Table"] == tableName
        desired <- isCurrentDatabase & isCurrentTable

        if (any(desired))
          {
            ## get the values associated to the corresponding identifiers
            subdata <- selection[desired,]
            identifiersData <- subdata[identifiers]

            ## construct the queries
            query = paste("DELETE FROM [",tableName,"] WHERE ",sep="")
            if (is.element(identifiers[1],c("Ticker","NumeroValore"))) tmp = paste(identifiers[1],"='",identifiersData[,identifiers[1]],"'",sep="")
            if (is.element(identifiers[1],c("ID","ID_Strumento"))) tmp = paste(identifiers[1],"=",identifiersData[,identifiers[1]],sep="")
            if (length(identifiers)>1)
              {
                for (ident in identifiers[-1])
                  {
                    if (is.element(ident,c("Ticker","NumeroValore"))) tmp = paste(tmp," AND ",ident,"='",identifiersData[,ident],"'",sep="")
                    if (is.element(ident,c("ID","ID_Strumento"))) tmp = paste(tmp," AND ",ident,"=",identifiersData[,ident],sep="")
                  }
              }
           query = paste(query,tmp,collapse=";")

           ## change database and execute the query
           connectionDatabase <- sqlSetWorkingDatabase(myConnection=odbcCon,newDatabase=database)
           sqlCommand(channel=odbcCon,query=query)

           ## select the "risk" database
           sqlSetWorkingDatabase(myConnection=odbcCon,newDatabase="risk")

           ## remove from DBTimeseries
           query <- paste("DELETE FROM DBTimeseries WHERE ID_Category=",subdata[,"ID_Category"]," AND ID=",subdata[,"ID"],sep="",collapse=" ; ")
           sqlCommand(channel=odbcCon,query=query)

           ## restore original database
           sqlSetWorkingDatabase(myConnection=odbcCon)

           ## refresh the tablelist data and the tablelist widget
           timeSeriesWidget[["update.data"]]()
           timeSeriesWidget[["refresh"]]()
          }
      }

    ## remove the entry in DBTimeseries
  }



                                        # program's start
topWindow <- tktoplevel()
tktitle(topWindow) <- "Riskfactor"



## create the timeSeriesWidget

riskfactorWidget <- create_riskfactorWidget(topWindow=topWindow,tblWidth=100,odbcCon=odbcCon)
tkgrid(riskfactorWidget[["lf.riskfactor"]][["labelFrame"]],padx=padx,pady=pady,rowspan=2,sticky="wn")


## create the actions buttons
lf.riskfactorButtons <- create_labelFrame(parent=topWindow,text="Riskfactors")
b.new <- create_button(parent=lf.riskfactorButtons[["labelFrame"]],text="New",width =15,command = onNewTimeseries)
b.remove <- create_button(parent=lf.riskfactorButtons[["labelFrame"]],text="Remove",width =15,command = riskfactorWidget[["remove.selection"]])
b.analyse <- create_button(parent=lf.riskfactorButtons[["labelFrame"]],text="Analyse",width =15,command = pippo)


tkgrid(lf.riskfactorButtons[["labelFrame"]],padx=padx,pady=pady,row=0,column=1)

tkgrid(b.new[["button"]],padx=padx,pady=pady)
tkgrid(b.move[["button"]],padx=padx,pady=pady)
tkgrid(b.copy[["button"]],padx=padx,pady=pady)
tkgrid(b.remove[["button"]],padx=padx,pady=pady)
tkgrid(b.analyse[["button"]],padx=padx,pady=pady)

create_tables <- function()
  {
    ## show the structure of the dataframe containing the table structure 
    ## data.frame(character(0),character(0),character(0),logical(0),logical(0),stringsAsFactors=FALSE)
    df.names <- c("Column Name","Data Type","Length","Allow Nulls","Primary Key")

    ## create the table DBCategory_Riskfactor
    ColNames = c("ID", "Category") 
    DataType = c("int","varchar")
    Length = c(4,50)
    AllowNulls = c(FALSE,FALSE)
    PrimaryKey = c(TRUE,FALSE)
    
    df.tmp <- data.frame(ColNames,DataType,Length,AllowNulls,PrimaryKey,stringsAsFactors=FALSE)    
    colnames(df.tmp) <- df.names

    sqlCreateTable(myConnection=odbcCon,database="risk",newTable="DBCategory_Riskfactor",
                   df.tableStructure=df.tmp,exec=TRUE)

    
    ## create the table DBCategory_Timeseries
    ColNames = c("ID", "Category") 
    DataType = c("int","varchar")
    Length = c(4,50)
    AllowNulls = c(FALSE,FALSE)
    PrimaryKey = c(TRUE,FALSE)
    
    df.tmp <- data.frame(ColNames,DataType,Length,AllowNulls,PrimaryKey,stringsAsFactors=FALSE)    
    colnames(df.tmp) <- df.names

    sqlCreateTable(myConnection=odbcCon,database="risk",newTable="DBCategory_Timeseries",
                   df.tableStructure=df.tmp,exec=TRUE)
                   
    ## create the table DBMap_Riskfactors_vs_Timeseries
    ColNames = c("ID_Riskfactor", "ID_Moneta_Riskfactor","[Database]","[Table]",
                 "ID_Currency","ID_Strumento","ID","Ticker","NumeroValore") 
    DataType = c("int","int","varchar","varchar","int","int","int","nvarchar","varchar")
    Length = c(4,4,63,63,4,4,4,255,50)
    AllowNulls = c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE)
    PrimaryKey = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
    
    df.tmp <- data.frame(ColNames,DataType,Length,AllowNulls,PrimaryKey,stringsAsFactors=FALSE)    
    colnames(df.tmp) <- df.names

    sqlCreateTable(myConnection=odbcCon,database="risk",newTable="DBMap_Riskfactors_vs_Timeseries",
                   df.tableStructure=df.tmp,exec=TRUE)    
    
    
    ## create the table DBMap_Strumenti_vs_Riskfactors
    ## create the table DBRf_Construction_Data_1
    ## create the table DBRf_Construction_Data_2
    ## create the table DBRf_Construction_method
    ## create the table DBRiskfactor
    ## create the table DBRiskmethod
    ## create the table DBTimeseries

  }



