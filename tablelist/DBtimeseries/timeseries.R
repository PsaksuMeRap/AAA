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
    ok <- create_newTimeSeriesWidget(myConnection=odbcCon)
    if (ok==0) 
      {
        ## get the new values
        timeSeriesWidget[["update.data"]]()
        timeSeriesWidget[["refresh"]]()
      }
    tkfocus(topWindow)
    tkgrab(topWindow)
    return()
  }
onRemoveTimeseries <- function()
  {
    ## select the list of timeseries to be removed 

    selection <- timeSeriesWidget[["get.selection"]]()
   
    ## if selection is not empty get the list of <database>,<table> pairs and
    ## for each of them extract the list of fields in the table. Let the user specify
    ## which of them uniquely determine the timeseries in the table
    if (nrow(selection) == 0) return()    
    
    ## get the unique list of pairs <database>,<table> from which remove the series
    uniqueDbTable <- unique(selection[,c("Database","Table")])

    availableIdentifiers <- c("Ticker","NumeroValore","ID","ID_Strumento")
    for (i in nrow(uniqueDbTable))
      {
        database <- uniqueDbTable[i,"Database"]
        tableName <- uniqueDbTable[1,"Table"]
        
        ## remove the timeseries data in the table
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
  
onCopy <- function()
  {
    ## get the selected list
    df.selection <- timeSeriesWidget[["get.selection"]]() 
    print(df.selection)
  }


                                        # program's start
topWindow <- tktoplevel() 
tktitle(topWindow) <- "Timeseries"



## create the timeSeriesWidget
timeSeriesWidget <- create_timeSeriesWidget(topWindow=topWindow,tblWidth=60,odbcCon=odbcCon)
tkgrid(timeSeriesWidget[["lf.timeseries"]][["labelFrame"]],padx=padx,pady=pady,rowspan=2,sticky="wn")



## create the actions buttons
lf.timeseriesButtons <- create_labelFrame(parent=topWindow,text="Timeseries")
b.new <- create_button(parent=lf.timeseriesButtons[["labelFrame"]],text="New",width =15,command = onNewTimeseries)
b.move <- create_button(parent=lf.timeseriesButtons[["labelFrame"]],text="Move",width =15,command = pippo)
b.copy <- create_button(parent=lf.timeseriesButtons[["labelFrame"]],text="Copy",width =15,command = onCopy)
b.remove <- create_button(parent=lf.timeseriesButtons[["labelFrame"]],text="Remove",width =15,command = onRemoveTimeseries)
b.analyse <- create_button(parent=lf.timeseriesButtons[["labelFrame"]],text="Analyse",width =15,command = pippo)

lf.data <- create_labelFrame(parent=topWindow,text="Data")
b.importData <- create_button(parent=lf.data[["labelFrame"]],text="Import",width =15,command = pippo)
b.exportData <- create_button(parent=lf.data[["labelFrame"]],text="Export",width =15,command = pippo)
b.removeData <- create_button(parent=lf.data[["labelFrame"]],text="Remove",width =15,command = pippo)

tkgrid(lf.timeseriesButtons[["labelFrame"]],padx=padx,pady=pady,row=0,column=1)
tkgrid(b.importData[["button"]],padx=padx,pady=pady)   
tkgrid(b.exportData[["button"]],padx=padx,pady=pady)
tkgrid(b.removeData[["button"]],padx=padx,pady=pady)

tkgrid(lf.data[["labelFrame"]],padx=padx,pady=pady,row=1,column=1) 
tkgrid(b.new[["button"]],padx=padx,pady=pady)
tkgrid(b.move[["button"]],padx=padx,pady=pady)
tkgrid(b.copy[["button"]],padx=padx,pady=pady)
tkgrid(b.remove[["button"]],padx=padx,pady=pady)
tkgrid(b.analyse[["button"]],padx=padx,pady=pady)





