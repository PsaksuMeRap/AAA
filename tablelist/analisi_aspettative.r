rm(list=ls(all=TRUE))
library("ayrton")
library("tkutilities")
library("odbcutilities")



## local variable
hscale <- 1.3 # graph
vscale <- 1.3 # graph
background <- "cornsilk" # graph
padx <- 10  ## windows options
pady <-  5  ## windows options

usernames <- c("Carlo","Claudio","Reto","Riccardo","Simona")
userIds <- 1:(length(usernames))
names(userIds) <- usernames


nb.pre.index=4  ## variable used to determine the number of sub indices; the first three column are 
                ## of the data frame containing the data are Country, Sector, Ticker, Ticker_AAA i.e. nb.pre.index = 4 


## remove some variables stored temporary in the .Globalenv, in realta' potremmo metterli
## in un environment appositamente createo per contenere le windows ...
if (exists("histogramWindow")) rm(histogramWindow)
if (exists("histogramNotebook")) rm("histogramNotebook",pos=1)
if (exists("boxPlotWindow")) rm(boxPlotWindow)
if (exists("boxplotNotebook")) rm("boxplotNotebook",pos=1)

## create the odbc connection
odbcCon <-  setOdbcConnection("analisi_aspettative")

## create the DB_dati_tabelle with the information on the tables
query <- paste("SELECT Nome_tabella, Nome_indice, Ordinamento_campo, Nome_campo,",
               "Ordinamento_intra_indice ",
               "FROM DB_dati_tabelle ",
               "WHERE Id_Provider = 1 AND Nome_indice IS NOT NULL ",
               "ORDER BY Nome_tabella, Ordinamento_campo,Ordinamento_intra_indice",
               sep=""
              ) 
DB_dati_tabelle <- sql.get.table(odbcCon,query=query,as.is=TRUE)
rm(query)

## utility functions (to be added to the utilities package
sameLength <- function(x,complete="left")
  {
    ## x is a vector of characters
    namesTmp <- names(x)
    
    nbChar <- nchar(x)
    maxNbChar <- max(nbChar)
    nbCharToAdd <- maxNbChar - nbChar
    emptyStrings <- apply(as.matrix(nbCharToAdd),MARGIN=1,
                    FUN=function(x){paste(rep(" ",x),collapse="")})
    if (complete == "left")
      {
        x <- paste(emptyStrings,x,sep="")
        if (!is.null(namesTmp)) names(x) <- namesTmp    
      }
    else
      {
        x <- paste(x,emptyStrings,sep="")
        if (!is.null(namesTmp)) names(x) <- namesTmp
      }
    return(x)
  }

## replace.NA
replace.NA <- function(dataframe,fields,values)
  {
    ## to be extended for vectors, meatrices ...
    values <- rep(values,length.out=length(fields))
    names(values) <- fields
    for (Name in fields)
      {
        dataframe[is.na(dataframe[,Name]),Name] <- values[Name] 
      }
    return(dataframe)
  }



## inizio codice
aggiorna_dipendenze <- function(odbcCon)
{
  # procedura che aggiorna le dipendenze della banca dati Analisi_aspettative
  # rimuovi tabella Copia_EquityDB
  result <- sqlCommand(channel=odbcCon, query="DROP TABLE Copia_EquityDB", errorText="Rimuovi tabella Copia_EquityDB")
  
  # ricrea tabella Copia_EquityDB
  query <- "SELECT ID, Ticker, Company, Branche, BrancheMarkowitz, ListaReturnSettoriale, InReturnSettoriale, [Specification Brance], Moneta, [Ranking settore], [Ranking moneta], NotUniversoVAR INTO Copia_EquityDB FROM [Prezzi storici azioni].dbo.EquityDB"
  result <- sqlCommand(channel=odbcCon, query, errorText="ricrea tabella Copia_EquityDB")
  
  tkmessageBox(message="Procedura d'aggiornamento delle dipendenze terminata", icon="info",type="ok")
}


verificaTickerAaa <- function(odbcCon)
{
  # crea una tabella temporanea da utilizzare nella maschera
  query = "CREATE TABLE #Lista_parziale_con_Null_da_importare " %+%
          "(Id int NOT NULL PRIMARY KEY, Provider nvarchar(50) NOT NULL, " %+%
          "Id_Provider int NOT NULL, Ticker nvarchar(255) NOT NULL, " %+%
          "Ticker_AAA nvarchar (255), Paese char(3))"

  result <- sqlCommand(channel=odbcCon, query, errorText="CREATE TABLE #Lista_parziale_con_Null_da_importare")

  # fai l'insert nella tabella appena creata
  query = "INSERT INTO #Lista_parziale_con_Null_da_importare " %+%
          "SELECT DISTINCT A.Id, B.Provider, A.Id_Provider, A.Ticker, A.Ticker_AAA AS [Ticker AAA], A.Paese " %+%
          "FROM Lista_da_importare A INNER JOIN DBExp_providers B ON A.Id_Provider = B.Id LEFT OUTER JOIN " %+%
          "Lista_da_importare_storico C ON A.Id_Provider = C.Id_Provider AND A.Ticker = C.Ticker " %+%
          "WHERE (C.Ticker_AAA='nullo') AND (A.Ticker_AAA='nullo') " %+%
          "ORDER BY B.Provider, A.Ticker, A.Ticker_AAA"

  result <- sqlCommand(channel=odbcCon, query, errorText="INSERT INTO #Lista_parziale_con_Null_da_importare")

}


form_verificaTickerAAA <- function(odbcConnection)
{
  dati <- sql.get.table(odbcConnection, tableName="#Lista_parziale_con_Null_da_importare",fieldNames=c("Ticker","Ticker_AAA","Paese"))
  rename.data.frame(dati, newColumnNames=c("Ticker","Ticker AAA","Paese"))
  topWindow <- tktoplevel()
  tbl <- create_tablelist(parent=topWindow,data=dati,width=120,height=15,withScrollBarX=TRUE,withScrollBarY=TRUE)
  tkgrid(tbl[["frame"]])
  closeConnection <- function() odbcClose(odbcConnection)
  b1 <- create_button(parent=topWindow,text="Close Connection",command=closeConnection)
  tkgrid(b1[["button"]])
}


summaryStatistics <- function(x)
  {  
    nbObs <- length(x)
    nbNA <- sum(is.na(x))

    valid =nbObs - nbNA
    stdv=sqrt(var(x,na.rm=TRUE))
    ss = summary(x)
    Range <- ss[["Max."]] - ss[["Min."]]
    
    if (nbNA==0)
      {
        tmp = c("Total obs."=nbObs,"NA's"=0,"Valid obs."=valid,Stdv=stdv,ss,"Range"=Range) 
      }
    else
      {
        tmp = c("Total obs."=nbObs,"Valid obs."=valid,Stdv=stdv,ss,"Range"=Range) 
              
      }
    ord <- c("Total obs.","NA's","Valid obs.","Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","Stdv","Range")
    tmp <- tmp[ord]
    names(tmp) <- ord
    tmp["3rd - 1st"] <- tmp["3rd Qu."] - tmp["1st Qu."]    
    return(list(data=x,statistics=tmp))
  }


writeSummaryStatistics <- function(x)
  {
    if (is.null(x)) return("")
    
    ## x is a list with a $statistics
    nbStatistics <- length(x$statistics)
    allnames <- names(x$statistics)  

    namesPart1 <- allnames[1:3]
    namesPart2 <- allnames[4:nbStatistics]
  
    a <- paste(round(x$statistics[namesPart1],digits=0),"  ")
    b <- format(round(x$statistics[namesPart2], digits = 2))
    statistics <- c(a,b);names(statistics) <- allnames
    statistics <- sameLength(statistics)
    firstColumn <- sameLength(allnames,complete="right");names(firstColumn) <- allnames
    
    string1 = "Observations:\n\n"
    
    string2 = paste(firstColumn[namesPart1],"\t",statistics[namesPart1]," (",
              format(round(x$statistics[namesPart1]/x$statistics[1]*100,digits=2)),"%)",sep="",collapse="\n")
    string2 = paste(string2,"\n\n","Statistics:\n",sep="\n")
    string3 = paste(firstColumn[namesPart2],statistics[namesPart2],sep="\t",collapse="\n")
    return(paste(string1,string2,string3,sep=""))
  }
  

start_window <- function()
{
  ## verify the validity of the selected user
  username <<- cb.user[["get.selection"]]()
  userId <<- userIds[username]
   
  if (!is.element(username,usernames))
    {
      tkmessageBox(message="Invalid user!",
                     icon="error",type="ok",parent=topWindow)
      return()
    }
  
  tkdestroy(topWindow)
  
  # create the new topWindow
  topWindow <- tktoplevel()  #used in get.riskFactors
  tktitle(topWindow) <- "Expectations analysis"


  onDateUpdate <- function()
    {
      cb.country[["reset"]]()
      cb.sector[["reset"]]()
      if (cb.index[["get.selection"]]() != "") updateNotebook()
    }  

  onTableUpdate <- function()
    {
      Table <- cb.table[["get.selection"]]()
      if (Table=="") return()
      
      desired <- DB_dati_tabelle[,"Nome_tabella"] == Table
      df.index <- unique(DB_dati_tabelle[desired,"Nome_indice"])

      if (length(df.index)>0)
        {
          cb.index[["modify.values"]](df.index)
          cb.index[["reset"]]() 
        }
      else
        {
          cb.index[["modify.values"]]()
          cb.index[["reset"]]()
        }
      if (exists("fullSelection")) rm(fullSelection,inherits=TRUE)
      if (exists("fullSelectionForReport")) rm(fullSelectionForReport,inherits=FALSE)
      if (exists("filteredSelection")) rm(filteredSelection,inherits=TRUE)
      if (exists("filteredSelectionForReport")) rm(filteredSelectionForReport,inherits=FALSE)
    } ## end function onTableUpdate
  

  onIndexUpdate <- function()
    {
      updateNotebook()
    }
  
  ## the filter block  
  onCountryUpdate <- function()
    {
      updateNotebook(updateData=FALSE)
    }

  onSectorUpdate <- function()
    {
      updateNotebook(updateData=FALSE)
    }
    
  onOnlyAaaUpdate <- function()
    {
      updateNotebook(updateData=FALSE)
    }
  
  ## the cross Analysis block
  refresh.cb.crossAnalysis <- function()
    {
        ## get the values for the corresponding entry field
        query = paste("SELECT DISTINCT Nome_constraint FROM DB_query_per_analisi",
                      "WHERE userId=",userId)
        df.Nome_constraint <- sql.get.table(odbcCon, query=query)              
        cb.crossAnalysis[["modify.values"]](values=df.Nome_constraint[,"Nome_constraint"])
    }
  onCrossAnalysis <- function () 
    {
      if (ckb.crossAnalysis[["get.value"]]()=="1")
        {
          tkconfigure(cb.crossAnalysis[["combo"]],state="normal")
          tkconfigure(b.crossAnalysisNew[["button"]],state="normal")
          tkconfigure(b.crossAnalysisModify[["button"]],state="normal")
          tkconfigure(b.crossAnalysisRemove[["button"]],state="normal")
          
          ## get the values for the corresponding entry field
          refresh.cb.crossAnalysis()
        }
      else
        {
          tkconfigure(cb.crossAnalysis[["combo"]],state="disabled")
          tkconfigure(b.crossAnalysisNew[["button"]],state="disabled")
          tkconfigure(b.crossAnalysisModify[["button"]],state="disabled")
          tkconfigure(b.crossAnalysisRemove[["button"]],state="disabled")        
        }
    }                        

  
  onCrossAnalysisNew <- function () 
    {
      crossAnalysis[["function"]]()  
    }


  onCrossAnalysisModify <- function ()
    {
      Nome_constraint <- cb.crossAnalysis[["get.selection"]]()
      if (Nome_constraint == "")
        {
          tkmessageBox (message = "Please select something",icon = "error",
                       type = "ok",parent=topWindow)
          return()
        }
      crossAnalysis[["function"]](updateAnalysis=TRUE)  
    }

    
  onCrossAnalysisRemove <- function ()
    {
      Nome_constraint <- cb.crossAnalysis[["get.selection"]]()
      if (Nome_constraint == "")
        {
          tkmessageBox (message = "Please select something",icon = "error",
                       type = "ok",parent=topWindow)
          return()
        }
      query <- paste("DELETE FROM DB_query_per_analisi WHERE userId = ",userId,
                     " AND Nome_constraint = '",Nome_constraint,"'",sep="")
      sqlCommand(channel=odbcCon, query=query)
      refresh.cb.crossAnalysis()
      cb.crossAnalysis[["reset"]]()
    }

  
  crossAnalysis <- list()
  crossAnalysis[["function"]] <- function(updateAnalysis=FALSE)
    {
      onDestroy <- function()
        {
          refresh.cb.crossAnalysis()
          tkfocus(topWindow)
        }
        
      topWindow <- tktoplevel()
      tktitle(topWindow) <- "Query construnction"
      tkbind(topWindow,"<Destroy>",onDestroy)
      tkfocus(topWindow)
      tkgrab(topWindow)
        
      onTableUpdate <- function()
        {
          Table <- cb.table[["get.selection"]]()
          if (Table=="") return()

          desired <- DB_dati_tabelle[,"Nome_tabella"] == Table
          df.index <- unique(DB_dati_tabelle[desired,"Nome_indice"])

          if (length(df.index)>0)
            {
              cb.index[["modify.values"]](df.index)
              cb.index[["reset"]]()
            }
          else
            {
              cb.index[["modify.values"]]()
              cb.index[["reset"]]()
            }
      
          ## reset the cb.field
          cb.field[["modify.values"]]()
          cb.field[["reset"]]()
        } ## end function onTableUpdate

      onIndexUpdate <- function()
        {
          desired <- DB_dati_tabelle[,"Nome_indice"] == cb.index[["get.selection"]]()
          fields <- unique(DB_dati_tabelle[desired,"Nome_campo"])
          if (length(fields)>0)
            {
              cb.field[["modify.values"]](fields)
              cb.field[["reset"]]()
            }
          else
            {
              cb.field[["modify.values"]]()
              cb.field[["reset"]]()
            }
        } ## end of onIndexUpdate
    
    
      onInsert <- function()
        {
          selection <- get.selection()
          ## verifiy that the selected index is not still in the tablelist
          selectedFields <- tbl.constr[["get.columns"]](i=3)
          if (is.element(selection["Field"],selectedFields[[1]]))
            {
              tkmessageBox (message = "This Field is still in the query.",icon = "error",
                       type = "ok",parent=topWindow)
              return()
            }
          if (check.selection(selection))
            {
              tbl.constr[["add.rows"]](newRows=selection)
            }
        
        }  ## end function onInsert

      get.selection <- function()
        {
          selection = c(cb.table[["get.selection"]](),cb.index[["get.selection"]](),
          cb.field[["get.selection"]](),cb.constrKindLow[["get.selection"]](),e.constrLow[["get.value"]](),
          cb.constrKindUp[["get.selection"]](),e.constrUp[["get.value"]]())
          names(selection) <- c("Table","Index","Field","Bound 1","Value 1","Bound 2","Value 2")
          return(selection)
        }
  
      check.selection <- function(selection)
        {
          required <- c("Table","Index","Field","Bound 1","Bound 2")
      
          if (any(selection[required]=="") | (selection["Value 1"]=="" & selection["Value 2"]==""))
            {
              tkmessageBox (message = "Some field is empty!",icon = "error",
                       type = "ok",parent=topWindow)
              return(0)
            }
          return(1)
        }
  
      onRemove <- function()
        {
          selection <- tbl.constr[["get.selection.rowNumber"]]()
          if (length(selection)>0) tbl.constr[["remove.rows"]](index=as.numeric(selection))
          #selection <- tbl.constr[["selection.clear"]]()
        }
  
    
      onSave <- function()
        {
          tbl.constr[["finishediting"]]()
          ## get the values in the tablelist widget and verify that is not empty
          dataFrame <<- tbl.constr[["get.table"]]()
     
          ## quit if dataFrame is empty
          if (dim(dataFrame)[1]==0) return()
    
    
          ## check the validity of the name
          constrName <- e.constrName[["get.value"]]()
          if (constrName == "")
            {
              tkmessageBox (message = "Invalid name!",icon = "error",
                       type = "ok",parent=topWindow)
              return()
            }
      
          ## check if some constraints with the same name already exist??
          query <- paste("SELECT COUNT(*) FROM DB_query_per_analisi WHERE Nome_constraint='",constrName,"' AND UserId=",userId,sep="")
          if (select.count(odbcCon,query=query) > 0)
            {
              answer <- as.character(tkmessageBox (message = "This name for the query is already in use. Overwrite it?",
                                 icon = "question",type = "yesno"))
              if (answer=='no') return()
              query <- paste("DELETE FROM DB_query_per_analisi WHERE Nome_constraint='",constrName,"' AND UserId=",userId,sep="")
              sqlCommand(channel=odbcCon,query=query)
            }
    
          ## store the values in the database
          nbRows <- dim(dataFrame)[1]
          dataFrame[dataFrame[,"Value 1"]=="","Value 1"] <- "NULL"
          dataFrame[dataFrame[,"Value 2"]=="","Value 2"] <- "NULL"
          d <- data.frame(userId=I(rep(as.integer(userId),nbRows)),
                     Nome_constraint=I(rep(constrName,nbRows)),
                     dataFrame[1:4],dataFrame[,"Value 1"],
                     dataFrame["Bound 2"],dataFrame[,"Value 2"]
                     )
          columnNames <- c("userId","Nome_constraint","[Table]","[Index]",
                               "Field","[Bound 1]","[Value 1]","[Bound 2]","[Value 2]")
          colnames(d) <- columnNames
          # create the query 

          query <- paste(columnNames,collapse=",")      
          query <- paste("INSERT INTO DB_query_per_analisi (",query,") VALUES(",sep="")
          tmp1 <- paste(d[,"Nome_constraint"],d[,"[Table]"],d[,"[Index]"],
                    d[,"Field"],d[,"[Bound 1]"],sep="','")

          tmp2 <- paste(d[,"userId"],",'",tmp1,"',",d[,"[Value 1]"],",'",d[,"[Bound 2]"],"',",d[,"[Value 2]"],")",sep="")
          tmp1 <- paste(query,tmp2,collapse=";")
          sqlCommand(channel=odbcCon,query=tmp1)
      
          yesno <- as.character(tkmessageBox (message = "The table has been saved.\nClose the form?",icon = "info",
                       type = "yesno",parent=topWindow))

          if (yesno=="yes")
            {
              tkdestroy(topWindow)
            }
      
          rm(query,tmp1,tmp2)
        }   ## end onSave
      
      onClose <- function() {tkdestroy(topWindow)}
       
      ## create the labelframe containing the selections and filters
      f.selection <- tkframe(parent=topWindow)
  
      ## the name of the query
      l.constrName <- create_label(parent=f.selection,value="Constraint's name")
      e.constrName <- create_entry(parent=f.selection,value="",width="20",justify="left")
  
      ## the Table combo
      l.table <- create_label(parent=f.selection,value="Table")
  
      query = "SELECT DISTINCT Nome_tabella FROM DB_tabelle WHERE Id_Provider = 1 ORDER BY Nome_tabella"
      df.tableNames <- sql.get.table(odbcCon,query=query)
      cb.table <- create_combo(parent=f.selection,values=df.tableNames[,"Nome_tabella"],
                           editable=FALSE,width=19,modifyCommand=onTableUpdate)
      Table <- df.tableNames[1,"Nome_tabella"]
      cb.table[["set.selection"]](Table)

      ##the Index combo
      l.index <- create_label(parent=f.selection,value="Index")
      cb.index <- create_combo(parent=f.selection,editable=FALSE,width=27,
                           modifyCommand=onIndexUpdate)


      ## the Field combo
      l.field <- create_label(parent=f.selection,value="Field")
      cb.field <- create_combo(parent=f.selection,editable=FALSE,width=31)

      onTableUpdate()
    
      ## create the first labelframe containing the selections and filters
      lf.constraints <- create_labelFrame(parent=f.selection,text="Constraints")
      cb.constrKindLow <- create_combo(parent=lf.constraints[["labelFrame"]],editable=FALSE,
                        values=c(">=",">"),width=2,startValue=">=")
      e.constrLow <- create_entry(parent=lf.constraints[["labelFrame"]],value="",width="9",justify="right")
      l.and <- tklabel(lf.constraints[["labelFrame"]],text="and")
      cb.constrKindUp <- create_combo(parent=lf.constraints[["labelFrame"]],editable=FALSE,
                        values=c("<=","<"),width=2,startValue="<=")
      e.constrUp <- create_entry(parent=lf.constraints[["labelFrame"]],value="",width="9",justify="right")
      b.insert <- create_button(parent=lf.constraints[["labelFrame"]],text="Insert",command=onInsert)
  
      ## create the data.frame
      df.constr <- data.frame(Table=I(character(0)),Index=I(character(0)),Field=I(character(0)),Type1=I(character(0)),Low=numeric(0),Type2=I(character(0)),Up=numeric(0))
      #df.constr <- data.frame(Table=I("aaa"),Index=I("dd"),Type1=I(">"),Low=0.15,Type2=I("<"),Up=0.23)
  
      dimnames(df.constr)[[2]] <- c("Table","Index","Field","Bound 1","Value 1","Bound 2","Value 2")
      editStartValue = list(Bound=c(">=",">"),Bound2=c("<=","<"))
      names(editStartValue) <- c("Bound 1","Bound 2")
      tbl.constr <- create_tablelist(parent=topWindow,dataFrame=df.constr,width=120,
                                 height=16,withScrollBarX=TRUE,withScrollBarY=TRUE,
                                 editable=c(rep("no",3),"ComboBox","Entry","ComboBox","Entry"),
                                 editStartValue=editStartValue,colFormats=c(rep("no",4),"round2","no","round2")
                                )

      ## create the remove, save, close buttons
      f.buttons <- tkframe(parent=topWindow)
      b.remove <- create_button(parent=f.buttons,text="Remove",command=onRemove)
      b.save <- create_button(parent=f.buttons,text="Save",command=onSave)
      b.close <- create_button(parent=f.buttons,text="Close",command=onClose)
  
      tkgrid(f.selection,sticky="w")
      tkgrid(l.constrName[["label"]],e.constrName[["entry"]],padx=padx,pady=pady,sticky="w")
      tkgrid(l.table[["label"]],cb.table[["combo"]],padx=padx,pady=pady,sticky="w")
      tkgrid(l.index[["label"]],cb.index[["combo"]],padx=padx,pady=pady,sticky="w")
      tkgrid(l.field[["label"]],cb.field[["combo"]],padx=padx,pady=pady,sticky="w")
      tkgrid(lf.constraints[["labelFrame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
      tkgrid(cb.constrKindLow[["combo"]],e.constrLow[["entry"]],l.and,
         cb.constrKindUp[["combo"]],e.constrUp[["entry"]],b.insert[["button"]],
         padx=padx,pady=pady,sticky="w")
      tkgrid(tbl.constr[["frame"]],padx=padx,pady=10)
  
      tkgrid(b.remove[["button"]],b.save[["button"]],b.close[["button"]],padx=padx,pady=10)
      tkgrid(f.buttons)
  
      if (updateAnalysis)
        {
          
          selectedConstr <- cb.crossAnalysis[["get.selection"]]()
          e.constrName[["set.value"]](newValue=selectedConstr)
          df.selectedConstr <- crossAnalysis[["get.dataframe"]](constraintName=selectedConstr)
          tbl.constr[["insert.data.frame"]](data=df.selectedConstr)
        }
      
    } ## end crossAnaliysWindow
  
  crossAnalysis[["get.dataframe"]] <- function(constraintName)
    {
      query <- paste("SELECT * FROM DB_query_per_analisi WHERE userId = ",userId,
                     " AND Nome_constraint = '",constraintName,"' ",
                     "ORDER BY [Table], [Index], Field",
                     sep=""
                    )

      df.selectedConstr <- sql.get.table(odbcCon,query=query)
      df.selectedConstr <- replace.NA(df.selectedConstr,fields=c("Value 1","Value 2"),values="")
      return(df.selectedConstr[-(1:2)])
    }
    
    
    
  crossAnalysis[["constructConstraintQuery"]] <- function(dataFrame)
    {
      doSelectPart <- function(d)
        {
          query <- paste(d[["Table"]],"_storico.[",d[["Field"]],"]",sep="",collapse=",")
          return(paste("SELECT ",d[1,"Table"],"_storico.Paese AS Country,B.BrancheMarkowitz AS Sector,",
                       d[1,"Table"],"_storico.Ticker,B.Ticker_AAA,", query,sep=""))
        }

      doFromPart <- function(d)
        {
          firstTable <- paste(d[1,1],"_storico",sep="")
          if (dim(d)[1]>1)
            {
              otherTables <- paste(d[-1,1],"_storico",sep="")
              query <- paste("INNER JOIN ",otherTables," ON ",
                              otherTables,".Data_download = ",firstTable,".Data_download"," AND ",
                              otherTables,".Ticker = ",firstTable,".Ticker",sep="",collapse=" ")
              query <- paste("FROM",firstTable,query)
            }
          else
            {
              query <- paste("FROM",firstTable)
            }
          ## consider the join with Lista_da_importare_storico
     
          query <- paste(query," INNER JOIN Lista_da_importare_storico B ON ",firstTable,
                         ".Ticker = B.Ticker AND ",firstTable,".Id_Provider = B.Id_Provider",
                         " AND ",firstTable,".Data_download = B.Data_download",sep=""
                        )
          
          return(query)
        }
        
      doWherePart <- function(d)
        {
          datum <- cb.dates[["get.selection"]]()
          datumSql <- paste(substr(datum,6,7),"-",substr(datum,9,10),"-",substr(datum,1,4),sep="")
          
          query <- paste(d[["Table"]],"_storico.[",d[["Field"]], "] ", d[["Bound"]],
                         " ",d[["Value"]],sep="",collapse=" AND ")
          query <- paste("WHERE ", query," AND ",d[1,"Table"],"_storico.Data_download = '",datumSql,"'",sep="")
          return(query)
        }

      doOrderPart <- function(d)
        {
          query <- paste(" ORDER BY ",d[1,"Table"],"_storico.Paese,B.BrancheMarkowitz,",
                         d[1,"Table"],"_storico.Ticker",sep="")
        }
      
      
      ## first consider the Bound 1
      step1 <- c("Table","Index","Field","Bound 1","Value 1")
      isNotEmpty <- dataFrame[,"Value 1"] != ""
      if (any(isNotEmpty))
        { 
          d <- dataFrame[isNotEmpty,step1]
          colnames(d) <- c("Table","Index","Field","Bound","Value")
        }
      
      ## Then consider Bound 2  
      step2 <- c("Table","Index","Field","Bound 2","Value 2")      
      isNotEmpty <- dataFrame[,"Value 2"] != ""
      if (any(isNotEmpty))
        {
          if (exists("d",inherits = FALSE))
            {
              dd <- dataFrame[isNotEmpty,step2]
              colnames(dd) <- c("Table","Index","Field","Bound","Value")
              d <- rbind(d,dd)
              rm(dd)  
            }
          else
            {
              d <- dataFrame[isNotEmpty,step2]
              colnames(d) <- c("Table","Index","Field","Bound","Value")
            }
        }
        
        ## construct the "SELECT" part
        selectPart <- doSelectPart(unique(d[c("Table","Field")]))
       
        ## construct the "FROM" part
        fromPart <- doFromPart(unique(d["Table"]))  
         
        ## construct the WHERE part
        wherePart <- doWherePart(d)
        
        ## construct the ORDER part
        orderPart <- doOrderPart(d)
   
        return(paste(selectPart,fromPart,wherePart,orderPart))
    }  ## end function crossAnalysis["constructConstraintQuery"]]
  
  
  onCrossAnalysisCombo <- function()
    {
      selectedConstr <- cb.crossAnalysis[["get.selection"]]()
      if (selectedConstr == "")
        {
          tkmessageBox (message = "Please select something",icon = "error",
                       type = "ok",parent=topWindow)
          return()
        }
      
      df.selectedConstr <- crossAnalysis[["get.dataframe"]](constraintName=selectedConstr)  
      workingQuery <- crossAnalysis[["constructConstraintQuery"]](dataFrame=df.selectedConstr) 
      updateNotebook(workingQuery=workingQuery)
    }
  
  onShowHistogram <- function()
    {
      if(exists("fullSelection")) updateHistogram(filteredSelection[["df.data"]])   
    }

  onShowBoxplot <- function()
    {
      if(exists("fullSelection")) updateBoxplot(filteredSelection[["df.data"]])   
    }





  onPeriodUpdate <- function() {return()}
  
  onEvolution <- function()
    {
      if (ckb.evolution[["get.value"]]()=="1")
        {
          tkconfigure(cb.firstDate[["combo"]],state="normal")
        }
      else
        {
          tkconfigure(cb.firstDate[["combo"]],state="disabled")        
        }
    }
    
  onFirstDate <- function()
    {
    
    
    }
    


  ## create the function used to plot the histograms
  plotHistogram <- function(df.data,notebook)
    {
      ## create the counter of the number of tabs used to create the      
      ## tab's name                                                  
      tabNameCounter <- 0
            
      ## create the containers of the data and functions for the plot                      
      envir <- list()
      f.plot.graphic <- list()
      plot.graphic <- list()
      b.exportGraphic <- list()
            
      ## determine the number of sub indices; the first three column are 
      ## Country, Sector, Ticker, i.e. nb.pre.index = 3   
      nb.sub.index <- dim(df.data)[[2]] - nb.pre.index
      countries <- unique(df.data[,"Country"])
      nb.countries <- length(countries)
      if (nb.countries > 1) 
        {
          oneCountry = FALSE
        } 
      else 
        {
          oneCountry = TRUE
        }
      rm(countries)

      for (i in 1:nb.sub.index) 
        { 
          j <- nb.pre.index + i
          notNull <- df.data[,j] != 0 & !is.na(df.data[,j])
          subIndexData <- df.data[notNull,c(1,j)]
          if (dim(subIndexData)[1]==0) next 

          ## create the vector of "Country" factors
          countries <- factor(subIndexData[,"Country"])
          nb.countryObs = tapply(subIndexData[,"Country"],countries,length)
          nb.obs <- sum(nb.countryObs)
          nb.countries <- length(levels(countries))

          nb.pages = nb.countries %/% 4
          if (nb.countries > nb.pages*4)
            {
              nb.pages = nb.pages + 1
            }

          for (q in 1:nb.pages) 
            {        
              ## aggiungi un tab dove inserire il grafico ed un relativo environment
              tabNameCounter <- tabNameCounter + 1 
              envir[[tabNameCounter]] <- new.env()
                
              ## riempi l'environment con i dati che dipendono da j e che sono utilizzati
              ## nella costruzione del grafico
              envir[[tabNameCounter]][["subIndexData"]] <- subIndexData
              envir[[tabNameCounter]][["countries"]] <- countries            
              envir[[tabNameCounter]][["nb.countries"]] <- nb.countries                                 
              envir[[tabNameCounter]][["nb.pre.index"]] <- nb.pre.index
              envir[[tabNameCounter]][["oneCountry"]] <- oneCountry
              envir[[tabNameCounter]][["background"]] <- background
                
              notebook[["add.tab"]](label=paste("Hist ",tabNameCounter))
              ## notebook[["add.tab"]](label=names(subIndexData)[2])
              
              ## create the function doing the plot
              envir[[tabNameCounter]][["q"]] <- q                                
              envir[[tabNameCounter]][["funcplot"]] <-  function()
                {
                  oldParams <- par(bg=background)
                  par(oma=c(1,1,4,1))
                  delta <- (q-1)*4

                  if (oneCountry) 
                    {
                      par(mfrow=c(1,1))
                    } 
                  else 
                    {
                      par(mfrow=c(2,2))
                    }
                  ## get the range limits values
                  rangeDown <- e.rangeDown[["get.value"]]()
                  rangeUp <- e.rangeUp[["get.value"]]()
              
                  inizio <- 1 + delta
                  fine <- min(nb.countries, delta+4)
                  for (country in levels(countries)[inizio:fine]) 
                    {
                      desiredCountry <- subIndexData[,1] == country
                      
                      ## verify range bounds

                      validRows <- rep(TRUE,length(desiredCountry))
                      if (rangeDown != "")
                        {
                          validRows <- validRows & (subIndexData[,2] >= as.numeric(rangeDown)) 
                        }
                      if (rangeUp != "")                                
                        {                                                                  
                          validRows <- validRows & (subIndexData[,2] <= as.numeric(rangeUp))
                        }
                      valid = (desiredCountry & validRows)                                                                  
                      hist(subIndexData[valid,2],freq=TRUE,breaks=11,main=country,
                      xlab=paste(sum(valid)," observations",sep=""))
                    }
                  mtext(names(subIndexData)[2],line=0,side=3,outer=TRUE)
                  par(oldParams)
                }
                environment(envir[[tabNameCounter]][["funcplot"]]) <- envir[[tabNameCounter]] 
                f.plot.graphic[[tabNameCounter]] <- tkframe(notebook[[paste("frame",notebook[["nbTabs"]],sep="")]])                                    
                plot.graphic[[tabNameCounter]] <- create_tkrplot(parent=f.plot.graphic[[tabNameCounter]],func=envir[[tabNameCounter]][["funcplot"]],
                                       hscale=1.8,vscale=1.8,background=background,exportButton=T)
                b.exportGraphic[[tabNameCounter]] <- create_button(f.plot.graphic[[tabNameCounter]],text="Export",
                                command=plot.graphic[[tabNameCounter]][["export"]])

                tkgrid(plot.graphic[[tabNameCounter]][["tkrplot"]])
                tkgrid(f.plot.graphic[[tabNameCounter]],padx=padx,pady=pady)
                tkgrid(b.exportGraphic[[tabNameCounter]][["button"]],padx=padx,pady=pady)
            }
        } # end for (i in 1:nb.sub.index)
    } # end function plotHistogram


  ## create the function used to plot the boxplots
  plotBoxplot <- function(df.data,notebook)
    {
      ## create the containers of the data and functions for the plot                      
      envir <- list()
      f.plot.graphic <- list()
      plot.graphic <- list()
      b.exportGraphic <- list()
            
      ## determine the number of sub indices; the first three column are 
      ## Country, Sector, Ticker, i.e. nb.pre.index = 3             
      nb.sub.index <- dim(df.data)[[2]] - nb.pre.index
          
      ## loop over the subindices
      for (tabNameCounter in 1:nb.sub.index)
        { 
          envir[[tabNameCounter]] <- new.env()
          indexName <- names(df.data)[nb.pre.index+tabNameCounter]
                
          ## riempi l'environment con i dati che dipendono da j e che sono utilizzati
          ## nella costruzione del grafico
          envir[[tabNameCounter]][["df.data"]] <- df.data                                 
          notebook[["add.tab"]](label=paste("Boxplot ",tabNameCounter))
          envir[[tabNameCounter]][["tabNameCounter"]] <-  tabNameCounter
          envir[[tabNameCounter]][["nb.pre.index"]] <-  nb.pre.index
          
          ## create the function doing the plot                              
          envir[[tabNameCounter]][["funcplot"]] <-  function()
            {
              oldParams <- par(bg=background)
              ## verify range bounds
              rangeDown <- e.rangeDown[["get.value"]]()
              rangeUp <- e.rangeUp[["get.value"]]()
              validRows <- rep(TRUE,length(df.data[,nb.pre.index+tabNameCounter])[[1]])
              if (rangeDown != "")
                {
                  validRows <- validRows & (df.data[,nb.pre.index+tabNameCounter] >= as.numeric(rangeDown)) 
                }
              if (rangeUp != "")                                
                {                                                                  
                  validRows <- validRows & (df.data[,nb.pre.index+tabNameCounter] <= as.numeric(rangeUp))
                }                                                                  

              boxplot(split(df.data[validRows,nb.pre.index+tabNameCounter],df.data[validRows,"Country"]),
                      main=indexName,col="lavender")
              par(oldParams)
            }
                    
          environment(envir[[tabNameCounter]][["funcplot"]]) <- envir[[tabNameCounter]] 
          f.plot.graphic[[tabNameCounter]] <- tkframe(notebook[[paste("frame",notebook[["nbTabs"]],sep="")]])                                    
          plot.graphic[[tabNameCounter]] <- create_tkrplot(parent=f.plot.graphic[[tabNameCounter]],func=envir[[tabNameCounter]][["funcplot"]],
                                   hscale=1.8,vscale=1.8,background=background,exportButton=T)
          b.exportGraphic[[tabNameCounter]] <- create_button(f.plot.graphic[[tabNameCounter]],text="Export",
                            command=plot.graphic[[tabNameCounter]][["export"]])
                            
          tkgrid(plot.graphic[[tabNameCounter]][["tkrplot"]])
          tkgrid(f.plot.graphic[[tabNameCounter]],padx=padx,pady=pady)
          tkgrid(b.exportGraphic[[tabNameCounter]][["button"]],padx=padx,pady=pady)
        }
    } ## end function plotBoxplot

  updateHistogram <- function(df.data,deleteFrom=1)
    {
      if (ckb.showHistogram[["get.value"]]()=="1")   ## update and plot the graph if required
        {
          if (exists("histogramWindow")) 
            {
              if (as.numeric(tkwinfo("exists",histogramWindow)))
                {
                  ## remove previous graphs, i.e. all tabs 3,4,...
                  histogramNotebook[["delete.tab"]](from=deleteFrom,to=histogramNotebook[["nbTabs"]])
                }
              else
                {
                  rm(histogramWindow,pos=1)
                  histogramWindow <<- tktoplevel()
                  tktitle(histogramWindow) <- "Histograms"
                  ## create the tabnotebook widget
                  histogramNotebook <<- create_tabnotebook (parent=histogramWindow,height=800,width=850,
                        tabpos="n",tabbackground="white",background="systemButtonFace")
                  histogramNotebook[["delete.tab"]](from=1,to=1)
                  tkgrid(histogramNotebook[["tabnotebook"]],padx=padx,pady=pady)
                }
            }
          else
            {
              # if (exists("histogramWindow")) tkdestroy(histogramWindow)
              histogramWindow <<- tktoplevel()
              tktitle(histogramWindow) <- "Histograms"
              ## create the tabnotebook widget
              histogramNotebook <<- create_tabnotebook (parent=histogramWindow,height=800,width=850,
                        tabpos="n",tabbackground="white",background="systemButtonFace")
              histogramNotebook[["delete.tab"]](from=1,to=1)
              tkgrid(histogramNotebook[["tabnotebook"]],padx=padx,pady=pady)  
            }
        
          ## plot the graphs
          plotHistogram(df.data,notebook=histogramNotebook)
          histogramNotebook[["select.tab"]]()
          return() 
        }
      if (ckb.showHistogram[["get.value"]]()=="0")   ## remove the histogramWindow
        {
          if (exists("histogramWindow"))
            {
              if (as.numeric(tkwinfo("exists",histogramWindow)))
                {
                  tkdestroy(histogramWindow)
                }
              rm("histogramWindow",pos=1)
            }
          if (exists("histogramNotebook")) rm("histogramNotebook",pos=1)
        }
      return()
    }
    

  updateBoxplot <- function(df.data,deleteFrom=1)
    {
       if (ckb.showBoxPlot[["get.value"]]()=="1")   ## update and plot the graph if required
        {
          if (exists("boxPlotWindow"))
            {
              if (as.numeric(tkwinfo("exists",boxPlotWindow)))
                {
                  ## remove previous graphs, i.e. all tabs 3,4,...
                  boxplotNotebook[["delete.tab"]](from=deleteFrom,to=boxplotNotebook[["nbTabs"]])            
                }
              else
                {
                  rm(boxPlotWindow,pos=1)
                  boxPlotWindow <<- tktoplevel()
                  tktitle(boxPlotWindow) <- "Boxplots"
                  ## create the tabnotebook widget
                  boxplotNotebook <<- create_tabnotebook (parent=boxPlotWindow,height=800,width=850,
                        tabpos="n",tabbackground="white",background="systemButtonFace")
                  boxplotNotebook[["delete.tab"]](from=1,to=1)
                  tkgrid(boxplotNotebook[["tabnotebook"]],padx=padx,pady=pady)
                }
            }
          else
            {
              # if (exists("boxPlotWindow")) tkdestroy(boxPlotWindow)
              boxPlotWindow <<- tktoplevel()
              tktitle(boxPlotWindow) <- "Boxplots"
              ## create the tabnotebook widget
              boxplotNotebook <<- create_tabnotebook (parent=boxPlotWindow,height=800,width=850,
                        tabpos="n",tabbackground="white",background="systemButtonFace")
              boxplotNotebook[["delete.tab"]](from=1,to=1)
              tkgrid(boxplotNotebook[["tabnotebook"]],padx=padx,pady=pady)  
            }
        
          ## plot the graphs
          plotBoxplot(df.data,notebook=boxplotNotebook)
          boxplotNotebook[["select.tab"]]()
          return() 
        }

      if (ckb.showBoxPlot[["get.value"]]()=="0")   ## remove the boxplotWindow
        {
          if (exists("boxPlotWindow"))
            {
              if (as.numeric(tkwinfo("exists",boxPlotWindow)))
                {
                  tkdestroy(boxPlotWindow)
                }
              rm("boxPlotWindow",pos=1)
            }
          if (exists("boxplotNotebook")) rm("boxplotNotebook",pos=1)
        }
      return()
    }


    
    
  get.data <- function(Table,index,datumSql,useFactors=FALSE,query)
    {
      if (missing(query))
        {
          fields <- c("Nome_indice","Nome_campo","Ordinamento_intra_indice")
          desired <- (DB_dati_tabelle[,"Nome_tabella"] == Table) & (DB_dati_tabelle[,"Nome_indice"] == index)
          df.fieldNames <- DB_dati_tabelle[desired,fields] 
          rm(fields,desired)
      
          if (nrow(df.fieldNames)==0)
            {
              tkmessageBox(message="Table without fields",
                     icon="error",type="ok",parent=topWindow)
              return()        
            }
    
          desiredFieldNames <- df.fieldNames[,"Nome_indice"] == index
          query <- paste("A.[",df.fieldNames[desiredFieldNames,"Nome_campo"],"]",sep="",collapse=",")
          query <- paste(query," FROM ",Table,"_storico A ",sep="")
      
          ## create the query used to get the data to visualize
          ## the first field has been removed, i.e. A.Data_download AS [Date], 
          query <- paste("SELECT A.Paese AS Country, B.BrancheMarkowitz AS Sector, A.Ticker, B.Ticker_AAA, ",query,
                         "INNER JOIN Lista_da_importare_storico B ON A.Ticker = B.Ticker AND A.Id_Provider = B.Id_Provider ",
                         "WHERE A.Data_download = B.Data_download AND A.Data_download = '",datumSql,"' ",
                         "ORDER BY B.Paese, B.BrancheMarkowitz, A.Ticker",
                         sep=""
                        )
        }
                     
      if (useFactors)
        {
          df.data <- sql.get.table(odbcCon,query=query,as.is=FALSE)   
        }
      else
        {
          ## change the number of as.is if you modify the number of fields!
          df.data <- sql.get.table(odbcCon,query=query,as.is=1:4)        
        }
      ## compute the number of data fields, i.e. fields with data to be analysed or plotted
      nbDataFields <- dim(df.data)[2] - nb.pre.index  
      
      return(list(df.data=df.data,nbDataFields=nbDataFields))
    } ## end function get.data

    
  filterData <- function(df.data,country="",sector="",onlyAaaTicker=0)
    {
      ## apply the filter

      desired <- rep(TRUE,dim(df.data)[1])
      
      if (country!="")
        {
          isDesiredCountry <- df.data[,"Country"] == country
          desired <- desired & isDesiredCountry
          rm(isDesiredCountry)
        }

      if (sector!="")
        {
          isDesiredSector <- df.data[,"Sector"] == sector
          desired <- desired & isDesiredSector
          rm(isDesiredSector)
        }
        
      if (onlyAaaTicker==1)
        {
          isOnlyAAA <- df.data[,"Ticker_AAA"] != "nullo"
          desired <- desired & isOnlyAAA
          rm(isOnlyAAA)
        }
      
      if (any(!desired))
        {
          df.data <- df.data[desired,]

          if (is.factor(df.data[["Country"]]))
            {
              df.data[["Country"]] <- factor(df.data[["Country"]])
              df.data[["Sector"]] <- factor(df.data[["Sector"]])
            }
        }
      return(df.data)  
    } ## end function filerData
    
  updateNotebook <- function(updateData=TRUE,workingQuery)
    {
    provider <- cb.provider[["get.selection"]]()
    Table <- cb.table[["get.selection"]]()
    index <- cb.index[["get.selection"]]()
    datum <- cb.dates[["get.selection"]]()
    datumSql <- paste(substr(datum,6,7),"-",substr(datum,9,10),"-",substr(datum,1,4),sep="")
    country <- cb.country[["get.selection"]]()
    sector <- cb.sector[["get.selection"]]()

    onlyAaaTicker <- as.numeric(ckb.Aaa[["get.value"]]())




    if (ckb.crossAnalysis[["get.value"]]()=="0")
      {
        if ((provider=="") | (Table=="") | (index=="") | (datum==""))
          {
            tkmessageBox(message="Some required field is empty",
                   icon="error",type="ok",parent=topWindow)
            return()
          }
      }
    else
      {
        if (cb.crossAnalysis[["get.selection"]]()=="")           
          {                                                                     
            tkmessageBox(message="No cross analysis selected",                
                   icon="error",type="ok",parent=topWindow)                     
            return()                                                                  
          }                                                                     
      }

    # get the data
    if (updateData)
      {
        if (missing(workingQuery))
          {
            fullSelection <<- get.data(Table,index,datumSql)
          }
        else
          {
            fullSelection <<- get.data(query=workingQuery)  
          }
      }
      
    ## remove the tablelist
    if (exists("tbl.data")) tkdestroy(tbl.data[["frame"]])
    nbColumns <- dim(fullSelection[["df.data"]])[2]
    
    filteredSelection <<- fullSelection
    filteredSelection[["df.data"]] <<- filterData(df.data=fullSelection[["df.data"]],country,sector,onlyAaaTicker)
    

    ## create a new tablelist with the new data
    tbl.data <<- create_tablelist(parent=tbn.notebook[["frame1"]],dataFrame=filteredSelection[["df.data"]][-4],width=130,
                                 height=42,withScrollBarX=TRUE,withScrollBarY=TRUE,colFormats=
                                 c(rep("no",3),rep("round2",nbColumns-4))
                                )
    
    ## insert the new one
    tkgrid(tbl.data[["frame"]])
    
    ## update the comboboxes sector and country
    if (updateData)
      { 
        sectorNewValues <- fullSelection[["df.data"]][c("Country","Sector")]
        if (dim(sectorNewValues)[[1]]>0)
          { 
            cb.sector[["modify.values"]](c("",unique(sectorNewValues[,"Sector"])))
            tmp <- unique(sectorNewValues[,"Country"])
            tmp <- tmp[order(tmp)]
            cb.country[["modify.values"]](c("",tmp))
            rm(tmp)
          }
        else
          {
            cb.sector[["modifiy.values"]]("")
            cb.country[["modify.values"]]("")
          }
      } ## end if (updateFilter)

                                                
    ## get the data
    if (updateData)
      {
        if (missing(workingQuery))
          {                                                     
            fullSelectionForReport <<- get.data(Table,index,datumSql,useFactors=TRUE)
          }
        else
          {
            fullSelectionForReport <<- get.data(query=workingQuery,useFactors=TRUE)
          }
      }   
      
    fieldnames <- dimnames(fullSelectionForReport[["df.data"]])[[2]][-(1:4)]
      
## assign("a",dataSelection,envir=globalenv())
    filteredSelectionForReport <<- fullSelection
    filteredSelectionForReport[["df.data"]] <<- filterData(df.data=fullSelectionForReport[["df.data"]],country,sector,onlyAaaTicker)
    
   
    updateHistogram(filteredSelectionForReport[["df.data"]])
    updateBoxplot(filteredSelectionForReport[["df.data"]])
    tbn.notebook[["select.tab"]]()
      
    ## are reports required?
    reportAllRequired <- ckb.reportAll[["get.value"]]() == "1"
    reportByCountryRequired <- ckb.reportByCountry[["get.value"]]() == "1"
    reportBySectorRequired <- ckb.reportBySector[["get.value"]]() == "1"
    reportByCountrySectorRequired <- ckb.reportByCountrySector[["get.value"]]() == "1"  

    if (any(reportAllRequired,reportByCountryRequired,reportBySectorRequired,reportByCountrySectorRequired))
    {                                           
      ## create the report

      ## clear the tabnotebook
      txw.report[["delete"]]()
      .Tcl(paste(txw.report[["textwindow"]],"tag configure small -font {courier 10}"))      
      .Tcl(paste(txw.report[["textwindow"]],"tag configure major -font {courier 14 bold}"))
      .Tcl(paste(txw.report[["textwindow"]],"tag configure bold -font {courier 12 bold}"))
      .Tcl(paste(txw.report[["textwindow"]],"tag configure blue -foreground blue"))
      if (reportAllRequired)
        {
          ## compute the summary statistics over all data
          ## create the vector of the fields names

          X <- filteredSelectionForReport[["df.data"]][fieldnames]

          allReport <- lapply(X=X,FUN=summaryStatistics)
          names(allReport) <- fieldnames
        
          tmp <- lapply(allReport,FUN=writeSummaryStatistics)
          
          txw.report[["insert"]](text="Analisi su tutte le osservazioni\n\n",tag="{major}") 
          for (field in fieldnames)
            {
              ##  insert into txw.report
              txw.report[["insert"]](text=paste("Report on",field),tag="{bold blue}")
              txw.report[["insert"]](text=paste("\n\n",tmp[[field]],"\n\n\n\n",sep=""))
            }
        }

      if (reportByCountryRequired)
        {
          ## compute the summary statistics grouped by country
          countryReport <- list()
          countryReportString <- list()
          
          for (field in fieldnames)
            {
              tmp <- list()
              countryReport[[field]] <- tapply(X=filteredSelectionForReport[["df.data"]][,field],
                     INDEX=filteredSelectionForReport[["df.data"]][,"Country"],FUN=summaryStatistics)
              for (country in names(countryReport[[field]]))
                {
                  tmp[[country]] <- writeSummaryStatistics(countryReport[[field]][[country]])
                }
              countryReportString[[field]] <- tmp 
            }
          
          txw.report[["insert"]](text="\n\n\nAnalisi per paese\n\n",tag="{major}")
          for (field in fieldnames)
            {
              txw.report[["insert"]](text=paste("Report on",field),tag="{bold blue}")
              
              for (country in names(countryReport[[field]]))
                {
                  txw.report[["insert"]](text=paste("\n\nCountry:",country,"- "),tag="{bold}")
                  txw.report[["insert"]](text=field,tag="{small}")
                  ##  insert into txw.report
                  txw.report[["insert"]](text=paste("\n\n",countryReportString[[field]][[country]],"\n\n",sep=""))
                }
            }  
 
        }
        
      if (reportBySectorRequired)
        { 
          ## compute the summary statistics grouped by sector
          sectorReport <- list()
          sectorReportString <- list()
          
          for (field in fieldnames)
            {
              tmp <- list()
              sectorReport[[field]] <- tapply(X=filteredSelectionForReport[["df.data"]][,field],
                     INDEX=filteredSelectionForReport[["df.data"]][,"Sector"],FUN=summaryStatistics)
              for (sector in names(sectorReport[[field]]))
                {
                  tmp[[sector]] <- writeSummaryStatistics(sectorReport[[field]][[sector]])
                }
              sectorReportString[[field]] <- tmp 
            }
          
          txw.report[["insert"]](text="\n\nAnalisi per settore\n\n",tag="{major}")
          for (field in fieldnames)
            {
              txw.report[["insert"]](text=paste("Report on",field),tag="{bold blue}")
              
              for (sector in names(sectorReport[[field]]))
                {
                  ##  insert into txw.report
                  txw.report[["insert"]](text=paste("\n\nSector:",sector,"- "),tag="{bold}")
                  txw.report[["insert"]](text=field,tag="{small}")
                  txw.report[["insert"]](text=paste("\n\n",sectorReportString[[field]][[sector]],"\n\n",sep=""))
                }
            }            
        }
        
      if (reportByCountrySectorRequired)
        {
  
          ## compute the summary statistics grouped by country and sector
          countrySectorReport <- list()
          countrySectorReportString <- list()
          for (field in fieldnames)
            {
              tmp <- list()
              countrySectorReport[[field]] <- tapply(X=filteredSelectionForReport[["df.data"]][,field],
                     INDEX=filteredSelectionForReport[["df.data"]][,c("Country","Sector")],FUN=summaryStatistics)
              Names <- dimnames(countrySectorReport[[field]])
              tmp <- list()
              for (country in Names[["Country"]])
                {
                  tmp[[country]] <- list()
                  for (sector in Names[["Sector"]])
                    {
                       tmp[[country]][[sector]] <- writeSummaryStatistics(sectorReport[[field]][[country]][[sector]])
                    }
                }
                

              #sectorReportString[[field]] <- tmp 
              print(dimnames(countrySectorReport[[field]]))
              print(countrySectorReport[[field]])
              print(is.array(countrySectorReport[[field]]))
            }
   
   
   
   
          
          
          #for (i in 1:dataSelection[["nbDataFields"]])
          #  {
          #    countrySectorReport[[i]] <- tapply(X=dataSelection[["df.data"]][,i+3],
          #          INDEX=dataSelection[["df.data"]][,1:2],FUN=summaryStatistics)    
          #  }
          #names(countrySectorReport) <- fieldnames
        }
#assign("countryReport",countryReport,envir=globalenv())

    } ## end if (any(...))
    
    ## finish the procedure
    tkraise(topWindow)
    tkfocus(topWindow)
  }  # end function updateNotebook                  
  
  ## create the first labelframe containing the selections and filters
  lf.selection <- create_labelFrame(parent=topWindow,text="Selections and filters")
  
  l.provider  <- create_label(parent=lf.selection[["labelFrame"]],value="Provider")
  cb.provider <- create_combo(parent=lf.selection[["labelFrame"]],values="UBS",
                 startValue="UBS",editable=FALSE)
  
  ## get the values for the table combo
  l.table <- create_label(parent=lf.selection[["labelFrame"]],value="Table")
  query = "SELECT DISTINCT Nome_tabella FROM DB_tabelle WHERE Id_Provider = 1 ORDER BY Nome_tabella"
  df.tableNames <- sql.get.table(odbcCon, query=query)
  cb.table <- create_combo(parent=lf.selection[["labelFrame"]],values=df.tableNames[,"Nome_tabella"],
                           editable=FALSE,width=19,modifyCommand=onTableUpdate)
  Table <- df.tableNames[1,"Nome_tabella"]
  cb.table[["set.selection"]](Table)

  ## get the values for the date combo
  l.dates <- create_label(parent=lf.selection[["labelFrame"]],value="Dates")
  query = paste("SELECT DISTINCT Data_download FROM ",Table,
                "_storico WHERE (Id_Provider=1) ORDER BY Data_download DESC",sep="")
  df.dates <- sql.get.table(odbcCon, query=query)
  cb.dates <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,modifyCommand=onDateUpdate)
  if (nrow(df.dates)>0)
    {
      tmp <- format.Date(as.Date(df.dates[,"Data_download"]))
      cb.dates <- create_combo(parent=lf.selection[["labelFrame"]],values=tmp,
                           editable=FALSE,modifyCommand=onDateUpdate)
      # cb.dates[["modify.values"]](tmp)
      cb.dates[["set.selection"]](tmp[1])
      rm(tmp)  
    }

    
  ## get the values for the index combo
  l.index <- create_label(parent=lf.selection[["labelFrame"]],value="Index")
  cb.index <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,width=27,modifyCommand=onIndexUpdate)
  onTableUpdate()

  ## get the values for the period combo
  l.period <- create_label(parent=lf.selection[["labelFrame"]],value="Period")
  cb.period <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,width=29,modifyCommand=onPeriodUpdate)
  onPeriodUpdate()

                           
  ## create the filter label
  l.filter <- create_label(parent=lf.selection[["labelFrame"]],value="Filters:")
  
  ## create the country, sector, onlyAaaTickers filters
    ## country
  l.country <- create_label(parent=lf.selection[["labelFrame"]],value="Country")
  cb.country <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,modifyCommand=onCountryUpdate)
                            
    ## Sector
  l.sector <- create_label(parent=lf.selection[["labelFrame"]],value="Sector")
  cb.sector <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,modifyCommand=onSectorUpdate)
  
    ## check box
  ckb.Aaa <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Only AAA tickers",
                                command=onOnlyAaaUpdate)



  ## create the Cross analysis
  ## the label
  l.crossAnalysis <- create_label(parent=lf.selection[["labelFrame"]],value="Cross analysis:")
  ckb.crossAnalysis <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Activate",
                          command=onCrossAnalysis)
  cb.crossAnalysis <- create_combo(parent=lf.selection[["labelFrame"]],
                           editable=FALSE,modifyCommand=onCrossAnalysisCombo)
  f.crossAnalysisButtons <- tkframe(parent=lf.selection[["labelFrame"]])
  b.crossAnalysisNew <- create_button(parent=lf.selection[["labelFrame"]],text="New",command=onCrossAnalysisNew)
  b.crossAnalysisModify <- create_button(parent=f.crossAnalysisButtons,text="modify",command=onCrossAnalysisModify)
  b.crossAnalysisRemove <- create_button(parent=f.crossAnalysisButtons,text="Remove",command=onCrossAnalysisRemove)
  tkconfigure(cb.crossAnalysis[["combo"]],state="disabled")
  tkconfigure(b.crossAnalysisNew[["button"]],state="disabled")
  tkconfigure(b.crossAnalysisModify[["button"]],state="disabled")
  tkconfigure(b.crossAnalysisRemove[["button"]],state="disabled")
  
  
    
  ## create the graphicalOutput label
  l.graphicalOutput <- create_label(parent=lf.selection[["labelFrame"]],value="Graphics:")
  ## create the show histograms and the show boxplot check buttons 
  ckb.showHistogram <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Show histogram",
                          command=onShowHistogram)
  ckb.showBoxPlot <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Show boxplot",  
                          command=onShowBoxplot)
  ## create the range label end min_range and max_range entry fields into a frame
  f.range <- tkframe(parent=lf.selection[["labelFrame"]])
  l.range <- create_label(parent=f.range,value="Limit range:")
  e.rangeDown <- create_entry(parent=f.range,value="",width="9",justify="right")
  e.rangeUp   <- create_entry(parent=f.range,value="",width="9",justify="right")



  ## create the reportsOutput label
  l.reportOutput <- create_label(parent=lf.selection[["labelFrame"]],value="Reports:")
  ## create the reports check buttons 
  ckb.reportAll <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Report over all data")
  ckb.reportByCountry <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Report by country")
  ckb.reportBySector <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Report by sector")
  ckb.reportByCountrySector <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Report by country & sector")
  
  ## create the evolution label
  l.evolution <- create_label(parent=lf.selection[["labelFrame"]],value="Evolution:")
  ## create the show evolution checkbox and the firstDate combobox 
  ckb.evolution <- create_checkButton(parent=lf.selection[["labelFrame"]],label="Show evolution from:",
                          command=onEvolution)
  cb.firstDate <- create_combo(parent=lf.selection[["labelFrame"]],values=cb.dates[["get.values"]](),  
                          modifyCommand=onFirstDate)

  
  ## create the tabnotebook widget
  tbn.notebook <- create_tabnotebook (parent=topWindow,label=c("Data","Report"),height=800,width=850,
                                        tabpos="n",tabbackground="white",background="systemButtonFace")

  txw.report <- create_textwindow (parent=tbn.notebook[["frame2"]],height=30,width=75,
                                        withScrollBarY=TRUE,withScrollBarX=TRUE)
                                  
  ## show the widgets
  tkgrid(l.provider[["label"]],cb.provider[["combo"]],padx=padx,pady=pady,sticky="wn")
  tkgrid(l.dates[["label"]],cb.dates[["combo"]],padx=padx,pady=pady,sticky="w")
  tkgrid(l.table[["label"]],cb.table[["combo"]],padx=padx,pady=pady,sticky="w")
  tkgrid(l.index[["label"]],cb.index[["combo"]],padx=padx,pady=pady,sticky="w")



  tkgrid(l.filter[["label"]],padx=padx,pady=c(20,pady),sticky="w")
  tkgrid(l.country[["label"]],cb.country[["combo"]],padx=padx,pady=pady,sticky="w")
  tkgrid(l.sector[["label"]],cb.sector[["combo"]],padx=padx,pady=pady,sticky="w")
  tkgrid(ckb.Aaa[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")


  tkgrid(l.crossAnalysis[["label"]],padx=padx,pady=c(20,pady),sticky="w")
  tkgrid(ckb.crossAnalysis[["frame"]],b.crossAnalysisNew[["button"]],padx=padx,pady=pady,sticky="w")
  tkgrid(cb.crossAnalysis[["combo"]],padx=15,pady=pady,columnspan=2,sticky="w")
  tkgrid(b.crossAnalysisModify[["button"]],b.crossAnalysisRemove[["button"]],padx=padx,pady=pady)
  tkgrid(f.crossAnalysisButtons,pady=pady,columnspan=2)


  tkgrid(l.graphicalOutput[["label"]],padx=padx,pady=c(20,pady),sticky="w")  
  tkgrid(ckb.showHistogram[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(ckb.showBoxPlot[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(l.range[["label"]],sticky="w")
  tkgrid(e.rangeDown[["entry"]],row=0,column=1,padx=padx,sticky="w")
  tkgrid(e.rangeUp[["entry"]],row=0,column=2,padx=padx,sticky="w") 
  tkgrid(f.range,padx=padx,pady=pady,columnspan=2,sticky="w")



  tkgrid(ckb.evolution[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(cb.firstDate[["combo"]],padx=4*padx,pady=pady,columnspan=2,sticky="w")
  tkconfigure(cb.firstDate[["combo"]],state="disabled")



  tkgrid(l.reportOutput[["label"]],padx=padx,pady=c(20,pady),sticky="w") 
  tkgrid(ckb.reportAll[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(ckb.reportByCountry[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(ckb.reportBySector[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")
  tkgrid(ckb.reportByCountrySector[["frame"]],padx=padx,pady=pady,columnspan=2,sticky="w")


    
  tkgrid(lf.selection[["labelFrame"]],padx=padx,pady=pady,sticky="nw")
  tkgrid(tbn.notebook[["tabnotebook"]],padx=padx,pady=pady,row=0,column=1)
  tkgrid(txw.report[["frame"]],padx=padx,pady=pady)
}

## get the identity name of the user

topWindow <- tktoplevel()  #used in get.riskFactors
tktitle(topWindow) <- "Login"

l.user <- create_label(parent=topWindow,value="Username:")
cb.user <- create_combo(parent=topWindow,values=usernames)
b.okCancel <- create_okCancelButton(parent=topWindow,onOk=start_window,onCancel=function(){tkdestroy(topWindow)})


tkgrid(l.user[["label"]],cb.user[["combo"]],padx=padx,pady=pady)
tkgrid(b.okCancel,columnspan=2)