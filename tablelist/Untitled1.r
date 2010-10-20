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

nb.pre.index=3  ## variable used to determine the number of sub indices; the first three column are
                ## of the data frame containing the data are Country, Sector, Ticker, i.e. nb.pre.index = 3
userId = 1

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
DB_dati_tabelle <- get.table(odbcCon,query=query,as.is=TRUE)
rm(query)

topWindow <- tktoplevel()
tktitle(topWindow) <- "Query construnction"

  constructConstraintQuery <- function(dataFrame)
    {
      doSelectPart <- function(d)
        {
          query <- paste(d[["Table"]],"_storico.[",d[["Table"]],"_storico.[",d[["Field"]],"]",sep="",collapse=",")
          return(paste("SELECT ",d[["Table"]],"_storico.Paese AS Country,B.BrancheMarkowitz AS Sector,",
                       d[["Table"]],"_storico.Ticker ", query))
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
          query <- paste(d[["Table"]],"_storico.[",d[["Field"]], "] ", d[["Bound"]]," ",d[["Value"]],sep="",collapse=" AND ")
          query <- paste("WHERE", query)
          return(query)
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

        return(paste(selectPart,fromPart,wherePart))
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
        
      
      ## query <- constructConstraintQuery(dataFrame)
    
      ## store the values in the database
      nbRows <- dim(dataFrame)[1]
      dataFrame[dataFrame[,"Value 1"]=="","Value 1"] <- "NULL"
      dataFrame[dataFrame[,"Value 2"]=="","Value 2"] <- "NULL"
      d <- data.frame(userId=I(rep(as.integer(userId),nbRows)),
                     Nome_constraint=I(rep(constrName,nbRows)),
                     dataFrame[1:4],dataFrame[,"Value 1"],
                     dataFrame["Bound 2"],dataFrame[,"Value 2"]
                     )
      columnNames <- c("userId","Nome_constraint","TableName","IndexName",
                               "FieldName","[Bound 1]","[Value 1]","[Bound 2]","[Value 2]")
      colnames(d) <-  columnNames
      
      # create the query 

      query <- paste(columnNames,collapse=",")      
      query <- paste("INSERT INTO DB_query_per_analisi (",query,") VALUES(",sep="")
      tmp1 <- paste(d[,"Nome_constraint"],d[,"TableName"],d[,"IndexName"],
                    d[,"FieldName"],d[,"[Bound 1]"],sep="','")

      tmp2 <- paste(d[,"userId"],",'",tmp1,"',",d[,"[Value 1]"],",'",d[,"[Bound 2]"],"',",d[,"[Value 2]"],")",sep="")
      tmp1 <- paste(query,tmp2,collapse=";")
      sqlCommand(channel=odbcCon,query=tmp1)

      rm(query,tmp1,tmp2)
    }
       
  ## create the labelframe containing the selections and filters
  f.selection <- tkframe(parent=topWindow)
  
  ## the name of the query
  l.constrName <- create_label(parent=f.selection,value="Constraint's name")
  e.constrName <- create_entry(parent=f.selection,value="",width="20",justify="left")
  
  ## the Table combo
  l.table <- create_label(parent=f.selection,value="Table")
  
  query = "SELECT DISTINCT Nome_tabella FROM DB_tabelle WHERE Id_Provider = 1 ORDER BY Nome_tabella"
  df.tableNames <- get.table(odbcCon,query=query)
  cb.table <- create_combo(parent=f.selection,values=df.tableNames[,"Nome_tabella"],
                           editable=FALSE,width=19,modifyCommand=onTableUpdate)
  Table <- df.tableNames[1,"Nome_tabella"]
  cb.table[["set.selection"]](Table)

  ##the Index combo
  l.index <- create_label(parent=f.selection,value="Index")
  cb.index <- create_combo(parent=f.selection,values="",editable=FALSE,width=27,
                           modifyCommand=onIndexUpdate)


  ## the Field combo
  l.field <- create_label(parent=f.selection,value="Field")
  cb.field <- create_combo(parent=f.selection,values="",editable=FALSE,width=31)

  onTableUpdate()
    
  ## create the first labelframe containing the selections and filters
  lf.constraints <- create_labelFrame(parent=f.selection,text="Constraints")
  cb.constrKindLow <- create_combo(parent=lf.constraints[["labelFrame"]],editable=FALSE,
                        values=">=",width=2,startValue=">")
  e.constrLow <- create_entry(parent=lf.constraints[["labelFrame"]],value="",width="9",justify="right")
  l.and <- tklabel(lf.constraints[["labelFrame"]],text="and")
  cb.constrKindUp <- create_combo(parent=lf.constraints[["labelFrame"]],editable=FALSE,
                        values="<=",width=2,startValue="<")
  e.constrUp <- create_entry(parent=lf.constraints[["labelFrame"]],value="",width="9",justify="right")
  b.insert <- create_button(parent=lf.constraints[["labelFrame"]],text="Insert",command=onInsert)
  
  ## create the data.frame
  df.constr <- data.frame(Table=I(character(0)),Index=I(character(0)),Field=I(character(0)),Type1=I(character(0)),Low=numeric(0),Type2=I(character(0)),Up=numeric(0))
  #df.constr <- data.frame(Table=I("aaa"),Index=I("dd"),Type1=I(">"),Low=0.15,Type2=I("<"),Up=0.23)
  
  dimnames(df.constr)[[2]] <- c("Table","Index","Field","Bound 1","Value 1","Bound 2","Value 2")
  editStartValue = list(Bound=c(">",">="),Bound2=c("<","<="))
  names(editStartValue) <- c("Bound 1","Bound 2")
  tbl.constr <- create_tablelist(parent=topWindow,dataFrame=df.constr,width=120,
                                 height=16,withScrollBarX=TRUE,withScrollBarY=TRUE,
                                 editable=c(rep("no",3),"ComboBox","Entry","ComboBox","Entry"),
                                 editStartValue=editStartValue
                                )

  ## create the remove, save, close buttons
  f.buttons <- tkframe(parent=topWindow)
  b.remove <- create_button(parent=f.buttons,text="Remove",command=onRemove)
  b.save <- create_button(parent=f.buttons,text="Save",command=onSave)
  
  
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
  
  tkgrid(b.remove[["button"]],b.save[["button"]],padx=padx,pady=10)
  tkgrid(f.buttons)
  