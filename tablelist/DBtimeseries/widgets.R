create_timeSeriesWidget <- function(topWindow=tkframe())
  {
    onShowDatabase <- function()
      {
        colname = "Database"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showDatabase[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }
    onShowTable <- function()
      {
        colname = "Table"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showTable[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }
    onShowCurrency <- function()
      {
        colname = "Currency"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showCurrency[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }
    onShowFrequency <- function()
      {
        colname = "Frequency"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showFrequency[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }
    onShowPriceAdj <- function()
      {
        colname = "Price adjusted"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showPriceAdj[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }
    onShowDescription <- function()
      {
        colname = "Description"
        visualizeColumns[colname] <<- as.logical(as.numeric(ckb.showDescription[["get.value"]]()))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns[colname]),columns=colname)
      }

    onCategory <- function()
      {
        selectedCategory <- cb.category[["get.selection"]]()
        if (selectedCategory == "")
          {
            tkmessageBox (message = "Please select a valid category",icon = "error",
                       type = "ok",parent=topWindow)
            return()
          }

        ## get the data
        desired <- data.timeseries[,"ID_Category"] == df.category[selectedCategory,"ID"]
        data.selection <- data.timeseries[desired,]
        tkdestroy(tbl.data[["frame"]])
        tbl.data <<- create_tablelist(parent=lf.timeseries[["labelFrame"]],dataFrame=data.selection,width=120,
                       height=16,withScrollBarX=TRUE,withScrollBarY=TRUE)
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns),names(visualizeColumns))
        tkgrid(tbl.data[["frame"]],padx=padx,pady=pady,columnspan=2)

        return()
      } ## end function onCategory

    query <- paste("SELECT A.[Database], A.[Table], A.ID_Category, B.Category, A.ID, A.Ticker, ",
                   "A.NumeroValore, A.ID_Currency, C.Moneta AS Currency, A.Frequency, ",
                   "A.[Price adjusted], A.Description ",
                   "FROM DBTimeseries A INNER JOIN DBCategory_Timeseries B ON ",
                   "A.ID_Category = B.ID INNER JOIN [Sistema (prova)].dbo.DBMoneta C ON A.ID_Currency = C.ID ",
                   "ORDER BY B.Category, A.Ticker",
                   sep=""
                  )

    data.timeseries <- get.table(odbcCon, query=query)
    visualizeColumns <- rep(FALSE,dim(data.timeseries)[2])
    names(visualizeColumns) <- colnames(data.timeseries)


    ## select the default columns to be visualized
    visualizeColumns["Category"] <- TRUE
    visualizeColumns["ID"] <- TRUE
    visualizeColumns["Ticker"] <- TRUE
    visualizeColumns["NumeroValore"] <- TRUE
    
    # create the first labelframe containing the Data sources
    lf.timeseries <- create_labelFrame(parent=topWindow,text="Timeseries selection")

    # create the combo box using the data in the following query
    query <- "SELECT ID, Category FROM DBCategory_Timeseries"
    df.category <- get.table(odbcCon, query=query)
    rownames(df.category) <- df.category[,"Category"]

    f.category <- tkframe(parent=lf.timeseries[["labelFrame"]])
    l.category <- create_label(parent=f.category,value="Time series category:")
    cb.category <- create_combo(parent=f.category,
                                editable=FALSE,values=df.category[,"Category"],
                                          modifyCommand=onCategory
                                         )
    ## create the check buttons to visualize the columns
    f.show <- tkframe(parent=lf.timeseries[["labelFrame"]])
    ckb.showDatabase <- create_checkButton(parent=f.show,label="Database",command=onShowDatabase)
    ckb.showTable <- create_checkButton(parent=f.show,label="Table",command=onShowTable)
    ckb.showCurrency <- create_checkButton(parent=f.show,label="Currency",command=onShowCurrency)
    ckb.showFrequency <- create_checkButton(parent=f.show,label="Frequency",command=onShowFrequency)
    ckb.showPriceAdj <- create_checkButton(parent=f.show,label="Price adjusted",command=onShowPriceAdj)
    ckb.showDescription <- create_checkButton(parent=f.show,label="Description",command=onShowDescription)


    ## create the tablelist
    tbl.data <- create_tablelist(parent=lf.timeseries[["labelFrame"]],dataFrame=data.timeseries[FALSE,],width=120,
                      height=16,withScrollBarX=TRUE,withScrollBarY=TRUE)
    tbl.data[["hidecolumns"]](hide=1,names(visualizeColumns)[!visualizeColumns])

    # INSERT OF THE CREATED WIDGETS
    # insert objects in the labelframe dataSources
    tkgrid(f.category,columnspan=2,sticky="w")
    tkgrid(l.category[["label"]],cb.category[["combo"]],padx=padx,pady=pady,sticky="w")
    tkgrid(f.show,padx=padx,pady=pady,sticky="w")

    tkgrid(ckb.showDatabase[["frame"]],ckb.showTable[["frame"]],ckb.showCurrency[["frame"]],
           ckb.showFrequency[["frame"]],ckb.showPriceAdj[["frame"]],ckb.showDescription[["frame"]],
           padx=padx,pady=pady,sticky="ew")
    tkgrid(tbl.data[["frame"]],padx=padx,pady=pady,columnspan=2)

    return(list(lf.timeseries=lf.timeseries))
  }


create_newTableWidget <- function(myConnection,database,newTable)
  {
    ## define the data.frame containing the fields attributes
    df.structure <- data.frame("","","",FALSE,FALSE)
    dimnames(df.structure)[[2]] <- c("Column Name","Data Type","Length","Allow Nulls","Primary Key")
    
    topWindow <- tktoplevel()
    tktitle(topWindow) <- "New table"
    if (newTable == "" | is.null(newTable))
      {
        tkmessageBox(message="Invalid table name!",icon="error",type="ok",parent=topWindow)
        tkdestroy(topWindow)
        return()
      }
    
    tkgrab(topWindow)
    
    tbl.attributes <- create_tablelist(parent=topWindow,dataFrame=df.structure,width=120,
                      height=16,withScrollBarX=TRUE,withScrollBarY=TRUE,
                      colFormats=c(rep("no",3),rep("logical",2)),
                      editable=c(rep("Entry",3),rep("checkbutton",2)))
    
    f.buttons <- tkframe(parent=topWindow)
    onAdd <- function()
      {
        tbl.attributes[["add.rows"]](newRows=data.frame("","","",FALSE,FALSE))
      }
    onImport <- function()
      {
        result <- select.databaseAndTable(odbcCon)
        if (is.null(result)) return()
        
        tkfocus(topWindow)
        result <- select.fieldsFromTable(myConnection=odbcCon,
                  selectedDatabase=result["Database"],selectedTable=result["Table"])
        if (!is.null(result))
          {
            tbl.attributes[["add.rows"]](newRows=result) 
          }
      }
    onRemove <- function()
      {
        index <- tbl.attributes[["get.selection.rowNumber"]]()
        if (length(index)>0) tbl.attributes[["remove.rows"]](index=index)
      }
    b.add <- create_button(parent=f.buttons,text="Add",command=onAdd)
    b.import <- create_button(parent=f.buttons,text="Import",command=onImport)
    b.remove <- create_button(parent=f.buttons,text="Remove",command=onRemove)
    
    ## The Ok or Cancel buttons and corresponding functions
    onCancel <- function(){tkdestroy(topWindow);return()}
    onOk <- function(){tbl.attributes[["finishediting"]]();return()}
    okCancel <- create_okCancelButton(parent=topWindow,onOk=onOk,onCancel=onCancel)
                      
    ## create the frame containing the input fields
    tkgrid(tbl.attributes[["frame"]],f.buttons,padx=padx,pady=pady)
    tkgrid(b.add[["button"]],padx=padx,pady=pady)
    tkgrid(b.import[["button"]],padx=padx,pady=pady)
    tkgrid(b.remove[["button"]],padx=padx,pady=pady)
    tkgrid(okCancel,padx=padx,pady=pady,columnspan=2)
    
    return()
  } # end  create_newTableWidget

# create_newTableWidget(myConnection=odbcCon,database="risk")

create_newTimeSeriesWidget <- function()
  {
  
      ## function to create a new table
    newTable <- function()
      {
        a <- sqlColumnsInfo(odbcCon, "DBTimeseries",columnNames=c("COLUMN_NAME","TYPE_NAME","COLUMN_SIZE"))
      }

    ## function getInput used to validate the user input
    getInput <- function()
      {
        inputValues <- list()

        ## verify the database
        tmp <- cb.database[["get.selection"]]()
        if (tmp != "")
          {
            inputValues[["Database"]] <- tmp
          }
        else
          {
            tkmessageBox(message="Enter or select a valid database!",icon="error",type="ok",parent=parent)
            return()
          }


        ## verify the table
        tmp <- cb.table[["get.selection"]]()
        if (tmp != "")
          {
            inputValues[["Table"]] <- tmp
          }
        else
          {
            tkmessageBox(message="Enter or select a valid table!",icon="error",type="ok",parent=parent)
            return()
          }


        ## verify the Category
        tmp <- cb.category[["get.foreignKey"]]()

        if (is.null(tmp))
          {
            tkmessageBox(message="Select the Category!",icon="error",type="ok",parent=parent)
            return()
          }
        inputValues[["ID_Category"]] <- tmp

        ## verify the ID
        tmp <- e.id[["get.value"]]()
        if (tmp == "")
          {
            tkmessageBox(message="Enter a valid ID!",icon="error",type="ok",parent=parent)
            return()
          }
        inputValues[["ID"]] <- tmp

        inputValues[["Ticker"]] <- e.ticker[["get.value"]]()
        inputValues[["numeroValore"]] <- e.numeroValore[["get.value"]]()

        ## Verify the ID_Currency
        tmp <- cb.currency[["get.foreignKey"]]()
        if (is.null(tmp))
          {
            tkmessageBox(message="Enter a valid currency!",icon="error",type="ok",parent=parent)
            return()
          }
        inputValues[["ID_Currency"]] <- tmp

        ## Verify the Frequency
        tmp <- cb.frequency[["get.selection"]]()
        if (tmp == "")
          {
            tkmessageBox(message="Enter a valid frequency!",icon="error",type="ok",parent=parent)
            return()
          }
        inputValues[["Frequency"]] <- tmp

        inputValues[["PriceAdusted"]] <- ckb.priceAdjusted[["get.value"]]()
        inputValues[["Description"]] <- txt.description[["get.value"]]()
print(inputValues)
      }

    onDefineStructureTable <- function()
      {
        ## check for a valid database
        database = cb.database[["get.selection"]]()
        if (database == "")
          { 
            tkmessageBox(message="Select a database first!",icon="error",type="ok",parent=topWindow)
            return()
          }
        if (cb.table[["get.id"]]() == 0 & cb.table[["get.selection"]]() != "")
          {         
            ## call the widget
            create_newTableWidget(myConnection=odbcCon,database="risk",newTable=cb.table[["get.selection"]]())
          }
        else
          {
            tkmessageBox(message="Empty name or table still exists!",icon="error",type="ok",parent=topWindow)
          }
        return() 
      }

    
    topWindow <- tktoplevel() 
    tktitle(topWindow) <- "Create new timeseries"
    tkfocus(topWindow)
    tkgrab(topWindow)
    
    ## create the main frame containing all widget
    f.fields <- tkframe(parent=topWindow)

    ## two labels used as headers for the columns
    l.new <- create_label(parent=f.fields,value="New Values",font="{courier} 12")

    ## get the Category
    query <- "SELECT ID, Category FROM DBCategory_Timeseries ORDER BY Category"
    df.categories <- get.table(odbcCon,query=query)

    ## get the Currency
    query <- "SELECT ID, Moneta AS Currency FROM [Sistema (prova)].dbo.DBMoneta ORDER BY Moneta"
    df.currencies <- get.table(odbcCon,query=query)

    ## get the Database and tables
    query <- "SELECT [Database], [Table], ID_Category, ID FROM DBTimeseries ORDER BY [Database],[Table]"
    df.timeSeries <- get.table(odbcCon,query=query)

    
    ## a line is compose by 3 widget: label, entry and combo
    #l.database <- create_label(parent=f.fields,value="Time series category:")
    #e.database <- create_entry(parent=f.fields,value="",width="6")
    #cb.database <- create_combo(parent=f.fields,editable=FALSE,values=df.category[,"Category"],modifyCommand=onCategory)

    ## database field
    l.database <- create_label(parent=f.fields,value="Database:")
    databases <- unique(df.timeSeries[,"Database"])
    cb.database <- create_combo(parent=f.fields,values=databases)
    b.newDatabase <- create_button(parent=f.fields,text="New database",command=pippo)
    
    ## table field
    l.table <- create_label(parent=f.fields,value="Table:")
    tables <- unique(df.timeSeries[,"Table"])
    tables <- tables[order(tables)]
    cb.table <- create_combo(parent=f.fields,values=tables)
    b.defineTable <- create_button(parent=f.fields,text="Define structure",command=onDefineStructureTable)

    ## category field
    l.category <- create_label(parent=f.fields,value="Category:")
    cb.category <- create_combo(parent=f.fields,editable=FALSE,values=df.categories[,"Category"],foreignKey=df.categories[,"ID"])

    ## id field
    onAutomaticId <- function()
      {
        if (ckb.id[["get.value"]]()=="0") return()
        if (cb.category[["get.selection"]]()=="")
          {
            tkmessageBox(message="First select the category!",type="ok",icon="info",parent=parent)
            ckb.id[["set.value"]]("0")
            return()
          }
      }
    l.id <- create_label(parent=f.fields,value="ID:")
    f.id <- tkframe(f.fields)
    e.id <- create_entry(parent=f.id,value="",width="12")
    ckb.id <- create_checkButton(parent=f.id,label="Automatic",command=onAutomaticId)


    ## ticker field
    l.ticker <- create_label(parent=f.fields,value="Ticker:")
    e.ticker <- create_entry(parent=f.fields,value="",width="25")

    ## numeroValore field
    l.numeroValore <- create_label(parent=f.fields,value="Numero Valore:")
    e.numeroValore <- create_entry(parent=f.fields,value="",width="25")

    ## currency field
    l.currency <- create_label(parent=f.fields,value="Currency:")
    cb.currency <- create_combo(parent=f.fields,editable=FALSE,values=df.currencies[,"Currency"],foreignKey=df.currencies[,"ID"])

    ## Frequency field
    l.frequency <- create_label(parent=f.fields,value="Frequency:")
    cb.frequency <- create_combo(parent=f.fields,editable=FALSE,values=c("d","w","m"))

    ## PriceAdjusted field
    l.priceAdjusted <- create_label(parent=f.fields,value="Price Adjusted:")
    ckb.priceAdjusted <- create_checkButton(parent=f.fields,label="")

    ## Description field
    l.description <- create_label(parent=f.fields,value="Description:")
    txt.description <- create_textwindow(parent=f.fields,state="normal",height=4,width=30,
                                         font="{MS Sans Serif} 8")

    ## The Ok or Cancel buttons and corresponding functions
    onCancel <- function(){tkdestroy(topWindow);return()}
    onOk <- function(){getInput();return()}
    okCancel <- create_okCancelButton(parent=f.fields,onOk=onOk,onCancel=onCancel)

   ## Insert into the widget
   tkgrid(l.new[["label"]],column=1)
   tkgrid(l.database[["label"]],cb.database[["combo"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.table[["label"]],cb.table[["combo"]],b.defineTable[["button"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.category[["label"]],cb.category[["combo"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.id[["label"]],f.id,padx=padx,pady=pady,sticky="w")
      tkgrid(e.id[["entry"]],ckb.id[["frame"]],padx=c(0,padx),pady=pady,sticky="w")
   tkgrid(l.ticker[["label"]],e.ticker[["entry"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.numeroValore[["label"]],e.numeroValore[["entry"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.currency[["label"]],cb.currency[["combo"]],padx=padx,pady=pady,sticky="w")
   tkgrid(l.frequency[["label"]],cb.frequency[["combo"]],padx=padx,pady=pady,sticky="w")

   tkgrid(l.priceAdjusted[["label"]],ckb.priceAdjusted[["frame"]],padx=padx,pady=pady,sticky="w")


   tkgrid(l.description[["label"]],txt.description[["frame"]],padx=padx,pady=pady,sticky="w")
   tkgrid(okCancel,pady=pady,columnspan=3)
   tkgrid(f.fields)
   
   tkwait.window(topWindow)
   return()
  }  ## end of create_newTimeSeriesWidget




select.databaseAndTable <- function(odbcCon)
  {
    result <- NULL
    ## get the databases
    df.databases <- sqlDatabases(odbcCon)
    dbSpace <- grep(" ",df.databases[,"Database"])
    databases <- df.databases[,"Database"]
    if (length(dbSpace)>0) databases[dbSpace] <- paste("[",databases[dbSpace],"]",sep="")

    ## define the function used on modifyCommand onDatabase
    onDatabase <- function()
      {
        database <- cb.database[["get.selection"]]()
        if (database == "")
          {
            tkmessageBox(message="Wrong database!",icon="error",type="ok",parent=topWindow)
            return()
          }
        df.tables <<- sqlTablesInfo(myConnection=odbcCon,database=database)
        ## construct the name of the database with Table owner, include [] if necessary
        ownerSpace <- grep(" ",df.tables[,"Table owner"])
        tableSpace <- grep(" ",df.tables[,"Table name"])
        owners <- df.tables[,"Table owner"]
        if (length(ownerSpace)>0) owners[ownerSpace] <- paste("[",owners[ownerSpace],"]",sep="")
        tables <- df.tables[,"Table name"]
        if (length(tableSpace)>0) tables[tableSpace] <- paste("[",tables[tableSpace],"]",sep="")

        cb.table[["modify.values"]](paste(owners,".",tables,sep=""))
        return()
      }

    topWindow <- tktoplevel()
    tktitle(topWindow) <- "Database and table selection"
    tkfocus(topWindow)
    tkgrab(topWindow)
    
    l.database <- tklabel(parent=topWindow,text="Database:")
    cb.database <- create_combo(parent=topWindow,editable=FALSE,values=databases,
                                modifyCommand=onDatabase)

    l.table <- tklabel(parent=topWindow,text="Table:")
    cb.table <- create_combo(parent=topWindow,editable=FALSE)

    ## The Ok or Cancel buttons and corresponding functions
    onCancel <- function(){tkdestroy(topWindow);return()}
    onOk <- function()
      {
        id <- cb.table[["get.id"]]()
        if (id == 0) return()

        tmp <- c(cb.database[["get.selection"]](),df.tables[id,"Table owner"],df.tables[id,"Table name"])
        names(tmp) <- c("Database","Owner","Table")
        result <<- tmp
        tkdestroy(topWindow)
        return()
      }
    okCancel <- create_okCancelButton(parent=topWindow,onOk=onOk,onCancel=onCancel)

    tkgrid(l.database,cb.database[["combo"]],padx=padx,pady=pady,sticky="w")
    tkgrid(l.table,cb.table[["combo"]],padx=padx,pady=pady,sticky="w")
    tkgrid(okCancel,pady=pady,columnspan=2)

    tkwait.window(topWindow)
    return(result)
  }


select.fieldsFromTable <- function(myConnection,selectedDatabase,selectedTable)
  {
    df.structure <- get.tableStructure(myConnection,databaseName=selectedDatabase,
                                            tableName=selectedTable)
    structureNames <- dimnames(df.structure)[[2]]
    nbColumns <- dim(df.structure)[2]

    df.structure[["Import"]] <- rep("0",dim(df.structure)[1])

    topWindow <- tktoplevel()
    tktitle(topWindow) <- "Field selection"
    tkfocus(topWindow)
    tkgrab(topWindow)
    
    tmp <- paste("Database - ",selectedDatabase,"\nTable - ",selectedTable,sep="")
    l.dbAndTable <- create_label(parent=topWindow,value=tmp,font="{MS Sans Serif} 12 bold")
    tbl.structure <- create_tablelist(parent=topWindow,dataFrame=df.structure,width=120,
                      height=16,withScrollBarX=TRUE,withScrollBarY=TRUE,
                      colFormats=c(rep("no",3),rep("logical",3)),
                      editable=c(rep("Entry",3),rep("checkbutton",3)))

    ## The Ok or Cancel buttons and corresponding functions
    onCancel <- function(){tkdestroy(topWindow);return()}
    onOk <- function()
      {
        tbl.structure[["finishediting"]]()
        selection <- tbl.structure[["get.table"]]()
        isDesired <- selection[,"Import"] == "1"
        if (!any(isDesired)) {tkdestroy(topWindow);return()}
        selection <- selection[isDesired,]
        selection <<- selection[structureNames]
        tkdestroy(topWindow)
        return()
      }
    okCancel <- create_okCancelButton(parent=topWindow,onOk=onOk,onCancel=onCancel)

    tkgrid(l.dbAndTable[["label"]],padx=padx,pady=pady)
    tkgrid(tbl.structure[["frame"]],padx=padx,pady=pady)
    tkgrid(okCancel,padx=padx,pady=pady)
    selection <- data.frame(character(0))
    selection <- NULL
    tkwait.window(topWindow)
    return(selection)
  }

#select.fieldsFromTable(myConnection=odbcCon,selectedDatabase="risk",selectedTable="DBTimeseries")


