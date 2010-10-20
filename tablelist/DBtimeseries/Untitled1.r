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

    update.data <- function()
      {
        query <- paste("SELECT A.[Database], A.[Table], A.ID_Category, B.Category, A.ID, A.Ticker, ",
                   "A.NumeroValore, A.ID_Currency, C.Moneta AS Currency, A.Frequency, ",
                   "A.[Price adjusted], A.Description ",
                   "FROM DBTimeseries A INNER JOIN DBCategory_Timeseries B ON ",
                   "A.ID_Category = B.ID INNER JOIN [Sistema (prova)].dbo.DBMoneta C ON A.ID_Currency = C.ID ",
                   "ORDER BY B.Category, A.Ticker",
                   sep=""
                  )
        data.timeseries.format = c(rep("character",2),"integer","character","integer",
                                   rep("character",2),"integer",rep("character",2),
                                   "logical","character")
        data.timeseries <- get.table(odbcCon, query=query,as.is=c(TRUE,TRUE,FALSE,TRUE,FALSE,
                                                                  TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE))
        data.timeseries["Price adjusted"] <- as.logical(data.timeseries[["Price adjusted"]])
        data.timeseries <<- data.timeseries
      }
    
    update.data()
    visualizeColumns <- rep(FALSE,dim(data.timeseries)[2])
    names(visualizeColumns) <- colnames(data.timeseries)

    ## select the default columns to be visualized
    visualizeColumns["Category"] <- TRUE
    visualizeColumns["ID"] <- TRUE
    visualizeColumns["Ticker"] <- TRUE
    visualizeColumns["NumeroValore"] <- TRUE

    # create the first labelframe containing the Data sources
    lf.timeseries <- create_labelFrame(parent=topWindow,text="Timeseries selection")

    # create the function used to refresh the tablelist after a removal or insertion
    refresh <- function()
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
                       height=16,withScrollBarX=TRUE,withScrollBarY=TRUE,colFormats=c(rep("no",10),"logical","no"))
        tbl.data[["hidecolumns"]](hide=as.numeric(!visualizeColumns),names(visualizeColumns))
        tkgrid(tbl.data[["frame"]],padx=padx,pady=pady,columnspan=2)
        return()
      } ## end function lf.timeseries[["refresh"]]

    # create the combo box using the data in the following query
    query <- "SELECT ID, Category FROM DBCategory_Timeseries ORDER BY Category"
    df.category <- get.table(odbcCon, query=query)
    rownames(df.category) <- df.category[,"Category"]

    f.category <- tkframe(parent=lf.timeseries[["labelFrame"]])
    l.category <- create_label(parent=f.category,value="Time series category:")
    cb.category <- create_combo(parent=f.category,
                                editable=FALSE,values=df.category[,"Category"],
                                          modifyCommand=refresh
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
    get.table <- function()
      {
        return(tbl.data[["get.table"]]())
      }
    get.selection <- function()
      {
        selection.rows <- tbl.data[["get.selection.rowNumber"]](adjust=FALSE)
        return(tbl.data[["get.row"]](i=selection.rows,adjust=FALSE))
      }
    return(list(lf.timeseries=lf.timeseries,refresh=refresh,get.selection=get.selection,
                get.table=get.table,update.data=update.data))
  }   ## end create_timeSeriesWidget