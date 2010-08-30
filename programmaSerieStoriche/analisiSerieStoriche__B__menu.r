
setTsFrequency <- function(freq="Weekly") {
  availableFreq = c("Daily","Weekly","Monthly","Quarterly","Yearly","Irregular")
  if (missing(freq)) {
	assign("tsFreq",freq,envir=env)

  } else {
    if (is.element(freq,availableFreq)) {
          assign("tsFreq",freq,envir=env)
          return()	
       } else {
	  tkmessageBox(message="The selected frequency is invalid.",icon="error")
          tkmessageBox(message=paste("Valid frequencies: ",paste(availableFreq,collapse=", "),".",sep=""),icon="info")
          return()
       }
  }
}

setTsFrequencyTcl <- function() {
  availableFreq = c("Daily","Weekly","Monthly","Quarterly","Yearly","Irregular")

  onOk <- function() {
    result = r.labelRadio[["get.selection"]]()
    assign("tsFreq",result,envir=env)
    tkdestroy(tw.setTsFrequency)
  }
   
  tw.setTsFrequency <- tktoplevel()
  tktitle(tw.setTsFrequency) <- "Frequency selection"
  tkconfigure(tw.setTsFrequency,background="#D8D8D8") #
  tkgrab(tw.setTsFrequency)
   
  r.labelRadio <- create_radio(parent=tw.setTsFrequency,values=availableFreq,
                               position="v",text=availableFreq)
  b.okCancel <- create_okCancelButton(parent=tw.setTsFrequency,onOk=onOk,onCancel=function() {tkdestroy(tw.setTsFrequency)})
  r.labelRadio[["set.selection"]](tsFreq)

  tkgrid(r.labelRadio[["frame"]],padx=padx,pady=pady)
  tkgrid(b.okCancel,padx=padx,pady=pady)
   
}

getDateFormat <- function(date) {
  delimiters = "-"
  tmp <- gregexpr("-",date)[[1]]
  if (tmp[1] == -1) {
    tmp <- gregexpr("/",date)[[1]]
    delimiters = "/"
  }
   
  if (tmp[1] == -1) {
      tkmessageBox(message="The date has an invalid format!\nOnly '-' or '/' are valid delimiters.",icon="error")
      return	
    }
  part <- substr(date,1,tmp[1]-1)
  part[2] <- substr(date,tmp[1]+1,tmp[2]-1)
  part[3] <- substr(date,tmp[2]+1,nchar(date))
  nbChar <- nchar(part)
  isYear <- nbChar == 4
  if (delimiters == "-") format = "%Y-%m-%d" else format = "%m/%d/%Y"
}

importTimeseries <- function() {
  filename <- tclvalue(tkgetOpenFile(parent=topWindow,defaultextension=".csv",
                                     filetypes=paste("{{Comma separated values files} {.csv}} {{All} {.*}}")))
  if (filename == "") return

  if (!file.exists(filename)) {
      tkmessageBox(message="The selected file does not exists",icon="error")
      return
    }

  ## load the time series. It assumes that the first row contains the date
  fileName <<- filename
  ts.names <- unlist(read.csv(fileName,nrow=1,header=FALSE,colClasses="character",as.is=TRUE)[1,,drop=TRUE])

  ts.df <- read.csv(fileName,colClasses=c("character",rep("numeric",length(ts.names)-1)))
  colnames(ts.df) <- ts.names
  ts.df[["test2"]] = 0
  if(nrow(ts.df)==0) {
       tkmessageBox(message="Zero number of observations.",
           icon="error",type="ok")
     	return()
     }
  ## create the dates vector
  if (is.element("Date",ts.names)) {
      withDates <<- TRUE
      ts.names <- setdiff(ts.names,"Date")
   
      ## determine the date format
      format = getDateFormat(ts.df[["Date"]][1])
   
      if (length(ts.names) != 0) {
          dates <<- as.Date(ts.df[["Date"]],format=format)
          ts.df <<- ts.df[,ts.names]
          dataInizio <<- as.character(min(dates))
          dataFine <<- as.character(max(dates))
          etichettaDataInizio <<- paste("Start:",dataInizio)
          etichettaDataFine <<- paste(  "End:  ",dataFine)
        } else {
	  tkmessageBox(message="No time series available.",
           icon="error",type="ok")
           return()
        }
    } else {
      withDates <<- FALSE
      dates <<- 1:nrow(ts.df)
      ts.df <<- ts.df[,ts.names]
      dataInizio <<- 1
      dataFine <<- length(dates)
      etichettaDataInizio <<- "Prima osservazione: 1"
      etichettaDataFine <<- paste("Ultima osservazione:",dataFine)

    }

  ## crea una copia dei valori originali da usare per il calcolo
  ## dei logaritmi, ecc. ecc.

  dates_orig <<- dates
  ts.df_orig <<- ts.df[,ts.names]


  nbObs <<- length(dates_orig)
  plotFrom <<- 1
  plotTo <<- nbObs

  slb.timeSeries[["reset"]](widget=1)
  slb.timeSeries[["reset"]](widget=2)
  rm(ts.df) ## rimuovi ts.df locale

  slb.timeSeries[["set.values"]](newValues=colnames(ts.df))


  dateStart.l[["set.label"]](value=paste("Start:",dates[1]))
  dateEnd.l[["set.label"]](value=paste("End:",dates[nbObs]))

  r.tsType[["set.selection"]]("Levels")

  ## verifica che non ci siano serie con valori negativi o uguali a 0
  withZeroOrNegativeValues <- function(ts) {
    
    if (any(ts<=0,na.rm=TRUE)) return(TRUE) else return(FALSE)

  }

  problematicSeries <- sapply(ts.df_orig,withZeroOrNegativeValues)
  if (any(problematicSeries)) {
        tkmessageBox(message=paste("Series with zero or neg. values:\n",
           paste(colnames(ts.df)[problematicSeries],collapse=","),sep=""),
           icon="error",type="ok")
  }

  ## crea il data.frame dei rendimenti
  nbObsReturns <<- length( dates_orig[-1])
  tsLogReturns.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObsReturns,])
  tsPercReturns.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObsReturns,]) / ts.df_orig[1:nbObsReturns,]
}

get_objects <- function () {
   oggetti <- ls(env=globalenv())
   isMatrix <- function(name) { eval(parse(text=paste("is.matrix(",name,")")))}
   isMatrix <- sapply(oggetti,isMatrix)
   return(oggetti[isMatrix])
}

importDataMatrixDirect <- function(selection) {
   txt = paste("ts.df <<- as.data.frame(",selection,")")
   eval(parse(text=txt))

   ## create the dates vector
   withDates <<- TRUE
   ts.names <- colnames(ts.df)

   ## determine the date format
   format = getDateFormat(rownames(ts.df)[1])
   
   if (length(ts.names) != 0) {
      dates <<- as.Date(rownames(ts.df),format=format)
      dataInizio <<- as.character(min(dates))
      dataFine <<- as.character(max(dates))
      etichettaDataInizio <<- paste("Start:",dataInizio)
      etichettaDataFine <<- paste(  "End:  ",dataFine)
   } else {
      tkmessageBox(message="No time series available.",
         icon="error",type="ok")
      return()
   }


   ## crea una copia dei valori originali da usare per il calcolo
   ## dei logaritmi, ecc. ecc.

   dates_orig <<- dates
   ts.df_orig <<- ts.df[,ts.names]


   nbObs <<- length(dates_orig)
   plotFrom <<- 1
   plotTo <<- nbObs

   slb.timeSeries[["reset"]](widget=1)
   slb.timeSeries[["reset"]](widget=2)

   slb.timeSeries[["set.values"]](newValues=colnames(ts.df))

   dateStart.l[["set.label"]](value=paste("Start:",dates[1]))
   dateEnd.l[["set.label"]](value=paste("End:",dates[nbObs]))

   r.tsType[["set.selection"]]("Levels")

   ## crea il data.frame dei rendimenti
   nbObsReturns <<- length( dates_orig[-1])
   tsLogReturns.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObsReturns,])
   tsPercReturns.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObsReturns,]) / ts.df_orig[1:nbObsReturns,]
}


importDataMatrix <- function() {
  oggetti <- get_objects()

  if (length(oggetti) == 0) {
      tkmessageBox(message="No data matrix available.",icon="error")
      return
    }
  selectionWindow <- tktoplevel()
  tktitle(selectionWindow) <- "Data matrix selection"
  tkconfigure(selectionWindow,background="#D8D8D8")
  
  onCancel <- function() {
    tkdestroy(selectionWindow)
    return()
  }

  onOk <- function() {
    selection <- selection.lbx[["get.selection"]]()
    if (length(selection)==0) {
      tkmessageBox(message="No data matrix selected.",icon="error")
      tkdestroy(selectionWindow)
      return()
    }
    ## verify the class of the object
    txt = paste("is.matrix(",selection,")")
    if (eval(parse(text=txt)) != TRUE) {
      tkmessageBox(message="The selected item is not a matrix.",icon="error")
      tkdestroy(selectionWindow)
      return()
    } else {
      txt = paste("ts.df <<- as.data.frame(",selection,")")
      eval(parse(text=txt))

      ## create the dates vector
      withDates <<- TRUE
      ts.names <- colnames(ts.df)

      ## determine the date format
      format = getDateFormat(rownames(ts.df)[1])
   
      if (length(ts.names) != 0) {
          dates <<- as.Date(rownames(ts.df),format=format)
          dataInizio <<- as.character(min(dates))
          dataFine <<- as.character(max(dates))
          etichettaDataInizio <<- paste("Start:",dataInizio)
          etichettaDataFine <<- paste(  "End:  ",dataFine)
      } else {
	  tkmessageBox(message="No time series available.",
           icon="error",type="ok")
           return()
      }


      ## crea una copia dei valori originali da usare per il calcolo
      ## dei logaritmi, ecc. ecc.

      dates_orig <<- dates
      ts.df_orig <<- ts.df[,ts.names]


      nbObs <<- length(dates_orig)
      plotFrom <<- 1
      plotTo <<- nbObs

      slb.timeSeries[["reset"]](widget=1)
      slb.timeSeries[["reset"]](widget=2)

      slb.timeSeries[["set.values"]](newValues=colnames(ts.df))

      dateStart.l[["set.label"]](value=paste("Start:",dates[1]))
      dateEnd.l[["set.label"]](value=paste("End:",dates[nbObs]))

      r.tsType[["set.selection"]]("Levels")

      ## crea il data.frame dei rendimenti
      nbObsReturns <<- length( dates_orig[-1])
      tsLogReturns.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObsReturns,])
      tsPercReturns.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObsReturns,]) / ts.df_orig[1:nbObsReturns,]
      tkdestroy(selectionWindow)
      return()
    } ## end if (eval(parse(text=txt)) != TRUE)
  } ## end on ok function

  selection.lbx <- create_listbox(parent=selectionWindow,values=oggetti,
                           height=20,width=60,order=TRUE,withScrollBarY=TRUE)

  okCancel.b <- create_okCancelButton(parent=selectionWindow,onOk=onOk,onCancel=onCancel)

  tkgrid(selection.lbx[["frame"]])
  tkgrid(okCancel.b)
}


## for (nome in ls()) {txt = paste("is.matrix(",nome,")");print(eval(parse(text=txt)))}

## crea il menu principale (la barra)
topMenu <- create_topmenu(topWindow)
## crea il menu contententi le voci di File
menuFile <- create_menu(topMenu[["menu"]])
menuFile[["addCommand"]]("Exit",command=function(){tkdestroy(topWindow)})

## crea il menu contententi le voci di Time Series
menuTimeseries <- create_menu(topMenu[["menu"]])
menuTimeseries[["addCommand"]]("Data matrix",command=importDataMatrix)
menuTimeseries[["addCommand"]]("Import data",command=importTimeseries)
menuTimeseries[["addCommand"]]("Set frequency",command=setTsFrequencyTcl)


topMenu[["addCascadeMenu"]](menu=menuFile[["menu"]],label="File")
topMenu[["addCascadeMenu"]](menu=menuTimeseries[["menu"]],label="Time series")

