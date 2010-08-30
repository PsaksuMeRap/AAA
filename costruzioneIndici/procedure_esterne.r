availableStatistics=c("Frequency",
"Nb. Obs.","Start date","End date",
"Min","Min at",
"Max","Max at",
"Mean","Annualized mean",
"Median","1st Qu.","3st Qu.",
"Stdev","Annualized stdev",
"Skewness","Excess kurtosis",
"Drawdown","Max Drawdown","Max Drawdown %",
"Sortino ratio","Bias ratio"
)


desiredLevels=c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,
FALSE,FALSE)
desiredReturns=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
statistics <- data.frame(available=I(availableStatistics),desiredLevels=desiredLevels,desiredReturns=desiredReturns)
rm(availableStatistics,desiredLevels,desiredReturns)


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

   ## crea il data.frame dei rendimenti
   nbObsReturns <<- length( dates_orig[-1])
   tsLogReturns.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObsReturns,])
   tsPercReturns.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObsReturns,]) / ts.df_orig[1:nbObsReturns,]
}


annualizeFactor <- function(frequency) {
  if (frequency == "Daily") return(sqrt(252))
  if (frequency == "Weekly") return(sqrt(52))
  if (frequency == "Monthly") return(sqrt(12))
  if (frequency == "Quarterly") return(2.0)
  if (frequency == "Yearly") return(1.0)
  if (frequency == "Irregular") return(1.0)
}


computeStatistics <- function(kind="Levels") {
  ## kind puo' essere "Levels", "LogReturns", "PercReturns"
  ## cambia per avere i rendimenti percentuali !!
  if (kind=="PercReturns") return(NULL)
  if (kind=="Levels") desired = "desiredLevels" else desired = "desiredReturns"

  desiredStatistics <- statistics[statistics[,desired],"available"]
  nbDesiredStatistics <- length(desiredStatistics)
  v.selectedSeries <- colnames(ts.df_orig)
  nbSelected <- length(v.selectedSeries)

  if (nbSelected == 0 | nbDesiredStatistics == 0) return(NULL)

  plotType = r.tsType[["get.selection"]]()
  if ((plotType == "Levels") & (kind=="Levels")) {
    v.range <- plotFrom:plotTo
    dates <- dates_orig
  }
  if ((plotType == "Levels") & (kind != "Levels")) {
    if (plotFrom == 1) tmpFrom = 1 else tmpFrom = plotFrom - 1
    tmpTo=max(1,plotTo-1)
    v.range <- tmpFrom:tmpTo
    dates <- dates_orig[-1]
  }
  if ((plotType != "Levels") & (kind == "Levels")) {
    tmpTo = plotTo+1
    v.range <- plotFrom:tmpTo
    dates <- dates_orig
  }
  if ((plotType != "Levels") & (kind != "Levels")) {
    v.range <- plotFrom:plotTo
    dates <- dates
  }

  if (kind=="Levels") {
    tsToAnalyse.df <- ts.df_orig[v.range,v.selectedSeries,drop=FALSE]
  } else {
    if (kind=="LogReturns") {
      tsToAnalyse.df <- tsLogReturns.df[v.range,v.selectedSeries,drop=FALSE]
    } else {
      tsToAnalyse.df <- tsPercReturns.df[v.range,v.selectedSeries,drop=FALSE]
    }
  }

  v.dates <- dates[v.range]
  nbObs <- length(v.range)
  rm(v.range)
  drawDown <- function(x,T=length(x)) {
    lth <- length(x)
    if (lth<2) return(NA_real_)
    if (T > lth) return(NA_real_)
    if (T<2) return(NA_real_)
    return(max(0,max(x[1:(T-1)],na.rm=TRUE)-x[T],na.rm=TRUE))
  }
  maxDrawDown <- function(x,T=length(x),percentage=FALSE) {
    lth <- length(x)
    if (lth<2) return(NA_real_)
    if (T > lth) return(NA_real_)
    if (T<2) return(NA_real_)
    DD <- rep(NA_real_,T)
    peak <- -Inf
    validIndices <- !is.na(x[1:T])

    i <- 1
    for (y in x[validIndices]) {
      if (y > peak) peak <- y
      if (percentage) DD[i] = 100 * (peak - y) / peak else DD[i] = peak - y
      i = i + 1
    }
    return(max(DD,na.rm=TRUE))
  }
  sortinoRatio <- function(x,mar) {
    ## x is a vector of numbers (returns)
    ## mar is a real, the minimum acceptable return
    na <- is.na(x)
    if (all(na)) return (NA_real_)
    availableX <- x[!na]
    tmp <- sum((availableX[availableX < mar] - mar)^2)/length(availableX)
    tmp <- sqrt(tmp)
    r_mean <- mean(availableX)
    return((r_mean - mar)/tmp)
  }

  biasRatio <- function(x) {
    ## this function computes the bias ratio defined as 
    ##     number r_t in [0,sigma]
    ##     ------------------------
    ##     number r_t in [-sigma,0)
    na <- is.na(x)
    if (all(na)) return (NA_real_)
    availableX <- x[!na]
    sigma <- sqrt(var(availableX))
    inZeroSigma <- availableX >= 0 & availableX <= sigma
    inMenoSigmaZero <- availableX >= -sigma & availableX < 0
    return (sum(inZeroSigma)/(1+sum(inMenoSigmaZero)))
  }


  ## crea la matrice TRUE/FALSE per sapere se l'osservazione è NA
  notNA.m <- matrix(sapply(tsToAnalyse.df,is.na),nrow=nrow(tsToAnalyse.df))
  notNA.m <- !notNA.m
  
  ## crea i vettori di NA numerici e di stringa
  numericNA.v <- rep(NA_real_,ncol(tsToAnalyse.df))
  characterNA.v <- rep(NA_character_,ncol(tsToAnalyse.df))

  ## assegna i nomi dei fondi ad alcune variabili
  nomiFondi <- colnames(tsToAnalyse.df)
  colnames(notNA.m) <- nomiFondi
  names(numericNA.v) <- nomiFondi
  names(characterNA.v) <- nomiFondi

  ## calcola la costante d'annualizzazione da usare con le varianze e i rendimenti medi
  annualizeConstant <- annualizeFactor(tsFreq)

  computeDesiredStatistics <- function() {
    result <- list()
    v.range <- 1:nbObs

    if (is.element("Frequency",desiredStatistics)) {
      result[["Frequency"]] <- rep(tsFreq,ncol(notNA.m))
    }

    ## determina nonché identifica il numero di serie con almeno una osservazione 
    tmp <- apply(notNA.m,MARGIN=2,FUN=sum)
    serieConOss <- tmp > 0
    anySerieConOss <- any(serieConOss)
    if (is.element("Nb. Obs.",desiredStatistics)) {
      result[["Nb. Obs."]] <- tmp
      result[["Nb. Obs."]] <- paste(result[["Nb. Obs."]], " (",round(result[["Nb. Obs."]]/annualizeConstant^2,digits=2)," Years)",sep="")
    }
    rm(tmp)

    if (is.element("Start date",desiredStatistics)) {
      getStartDate <- function(x,v.dates,v.range) {
        if (any(x)) {
          pos <- min(v.range[x])
          return(as.character(v.dates[pos]))
        } else {
          return(NA_character_)
        }
      }
      pippo <- apply(notNA.m,2,getStartDate,v.dates,v.range)
      result[["Start date"]] <- pippo
      rm(pippo)
    }
    if (is.element("End date",desiredStatistics)) {
      getEndDate <- function(x,v.dates,v.range) {
        if (any(x)) {
          pos <- max(v.range[x])
          return(as.character(v.dates[pos]))
        } else {
          return(NA)
        }
      }
      pippo <- apply(notNA.m,2,getEndDate,v.dates,v.range)
      result[["End date"]] <- pippo
      rm(pippo)
    }
    if (is.element("Min",desiredStatistics) | is.element("Min at",desiredStatistics)) {
      result[["Min"]] <- sapply(tsToAnalyse.df,min,na.rm=TRUE)
      tmp <- unlist(sapply(tsToAnalyse.df,which.min))
      whereReplace <- result[["Min"]] == Inf
      if (any(whereReplace)) tmp[whereReplace] <- NA
      if (any(!whereReplace)) tmp[!whereReplace] <- as.character(v.dates[tmp[!whereReplace]])
      result[["Min at"]] <- tmp
      result[["Min"]][whereReplace] <- NA
      rm(tmp,whereReplace)
    }
    if (is.element("Max",desiredStatistics) | is.element("Max at",desiredStatistics)) {
      result[["Max"]] <- sapply(tsToAnalyse.df,max,na.rm=TRUE)
      tmp <- unlist(sapply(tsToAnalyse.df,which.max))
      whereReplace <- result[["Max"]] == -Inf
      if (any(whereReplace)) tmp[whereReplace] <- NA
      if (any(!whereReplace)) tmp[!whereReplace] <- as.character(v.dates[tmp[!whereReplace]])
      result[["Max at"]] <- tmp
      result[["Max"]][whereReplace] <- NA
      rm(tmp,whereReplace) 
    }
    if (is.element("Mean",desiredStatistics)) {
      tmp <- sapply(tsToAnalyse.df,mean,na.rm=TRUE)
      whereReplace <- is.na(tmp)
      if (any(whereReplace)) tmp[whereReplace] <- NA
      result[["Mean"]] <- tmp
      rm(tmp,whereReplace)
    }
    if (is.element("Annualized mean",desiredStatistics)) {
      tmp <- sapply(tsToAnalyse.df,mean,na.rm=TRUE)
      whereReplace <- is.na(tmp)
      if (any(whereReplace)) tmp[whereReplace] <- NA
      result[["Annualized mean"]] <- tmp*annualizeConstant^2*100
      rm(tmp,whereReplace)
    }
    if (is.element("Median",desiredStatistics)) {
      result[["Median"]] <- sapply(tsToAnalyse.df,median,na.rm=TRUE)
    }
    if (is.element("1st Qu.",desiredStatistics)) {
      result[["1st Qu."]] <- sapply(tsToAnalyse.df,quantile,probs=0.25,na.rm=TRUE)
    }
    if (is.element("3st Qu.",desiredStatistics)) {
      result[["3rd Qu."]] <- sapply(tsToAnalyse.df,quantile,probs=0.75,na.rm=TRUE)
    }
    if (is.element("Stdev",desiredStatistics)) {
      result[["Stdev"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Stdev"]][serieConOss] <- sqrt(sapply(tsToAnalyse.df[serieConOss],var,na.rm=TRUE))
      }
    }
    if (is.element("Annualized stdev",desiredStatistics)) {
      result[["Annualized stdev"]] <- numericNA.v
      if (anySerieConOss) {
        tmp <- sqrt(sapply(tsToAnalyse.df[,serieConOss],var,na.rm=TRUE))
        result[["Annualized stdev"]][serieConOss] <- 100*tmp*annualizeFactor(tsFreq)
        rm(tmp)
      }
    }
    if (is.element("Skewness",desiredStatistics)) {
      result[["Skewness"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Skewness"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss],skewness,na.rm=TRUE,method="moment")
      }
    }
    if (is.element("Excess kurtosis",desiredStatistics)) {
      result[["Excess kurtosis"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Excess kurtosis"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss],kurtosis,na.rm=TRUE,method="excess")
      }
    }
    if (is.element("Drawdown",desiredStatistics)) {
      result[["Drawdown"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Drawdown"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss],drawDown)
      }
    }
    if (is.element("Max Drawdown",desiredStatistics)) {
      result[["Max Drawdown"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Max Drawdown"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss],maxDrawDown)
      }
    }
    if (is.element("Max Drawdown %",desiredStatistics)) {
      result[["Max Drawdown %"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Max Drawdown %"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss],maxDrawDown,percentage=TRUE)
      }
    }
    if (is.element("Sortino ratio",desiredStatistics)) {
      annualizeConstant = annualizeFactor(tsFreq)
      result[["Sortino ratio"]] <- numericNA.v
      if (anySerieConOss) {
        tmp <- sapply(tsToAnalyse.df[,serieConOss], sortinoRatio, mar=as.numeric(chop(entry.minimumAcceptableReturn[["get.value"]]())) / 100 / annualizeConstant^2)
        result[["Sortino ratio"]][serieConOss] <- tmp * annualizeConstant
        rm(tmp)
      }
    }
    if (is.element("Bias ratio",desiredStatistics)) {
      result[["Bias ratio"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Bias ratio"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss], biasRatio)
      }
    }

    return(result)
  }
  return(computeDesiredStatistics())
}


getDateFormat <- function(date) {
  delimiters = "-"
  tmp <- gregexpr("-",date)[[1]]
  if (tmp[1] == -1) {
    tmp <- gregexpr("/",date)[[1]]
    delimiters = "/"
  }
   
  if (tmp[1] == -1) {
      library("tcltk")
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
      library("tcltk")
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
          library("tcltk")
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

  rm(ts.df)


  ## crea il data.frame dei rendimenti
  nbObsReturns <<- length( dates_orig[-1])
  tsLogReturns.df <<- log(ts.df_orig[-1,]) - log(ts.df_orig[1:nbObsReturns,])
  tsPercReturns.df <<- (ts.df_orig[-1,] - ts.df_orig[1:nbObsReturns,]) / ts.df_orig[1:nbObsReturns,]
}
