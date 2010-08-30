

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
  v.selectedSeries <- slb.timeSeries[["get.selected"]]()
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
        tmp <- sqrt(sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],var,na.rm=TRUE))
        result[["Annualized stdev"]][serieConOss] <- 100*tmp*annualizeFactor(tsFreq)
        rm(tmp)
      }
    }
    if (is.element("Skewness",desiredStatistics)) {
      result[["Skewness"]] <- numericNA.v

      if (anySerieConOss) {
        result[["Skewness"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],skewness,na.rm=TRUE,method="moment")
      }
    }
    if (is.element("Excess kurtosis",desiredStatistics)) {
      result[["Excess kurtosis"]] <- numericNA.v

      if (anySerieConOss) {
        result[["Excess kurtosis"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],kurtosis,na.rm=TRUE,method="excess")
      }
    }
    if (is.element("Drawdown",desiredStatistics)) {
      result[["Drawdown"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Drawdown"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],drawDown)
      }
    }
    if (is.element("Max Drawdown",desiredStatistics)) {
      result[["Max Drawdown"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Max Drawdown"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],maxDrawDown)
      }
    }
    if (is.element("Max Drawdown %",desiredStatistics)) {
      result[["Max Drawdown %"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Max Drawdown %"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE],maxDrawDown,percentage=TRUE)
      }
    }
    if (is.element("Sortino ratio",desiredStatistics)) {
      annualizeConstant = annualizeFactor(tsFreq)
      result[["Sortino ratio"]] <- numericNA.v
      if (anySerieConOss) {
        tmp <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE], sortinoRatio, mar=as.numeric(chop(entry.minimumAcceptableReturn[["get.value"]]())) / 100 / annualizeConstant^2)
        result[["Sortino ratio"]][serieConOss] <- tmp * annualizeConstant
        rm(tmp)
      }
    }
    if (is.element("Bias ratio",desiredStatistics)) {
      result[["Bias ratio"]] <- numericNA.v
      if (anySerieConOss) {
        result[["Bias ratio"]][serieConOss] <- sapply(tsToAnalyse.df[,serieConOss,drop=FALSE], biasRatio)
      }
    }

    return(result)
  }
  return(computeDesiredStatistics())
}


printStatistics <- function(v.selectedSeries,x,y,z) {
  tsNames <- v.selectedSeries
  if (!is.null(x)) {
    statisticNames_x <- colnames(x)
    ## compute which fields do not require a rounding
    noChange_x <- is.element(statisticNames_x,c("Frequency","Nb. Obs.","Start date","End date","Min at",
                                                "Max at"))
    change_x <- !noChange_x

    ## add the semicolon to the names
    rowlabels_x <- paste(statisticNames_x,": ....................................",sep="")
  }

  if (!is.null(y)) {
    statisticNames_y <- colnames(y)
    ## compute which fields do not require a rounding
    noChange_y <- is.element(statisticNames_y,c("Frequency","Nb. Obs.","Start date","End date","Min at",
                                                "Max at"))
    change_y <- !noChange_y

    ## add the semicolon to the names
    rowlabels_y <- paste(statisticNames_y,": ....................................",sep="")
  }

  if (!is.null(z)) {
    statisticNames_z <- colnames(z)
    ## compute which fields do not require a rounding
    noChange_z <- is.element(statisticNames_z,c("Frequency","Nb. Obs.","Start date","End date","Min at",
                                                "Max at"))
    change_z <- !noChange_z

    ## add the semicolon to the names
    rowlabels_z <- paste(statisticNames_z,": ....................................",sep="")
  }

   
  ## porta i nomi delle statistiche alla medesima lunghezza
  lmax <- 30
  emptyString <- paste(rep(" ",lmax),collapse="")
  if (!is.null(x)) rowlabels_x <- substr(paste(rowlabels_x,emptyString,sep=""),1,lmax)
  if (!is.null(y)) rowlabels_y <- substr(paste(rowlabels_y,emptyString,sep=""),1,lmax)
  if (!is.null(z)) rowlabels_z <- substr(paste(rowlabels_z,emptyString,sep=""),1,lmax)

  for (tsName in tsNames){
    report.txt[["insert"]](text=paste("\n\n\n",tsName,"\n",sep=""),tag="nomeSerie")
    if (!is.null(x)) {
      report.txt[["insert"]](text="\nLevels:\n")
      values_x <- x[tsName,]
      values_x[,change_x] <- round(x[tsName,change_x],digits=4)
      if (is.element("Annualized mean",statisticNames_x)) values_x[,"Annualized mean"] <- paste(values_x[,"Annualized mean"],"%",sep="")
      if (is.element("Annualized stdev",statisticNames_x)) values_x[,"Annualized stdev"] <- paste(values_x[,"Annualized stdev"],"%",sep="")
      if (is.element("Max Drawdown %",statisticNames_x)) values_x[,"Max Drawdown %"] <- paste(values_x[,"Max Drawdown %"],"%",sep="")
      if (is.element("Sortino ratio",statisticNames_x)) values_x[,"Sortino ratio"] <- paste(values_x[,"Sortino ratio"],"%",sep="")
      tmp <- paste(rowlabels_x,values_x[tsName,],sep=" ")
      report.txt[["insert"]](text=paste(tmp,collapse="\n"))
    }
    if (!is.null(y)) {
      report.txt[["insert"]](text="\n\nLogarithmic returns:\n")
      values_y <- y[tsName,]
      values_y[,change_y] <- round(y[tsName,change_y],digits=4)
      if (is.element("Annualized mean",statisticNames_y)) values_y[,"Annualized mean"] <- paste(values_y[,"Annualized mean"],"%",sep="")
      if (is.element("Annualized stdev",statisticNames_y)) values_y[,"Annualized stdev"] <- paste(values_y[,"Annualized stdev"],"%",sep="")
      if (is.element("Max Drawdown %",statisticNames_y)) values_y[,"Max Drawdown %"] <- paste(values_y[,"Max Drawdown %"],"%",sep="")
      if (is.element("Sortino ratio",statisticNames_y)) values_y[,"Sortino ratio"] <- paste(values_y[,"Sortino ratio"],"%",sep="")
      tmp <- paste(rowlabels_y,values_y[tsName,],sep=" ")
      report.txt[["insert"]](text=paste(tmp,collapse="\n"))
    }
    if (!is.null(z)) {
      report.txt[["insert"]](text="\n\nPercentage returns:\n")
      values_z <- z[tsName,]
      values_z[,change_z] <- round(z[tsName,change_z],digits=4)
      if (is.element("Annualized mean",statisticNames_z)) values_z[,"Annualized mean"] <- paste(values_z[,"Annualized mean"],"%",sep="")
      if (is.element("Annualized stdev",statisticNames_z)) values_z[,"Annualized stdev"] <- paste(values_z[,"Annualized stdev"],"%",sep="")
      if (is.element("Max Drawdown %",statisticNames_z)) values_z[,"Max Drawdown %"] <- paste(values_z[,"Max Drawdown %"],"%",sep="")
      if (is.element("Sortino ratio",statisticNames_z)) values_z[,"Sortino ratio"] <- paste(values_z[,"Sortino ratio"],"%",sep="")
      tmp <- paste(rowlabels_z,values_z[tsName,],sep=" ")
      report.txt[["insert"]](text=paste(tmp,collapse="\n"))
    }
  } ## end for
}

writeReport <- function()
{
  write(x=report.txt$get.value(),file="report.txt")
}

filterLevelsAndReturns <- function(levels,logReturns,percReturns) {

  filterLevels <- function(computedStatistics) {
    if (is.null(computedStatistics)) return(FALSE)

    minNbObs <- as.integer( entry.minNbObs[["get.value"]]() )
    maxDrawdown <- as.numeric( chop( entry.maxDrawdown[["get.value"]]() ) )
    remove <- rep(FALSE,length(computedStatistics[[1]]))
    useFilter <- FALSE
    if (!is.na(maxDrawdown) & (is.element("Max Drawdown %",names(computedStatistics)))) {
      invalid <- is.na(computedStatistics$"Max Drawdown %")
      remove <- invalid
      remove[!invalid] <- computedStatistics$"Max Drawdown %"[!invalid] > maxDrawdown
      useFilter <- TRUE
    }

    if (!is.na(minNbObs) & (is.element("Nb. Obs.",names(computedStatistics)))) {
      invalid <- is.na(computedStatistics$"Nb. Obs.")
      if (useFilter) {
        remove[invalid] <- invalid[invalid]
        remove[!invalid] <- remove[!invalid] | (computedStatistics$"Nb. Obs."[!invalid] < minNbObs) 
      } else {
        remove <- invalid
        remove[!invalid] <- computedStatistics$"Nb. Obs."[!invalid] < minNbObs
      }
      useFilter <- TRUE
    }
    names(remove) <- names(computedStatistics[[1]])
    return(remove)
  }


  filterReturns <- function(computedStatistics) {
    if (is.null(computedStatistics)) return(FALSE)

    minimumAcceptableReturn <- as.numeric(chop(entry.minimumAcceptableReturn[["get.value"]]()))
    maxStdev <- as.numeric(chop(entry.maxStdev[["get.value"]]()))
    remove <- rep(FALSE,length(computedStatistics[[1]]))
    useFilter <- FALSE
    if (!is.na(minimumAcceptableReturn) & (is.element("Annualized mean",names(computedStatistics)))) {
      invalid <- is.na(computedStatistics$"Annualized mean")
      remove <- invalid
      remove[!invalid] <- computedStatistics$"Annualized mean"[!invalid] < minimumAcceptableReturn
      useFilter <- TRUE
    }

    if (!is.na(maxStdev) & (is.element("Annualized stdev",names(computedStatistics)))) {
      invalid <- is.na(computedStatistics$"Annualized stdev")
      if (useFilter) {
        remove[invalid] <- invalid[invalid]
        remove[!invalid] <- remove[!invalid] | (computedStatistics$"Annualized stdev"[!invalid] > maxStdev) 
      } else {
        remove <- invalid
        remove[!invalid] <- computedStatistics$"Annualized stdev"[!invalid] > maxStdev
      }
      useFilter <- TRUE
    }
    names(remove) <- names(computedStatistics[[1]])
    return(remove)
  }

  rem1 <- filterLevels(levels)
  rem2 <- filterReturns(logReturns)
  rem3 <- filterReturns(percReturns)

  myRemove <- rem1 | rem2 | rem3

  if (any(myRemove)) {
    newLevels.df <- levels[!myRemove,]
    newLogReturns.df <- logReturns[!myRemove,]
    newPercReturns.df <- percReturns[!myRemove,]
  } else {
    newLevels.df <- levels
    newLogReturns.df <- logReturns
    newPercReturns.df <- percReturns
  }
  return(list(computedStatisticsLevels=newLevels.df,computedStatisticsLogReturns=newLogReturns.df,computedStatisticsPercReturns=newPercReturns.df,removedFonds=rownames(levels)[myRemove]))
}

onCreateReport <- function() {
  if (exists("b.exportReport")) tkdestroy(b.exportReport[["button"]])
  b.exportReport <<- create_button(parent=tabNotebook[["pages"]][[2]],text="Export report",command=writeReport)
  tkpack(b.exportReport[["button"]],pady=10)

  v.selectedSeries <- slb.timeSeries[["get.selected"]]()

  computedStatisticsLevels <- computeStatistics(kind="Levels")
  computedStatisticsLogReturns <- computeStatistics(kind="LogReturns")
  computedStatisticsPercReturns <- computeStatistics(kind="PercReturns")

  if (!is.null(computedStatisticsLevels)) {
    columnNames <- names(computedStatisticsLevels)
    computedStatisticsLevels <- as.data.frame(computedStatisticsLevels,stringsAsFactors=FALSE)
    dimnames(computedStatisticsLevels) <- list(v.selectedSeries,columnNames)
  }
  if (!is.null(computedStatisticsLogReturns)) {
    columnNames <- names(computedStatisticsLogReturns)
    computedStatisticsLogReturns <- as.data.frame(computedStatisticsLogReturns,stringsAsFactors=FALSE)
    dimnames(computedStatisticsLogReturns) <- list(v.selectedSeries,columnNames)
  }
  if (!is.null(computedStatisticsPercReturns)) {
    columnNames <- names(computedStatisticsPercReturns)
    computedStatisticsPercReturns <- as.data.frame(computedStatisticsPercReturns,stringsAsFactors=FALSE)
    dimnames(computedStatisticsPercReturns) <- list(v.selectedSeries,columnNames)
  }

  tmp <- filterLevelsAndReturns(computedStatisticsLevels,computedStatisticsLogReturns,computedStatisticsPercReturns)
  filteredComputedStatisticsLevels <- tmp$computedStatisticsLevels
  filteredComputedStatisticsLogReturns <- tmp$computedStatisticsLogReturns
  filteredComputedStatisticsPercReturns <- tmp$computedStatisticsPercReturns

  if (length(tmp$removedFonds) > 0) {

    disponibili <- c(slb.timeSeries[["get.available"]](), tmp$removedFonds)
    selezionati <- setdiff(slb.timeSeries[["get.selected"]](), tmp$removedFonds)
    slb.timeSeries[["set.values"]](newValues = disponibili, widget = 1)
    slb.timeSeries[["set.values"]](newValues = selezionati, widget = 2)
  }

  report.txt[["delete"]]()
  report.txt[["insert"]](text="                         Report of the desired statistics\n",tag="title")
  report.txt[["insert"]](text=paste("                         ",format(Sys.Date(), "%d %b %Y"),"\n",sep=""))

  v.filteredSelectedSeries <- rownames(filteredComputedStatisticsLevels)
  printStatistics(v.filteredSelectedSeries,filteredComputedStatisticsLevels,filteredComputedStatisticsLogReturns,filteredComputedStatisticsPercReturns)
  computedStatistics <<- list(Levels=computedStatisticsLevels,LogReturns=computedStatisticsLogReturns,Percreturns=computedStatisticsPercReturns,
       filteredLevels=filteredComputedStatisticsLevels,filteredLogReturns=filteredComputedStatisticsLogReturns,filteredPercreturns=filteredComputedStatisticsPercReturns)
}


b.createReport <- create_button(parent=f.buttons,text="Create report",command=onCreateReport)
