## crea il bottone per l'esecuzione dei grafici e del report

onPlotSelectedSeries <- function() {
  # v.selectedSeries <- slb.timeSeries[["get.selected"]]()
  v.selectedSeries <- slb.timeSeries[["get.curselection"]](widget=2)

  nbSelected <- length(v.selectedSeries)
  desiredType <- r.tsType[["get.selection"]]()

  if (nbSelected == 0) return()

  if (plotFrom == plotTo) return()

  plotRange <- plotFrom:plotTo

  allNA <- function(tseries) {
    return(all(is.na(tseries)))
  }

  if (nbSelected == 1) {
    if (allNA(ts.df[plotRange,v.selectedSeries])) 
      {
        tkmessageBox(message=paste("La serie",v.selectedSeries,
                       "è senza osservazioni."),icon="warning",type="ok",parent=topWindow)
                                        # tkfocus(topWindow)
        return()
      }
    plotSingleSeries <- function() {
      plot(dates[plotRange],ts.df[plotRange,v.selectedSeries],type="l",ylab="",main=v.selectedSeries)
    }
    if (exists("myPlot")) {tkdestroy(myPlot[["tkrplot"]]);tkdestroy(b.exportPlot[["button"]])}
    ## myPlot <<- tkrplot(parent=tabNotebook[["pages"]][[1]],fun=plotSingleSeries, hscale=1.3, vscale=1.3)
    myPlot <<- create_tkrplot(parent=tabNotebook[["pages"]][[1]],func=plotSingleSeries,hscale=1.3,
                              vscale=1.3,background="white",exportButton=TRUE)
    tkpack(myPlot[["tkrplot"]])
  } else {
    dataPlot.df <- ts.df[plotRange,v.selectedSeries]

    ## determina le serie con tutti NA
    areAllNa <- sapply(X=dataPlot.df,FUN=allNA)
    seriesToPlot <- names(areAllNa)
    if (any(areAllNa)) {
      nbSeries <- sum(areAllNa)

      namesSeries <- colnames(dataPlot.df)[areAllNa]
      if (nbSeries == 1) tkmessageBox(message=paste("La serie",namesSeries,
                                        "è senza osservazioni."),icon="warning",type="ok",parent=topWindow)
      tmp1 = "Le serie:\n"
      tmp2 = paste("- ",namesSeries,sep="",collapse="\n")
      if (nbSeries > 1) tkmessageBox(message=paste(tmp1,tmp2,
                                       "\nsono senza osservazioni.",sep=""),icon="warning",type="ok",parent=topWindow)
      dataPlot.df <- dataPlot.df[,!areAllNa,drop=FALSE]

      if (ncol(dataPlot.df) == 0) return()
      seriesToPlot <- seriesToPlot[!areAllNa]
    }
    ## determina la posizione del primo valore diverso da NA
    indexFirstValue <- function(tseries) {
      return(min((1:nbObs)[ !is.na(tseries) & !(tseries == 0) ]))
    }
    v.indexFirstValue <- sapply(X=dataPlot.df,FUN=indexFirstValue)
    zeroSeries <- v.indexFirstValue == Inf
    if (any(zeroSeries)) {
      nbSeries <- sum(zeroSeries)

      namesSeries <- colnames(dataPlot.df)[zeroSeries]
      if (nbSeries == 1) tkmessageBox(message=paste("La serie",namesSeries,
                                        "è identicamente uguale a 0."),icon="warning",type="ok",parent=topWindow)
      tmp1 = "Le serie:\n"
      tmp2 = paste("- ",namesSeries,sep="",collapse="\n")
      if (nbSeries > 1) tkmessageBox(message=paste(tmp1,tmp2,
                                       "\nsono identicamente uguali a 0.",sep=""),icon="warning",type="ok",parent=topWindow)
      dataPlot.df <- dataPlot.df[,!zeroSeries,drop=FALSE]
      if (ncol(dataPlot.df) == 0) return()
      v.indexFirstValue <- v.indexFirstValue[!zeroSeries]
      seriesToPlot <- seriesToPlot[!zeroSeries]
      rm(nbSeries,namesSeries,tmp1,tmp2)
    }

    nbPlots <- length(v.indexFirstValue)


    if (desiredType == "Levels") {
      ## standardizza a 1 la prima osservazione disponibile
      for (i in 1:nbPlots)  { dataPlot.df[,i] <- dataPlot.df[,i]/dataPlot.df[v.indexFirstValue[i],i] }
    }

    plotMultipleSeries <- function() {
      subNbPlots <- min(8,nbPlots)
      toPlot <- 1:subNbPlots
      maxValue <- max(dataPlot.df[,toPlot],na.rm=TRUE)
      minValue <- min(dataPlot.df[,toPlot],na.rm=TRUE)

      ylimMax <- maxValue + (maxValue - minValue)*0.17
      plot(dates[plotRange], dataPlot.df[,1],type="l",ylab="",
           xlab="",ylim=c(minValue,ylimMax))

      colors = c("black","red","blue","green","gray","pink","orange","yellow")
      if (nbPlots > 1)
        {
          for (i in 2:subNbPlots) 
            {
              lines(dates[plotRange],dataPlot.df[,i],type="l",lty=(i-1) %% 6+1,col=colors[i],ylab="")
            } 
        }
      truncateString <- function(x) {
        y <- nchar(x)
        long <- y > 35
        y <- x
        y[long] <- substr(x[long],1,35)
        return(y)
      }
      par(bg="antiquewhite1")
      legend((dates[plotRange])[1], ylimMax,truncateString(seriesToPlot[toPlot]),col=colors[toPlot],lty=(toPlot-1) %% 6+1,
             lwd=2,ncol = 2, cex = 0.6)

      if (nbPlots > 8) tkmessageBox(message="Only the first 8 series have been plotted.",icon="warning",type="ok")
    }

    if (exists("myPlot")) {tkdestroy(myPlot[["tkrplot"]]);tkdestroy(b.exportPlot[["button"]])}
    ## myPlot <<- tkrplot(parent=tabNotebook[["pages"]][[1]],fun=plotMultipleSeries, hscale=1.3, vscale=1.3)
    myPlot <<- create_tkrplot(parent=tabNotebook[["pages"]][[1]],func=plotMultipleSeries,hscale=1.3,
                              vscale=1.3,background="white",exportButton=TRUE)
    tkpack(myPlot[["tkrplot"]])
  }
  b.exportPlot <<- create_button(parent=tabNotebook[["pages"]][[1]],text="Export plot",command=myPlot[["export"]])
  tkpack(b.exportPlot[["button"]],pady=10)
}

## b.exportPlot <- create_button(parent=tabNotebook[["pages"]][[1]],text="Export plot",command=myPlot[["export"]])
b.plotSelectedSeries <- create_button(parent=f.buttons,text="Plot series",command=onPlotSelectedSeries)
