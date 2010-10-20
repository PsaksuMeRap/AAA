rm(list=ls(all=TRUE))
library("ayrton")
library("tkutilities")
library("odbcutilities")



                                        # temporary function (to delete later)
pippo <- function() print("pippo")


# open odbc connection
odbcCon = setOdbcConnection("Sistema_prova")

# create the local environment
le <- new.env()
start_window <- function()
  {

    # local variable
    hscale <- 1.3 # graph
    vscale <- 1.3 # graph
    background <- "white" # graph
    
    # function for risk factor recovering
    onRiskFactorSelection <- function()
      {
        selectedRf <- lr.riskFactors[["get.selection"]]()

        if (selectedRf==1) {df.riskFactor = get.equityRiskFactor()}
        if (selectedRf==2) {df.riskFactor = get.interestRatesRiskFactor()}
        if (selectedRf==3) {df.riskFactor = get.fxRiskFactor()}
        if (selectedRf==4) {df.riskFactor = get.preciousMetals()}
        if (selectedRf==5) {df.riskFactor = get.equityIndices()}
        if (!exists("df.riskFactor"))
          {
            tkmessageBox(message=paste("No method for risk factor ID",selectedRf),
                     icon="error",type="ok",parent=topWindow)
            return()
          }

        # destroy the widget
        tkdestroy(sltl.riskFactor[["frame"]])

        nbcol <- ncol(df.riskFactor)
        if (nbcol==1)
          {
            df.selectedRiskFactor <- data.frame("pippo"=NA)
            dimnames(df.selectedRiskFactor)[[2]] <- dimnames(df.riskFactor)[[2]]
          }
        else
          {
            df.selectedRiskFactor <- df.riskFactor[-(1:nrow(df.riskFactor)),]
          }
 
        # create the selection tablelist

        sltl.riskFactor <<- create_selectionTablelist(parent=lf.dataSources[["labelFrame"]],dataFrame1=df.riskFactor,
                              title="",dataFrame2=df.selectedRiskFactor,width=30,height=20,
                              alignCols=rep("left",nbcol),alignColsLabel=rep("center",nbcol),withScrollBarX=TRUE,withScrollBarY=TRUE,
                              multiSortColumn=T,selectmode="multiple",rTypes=rep("character",nbcol),
                              grabWindow=topWindow)
        rm(df.riskFactor,df.selectedRiskFactor)
        tkgrid(sltl.riskFactor[["frame"]],row=1,columnspan=2)
        return()
      }

    # define the function collecting all equity Risk Factors
    get.equityRiskFactor <- function()
      {
        odbcCon = setOdbcConnection("Prezzi_storici_VAR")
        
        # Select the list of available time series
        query <- paste("SELECT ID, Moneta AS Currency, Ticker, Company FROM EquityDB",
                       "WHERE ID_strumento=1 AND NotUniversoVar=0",
                       "ORDER BY Moneta, Company")
        df.riskFactors <- get.table(odbcCon, query=query)
        close(odbcCon)
        return(df.riskFactors)
      }

    # define the function collecting all FX
    get.fxRiskFactor <- function()
      {
        odbcCon = setOdbcConnection("Prezzi_storici_VAR")

        # Select the list of available time series
        query <- paste("SELECT DISTINCT B.ID, TotaleCambiStorico.Ticker AS Currency",
                       "FROM TotaleCambiStorico LEFT OUTER JOIN [Sistema (Prova)].dbo.DBMoneta B ON",
                       "TotaleCambiStorico.Ticker=B.Moneta",
                       "WHERE ID IS NOT NULL",
                       "ORDER BY TotaleCambiStorico.Ticker"
                      )

        df.riskFactors <- get.table(odbcCon, query=query)
        close(odbcCon)
        return(df.riskFactors)
      }

    # define the function collecting all precious metals
    get.preciousMetals <- function()
      {
        odbcCon = setOdbcConnection("Prezzi_storici_VAR")

        # Select the list of available precious metals time series
        query <- paste("SELECT DISTINCT TotaleCambiStorico.Ticker",
                       "FROM TotaleCambiStorico LEFT OUTER JOIN [Sistema (Prova)].dbo.DBMoneta B ON",
                       "TotaleCambiStorico.Ticker=B.Moneta",
                       "WHERE ID IS NULL",
                       "ORDER BY TotaleCambiStorico.Ticker"
                      )

        df.riskFactors <- get.table(odbcCon, query=query)
        close(odbcCon)
        return(df.riskFactors)
      }

    # define the function collecting all interest rates
    get.interestRatesRiskFactor <- function()
      {
        odbcCon = setOdbcConnection("Tassi_storici_VAR")

        # Select the list of available time series
        query <- paste("SELECT DISTINCT Moneta AS Currency, Scadenza AS Maturity,",
                       "Scadenza1",
                       "FROM TotaleTassiStorico",
                       "ORDER BY Moneta, Scadenza1"
                      )

        df.riskFactors <- get.table(odbcCon, query=query)
        close(odbcCon)
        return(df.riskFactors[,1:2])
      }


    # define the function collecting the precious metals time series
    get.preciousMetalsTimeSeries <- function(tickers,dataRange)
      {
      
        query <- paste("SELECT Ticker, TRADE_DATE AS [Date], [Close]",
                       "FROM TotaleCambiStorico"
                      )
                      
        odbcCon = setOdbcConnection("Prezzi_storici_VAR")
        tmp="WHERE "

        if (!missing(tickers))
          {
            selectedTickers <- paste(tickers,collapse="','")
            tmp = paste(tmp,"Ticker IN ('",selectedTickers,"') ",
                        sep="")
          }
        else
          {
            # Select the list of available precious metals time series
            query <- paste("SELECT DISTINCT TotaleCambiStorico.Ticker AS Metal",
                       "FROM TotaleCambiStorico LEFT OUTER JOIN [Sistema (Prova)].dbo.DBMoneta B ON",
                       "TotaleCambiStorico.Ticker=B.Moneta",
                       "WHERE ID IS NULL",
                       "ORDER BY TotaleCambiStorico.Ticker"
                      )
            df.tickers <- get.table(odbcCon, query=query)
            selectedTickers <- paste(df.tickers[,"Ticker"],collapse="','")
            tmp = paste(tmp,"Ticker IN ('",selectedTickers,"') ",
                        sep="")
          }
          
        if (!missing(dataRange))
          {
           tmp <- paste(tmp,"AND ")
           tmp <- paste(tmp,"TRADE_DATE >= '",dataRange[1],"' AND TRADE_DATE <= '",
                        dataRange[2],"'",
                        sep=""
                       )
          }
        query <- paste(query,tmp, "ORDER BY Ticker, TRADE_DATE")

        df.result <- get.table(odbcCon, query=query,as.is=c(FALSE,TRUE,FALSE))
        close(odbcCon)
        return(df.result)
      }

    # define the function collecting the fx time series
    get.fxTimeSeries <- function(tickers,dataRange)
      {
        query <- paste("SELECT A.Ticker AS Currency, A.TRADE_DATE AS [Date], A.[Close]",
                       "FROM TotaleCambiStorico A "
                      )

        odbcCon = setOdbcConnection("Prezzi_storici_VAR")
        tmp="WHERE "

        if (!missing(tickers))
          {
            selectedTickers <- paste(tickers,collapse="','")
            tmp = paste(tmp,"Ticker IN ('",selectedTickers,"') ",
                        sep="")
          }
        else
          {
            # Select the list of available precious metals time series
            query <- paste("SELECT DISTINCT A.Ticker Currency, A.TRADE_DATE AS [Date], A.[Close]",
                       "FROM TotaleCambiStorico A LEFT OUTER JOIN [Sistema (Prova)].dbo.DBMoneta B ON",
                       "A.Ticker=B.Moneta",
                       "WHERE ID IS NOT NULL "
                      )
            df.tickers <- get.table(odbcCon, query=query)
            selectedTickers <- paste(df.tickers[,"Ticker"],collapse="','")
            tmp = paste(tmp,"Ticker IN ('",selectedTickers,"') ",
                        sep="")
          }

        if (!missing(dataRange))
          {
           tmp <- paste(tmp,"AND ")
           tmp <- paste(tmp,"TRADE_DATE >= '",dataRange[1],"' AND TRADE_DATE <= '",
                        dataRange[2],"'",
                        sep=""
                       )
          }
        query <- paste(query,tmp, "ORDER BY A.Ticker, A.TRADE_DATE")

        df.result <- get.table(odbcCon, query=query,as.is=c(FALSE,TRUE,FALSE))
        close(odbcCon)
        return(df.result)
      }



    # define the function collecting the interest rates time series
    get.equityTimeSeries <- function(tickers,dataRange)
      {
        query = paste("SELECT Ticker, TRADE_DATE AS [Date], [Close]",
                      "FROM TotalePrezziStorico "
                     )
        odbcCon = setOdbcConnection("Prezzi_storici_VAR")
        tmp="WHERE "

        if (!missing(tickers))
          {
            selectedTickers <- paste(tickers,collapse="','")
            tmp = paste(tmp,"Ticker IN ('",selectedTickers,"') ",
                        sep="")
                        
          }

        if (!missing(dataRange))
          {
           if (!missing(tickers)) tmp <- paste(tmp,"AND ")
           tmp <- paste(tmp,"TRADE_DATE >= '",dataRange[1],"' AND TRADE_DATE <= '",
                        dataRange[2],"'",
                        sep=""
                       )
          }
        query <- paste(query,tmp, "ORDER BY Ticker, TRADE_DATE")
        df.result <- get.table(odbcCon, query=query,as.is=c(FALSE,TRUE,FALSE))
        close(odbcCon)
        return(df.result)
      }

    # define the function collecting the interest rates time series
    get.interestRatesTimeSeries <- function(df.tickers,dataRange)
      {

        query = paste("SELECT Moneta + ' ' + Scadenza AS Ticker, [Date] AS [Date], Tasso AS [Close]",
                      "FROM TotaleTassiStorico "
                     )
        odbcCon = setOdbcConnection("Tassi_storici_VAR")
        tmp="WHERE "

        if (!missing(df.tickers))
          {
            currency <- paste("(Moneta='",df.tickers[,"Currency"],"')",sep="")
            maturity <- paste("(Scadenza='",df.tickers[,"Maturity"],"')",sep="")
            cur_and_mat <- paste("(",currency," AND ",maturity,")",sep="")
            cur_and_mat <- paste(cur_and_mat, collapse=" OR ")
            tmp <- paste(tmp,"(",cur_and_mat,")")
          }

        if (!missing(dataRange))
          {
           if (!missing(df.tickers)) tmp <- paste(tmp,"AND ")
           tmp <- paste(tmp,"date >= '",dataRange[1],"' AND date <= '",
                        dataRange[2],"'",
                        sep=""
                       )
          }

        query <- paste(query,tmp,"ORDER BY Moneta, Scadenza, [Date]")

        df.result <- get.table(odbcCon,query=query,as.is=c(FALSE,TRUE,FALSE))
        close(odbcCon)
        return(df.result)
      }

    report.fxRiskFactor <- function(df.selected,dataRange)
      {
        dimnames(df.selected)[[1]] <- df.selected[,"Currency"]
         # 1) determine the kind of risk factor
        selectedRf <- lr.riskFactors[["get.selection"]]()
        df.result <- get.fxTimeSeries(df.selected[,"Currency"],dataRange)
 
        if (nrow(df.result)==0)
          {
            tkmessageBox(message="No data for the selected risk factors and range",
                     icon="error",type="ok",parent=topWindow)
            return()
          }

        # get the levels in Ticker
        lev <- levels(df.result[,"Currency"])
        df.result[,"Date"] <- substr(df.result[,"Date"],1,10) # convert the date field

        # write into the Report window
        nbSelectedSeries <- length(lev)
        txw.report[["delete"]]()
        if (nbSelectedSeries>1)
          {
            txw.report[["insert"]](text=paste("The following",nbSelectedSeries,
                                   "currencies have been selected:\n\n"))
          }
        else
          {
            txw.report[["insert"]](text="The following currency has been selected:\n\n")
          }
        txw.report[["insert"]](text=paste(1:nbSelectedSeries,")",lev,sep="",collapse="\n"))

        txw.data[["delete"]]()
        dataPlot <- list()
        for (currency in lev)
          {
            tmp <- list()
            tmp[["currency"]] <- currency
            tmp[["df.data"]] <- df.result[df.result[,"Currency"] == currency,-1]
            tmp[["nbObservations"]] <- nrow(tmp[["df.data"]])
            tmp[["firstDate"]] <- tmp[["df.data"]][1,"Date"]
            tmp[["lastDate"]] <- tmp[["df.data"]][tmp[["nbObservations"]],"Date"]
            isNa <- is.na(tmp[["df.data"]][,"Close"])
            tmp[["nbMissingValues"]] <- sum(isNa)
            tmp[["missingDates"]] <- tmp[["df.data"]][isNa,"Date"]

            attach(tmp)
            percent <- round(nbMissingValues/nbObservations*100,2)
            toPrint <- paste(df.data[,"Date"],df.data[,"Close"], sep="\t",collapse="\n")

            # insert the values in the report tab of the notebook
            string <- paste("\n\n\n",currency,"\n\n",
                            "First date: \t",firstDate,"\n","Last date: \t",
                            lastDate,"\n","Number of observations: ",
                            nbObservations,"\n",
                            "Number of missing obs.: ",nbMissingValues,
                            " (",percent,"%)\n","Missing dates :\n",
                            sep=""
                           )

            miss <- paste("\t",missingDates," ",weekdays(as.Date(missingDates)),sep="",collapse="\n")
            string <- paste(string,miss)
            txw.report[["insert"]](text=string)
            detach(tmp)
            rm(string,miss)

            # Insert the values in the data tab of the notebook
            txw.data[["insert"]](text=paste("Data values for ",currency,":\n\n",sep=""))
            txw.data[["insert"]](text=paste(toPrint,"\n\n\n"))
            dataPlot[[1+length(dataPlot)]] = tmp
          }
        
        # insert the values in the plot
        funcplot <- function()
          {
           # attach(dataPlot[[1]])
            oldParams <- par(bg=background)
            plot(dataPlot[[1]][["df.data"]][,"Close"],type="l",ylab="",main=dataPlot[[1]][["currency"]])
            par(oldParams)
           # detach(dataPlot[[1]])
          }
           plot.graphic[["tkrreplot"]]
        plot.graphic[["tkrreplot"]](func=funcplot,hscale=hscale,vscale=vscale)
      }


    report.preciousMetalsRiskFactor <- function(df.selected,dataRange)
      {
        dimnames(df.selected)[[1]] <- df.selected[,"Ticker"]
         # 1) determine the kind of risk factor
        selectedRf <- lr.riskFactors[["get.selection"]]()
        df.result <- get.preciousMetalsTimeSeries(df.selected[,"Ticker"],dataRange)

        if (nrow(df.result)==0)
          {
            tkmessageBox(message="No data for the selected risk factors and range",
                     icon="error",type="ok",parent=topWindow)
            return()
          }

        # get the levels in Ticker
        lev <- levels(df.result[,"Ticker"])
        df.result[,"Date"] <- substr(df.result[,"Date"],1,10) # convert the date field

        # write into the Report window
        nbSelectedSeries <- length(lev)
        txw.report[["delete"]]()
        if (nbSelectedSeries>1)
          {
            txw.report[["insert"]](text=paste("The following",nbSelectedSeries,
                                   "tickers have been selected:\n\n"))
          }
        else
          {
            txw.report[["insert"]](text="The following ticker has been selected:\n\n")
          }
        txw.report[["insert"]](text=paste(1:nbSelectedSeries,")",lev,sep="",collapse="\n"))

        txw.data[["delete"]]()
        dataPlot <- list()
        for (ticker in lev)
          {
            tmp <- list()
            tmp[["ticker"]] <- ticker
            tmp[["df.data"]] <- df.result[df.result[,"Ticker"] == ticker,-1]
            tmp[["nbObservations"]] <- nrow(tmp[["df.data"]])
            tmp[["firstDate"]] <- tmp[["df.data"]][1,"Date"]
            tmp[["lastDate"]] <- tmp[["df.data"]][tmp[["nbObservations"]],"Date"]
            isNa <- is.na(tmp[["df.data"]][,"Close"])
            tmp[["nbMissingValues"]] <- sum(isNa)
            tmp[["missingDates"]] <- tmp[["df.data"]][isNa,"Date"]

            attach(tmp)
            percent <- round(nbMissingValues/nbObservations*100,2)
            toPrint <- paste(df.data[,"Date"],df.data[,"Close"], sep="\t",collapse="\n")

            # insert the values in the report tab of the notebook
            string <- paste("\n\n\n",ticker,"\n\n",
                            "First date: \t",firstDate,"\n","Last date: \t",
                            lastDate,"\n","Number of observations: ",
                            nbObservations,"\n",
                            "Number of missing obs.: ",nbMissingValues,
                            " (",percent,"%)\n","Missing dates :\n",
                            sep=""
                           )

            miss <- paste("\t",missingDates," ",weekdays(as.Date(missingDates)),sep="",collapse="\n")
            string <- paste(string,miss)
            txw.report[["insert"]](text=string)
            detach(tmp)
            rm(string,miss)

            # Insert the values in the data tab of the notebook
            txw.data[["insert"]](text=paste("Data values for ",ticker,":\n\n",sep=""))
            txw.data[["insert"]](text=paste(toPrint,"\n\n\n"))
            dataPlot[[1+length(dataPlot)]] = tmp
          }
        # insert the values in the plot
        funcplot <- function()
          { 
            oldParams <- par(bg=background)
            plot(dataPlot[[1]][["df.data"]][,"Close"],type="l",ylab="",main=dataPlot[[1]][["ticker"]])
            par(oldParams)
          }
        plot.graphic[["tkrreplot"]](func=funcplot,hscale=1.4,vscale=1.4)
      }


    report.interestRatesRiskFactor <- function(df.selected,dataRange)
      {
        dimnames(df.selected)[[1]] <- paste(df.selected[,"Currency"],df.selected[,"Maturity"],sep="")
        # get the time series of interest rates
        df.result <- get.interestRatesTimeSeries(df.selected,dataRange)
        
        if (nrow(df.result)==0)
          {
            tkmessageBox(message="No data for the selected risk factors and range",
                     icon="error",type="ok",parent=topWindow)
            return()
          }

        df.result[,"Date"] <- substr(df.result[,"Date"],1,10) # convert the date field

        # write into the Report window
        lev <- levels(df.result[,"Ticker"])
        
        nbSelectedSeries <- length(lev)
        txw.report[["delete"]]()
        if (nbSelectedSeries>1)
          {
            txw.report[["insert"]](text=paste("The following",nbSelectedSeries,
                                   "tickers have been selected:\n\n"))
          }
        else
          {
            txw.report[["insert"]](text="The following ticker has been selected:\n\n")
          }
        txw.report[["insert"]](text=paste(1:nbSelectedSeries,")",lev,sep="",collapse="\n"))

        txw.data[["delete"]]()
        dataPlot <- list()
        for (ticker in lev)
          {
            tmp <- list()
            tmp[["ticker"]] <- ticker
            tmp[["maturity"]] <- df.selected[ticker,"Maturity"]
            tmp[["df.data"]] <- df.result[df.result[,"Ticker"] == ticker,-1]
            tmp[["nbObservations"]] <- nrow(tmp[["df.data"]])
            tmp[["firstDate"]] <- tmp[["df.data"]][1,"Date"]
            tmp[["lastDate"]] <- tmp[["df.data"]][tmp[["nbObservations"]],"Date"]
            isNa <- is.na(tmp[["df.data"]][,"Close"])
            tmp[["nbMissingValues"]] <- sum(isNa)
            tmp[["missingDates"]] <- tmp[["df.data"]][isNa,"Date"]

            attach(tmp)
            percent <- round(nbMissingValues/nbObservations*100,2)
            toPrint <- paste(df.data[,"Date"],df.data[,"Close"], sep="\t",collapse="\n")

            # insert the values in the report tab of the notebook
            string <- paste("\n\n\n",ticker,"\n\n",
                            "First date: \t",firstDate,"\n","Last date: \t",
                            lastDate,"\n","Number of observations: ",
                            nbObservations,"\n",
                            "Number of missing obs.: ",nbMissingValues,
                            " (",percent,"%)\n","Missing dates :\n",
                            sep=""
                           )

            miss <- paste("\t",missingDates," ",weekdays(as.Date(missingDates)),sep="",collapse="\n")
            string <- paste(string,miss)
            txw.report[["insert"]](text=string)
            detach(tmp)
            rm(string,miss)

            # Insert the values in the data tab of the notebook
            txw.data[["insert"]](text=paste("Data values for ",ticker,":\n\n",sep=""))
            txw.data[["insert"]](text=paste(toPrint,"\n\n\n"))
            dataPlot[[1+length(dataPlot)]] = tmp
          }
        # insert the values in the plot
        funcplot <- function()
          {
            oldParams <- par(bg=background)
            plot(dataPlot[[1]][["df.data"]][,"Close"],type="l",ylab="",main=dataPlot[[1]][["ticker"]])
            par(oldParams)
          }
        plot.graphic[["tkrreplot"]](func=funcplot,hscale=1.4,vscale=1.4)
      }
      
    report.equityRiskFactor <- function(df.selected,dataRange)
      {
        # delete ... selectedTickers <- paste(df.selected[,"Ticker"],collapse="','")
        dimnames(df.selected)[[1]] <- df.selected[,"Ticker"]

        df.result <- get.equityTimeSeries(df.selected[,"Ticker"],dataRange)

        if (nrow(df.result)==0)
          {
            tkmessageBox(message="No data for the selected risk factors and range",
                     icon="error",type="ok",parent=topWindow)
            return()
          }

        # get the levels in Ticker
        lev <- levels(df.result[,"Ticker"])
        df.result[,"Date"] <- substr(df.result[,"Date"],1,10) # convert the date field

        # write into the Report window
        nbSelectedSeries <- length(lev)
        txw.report[["delete"]]()
        if (nbSelectedSeries>1)
          {
            txw.report[["insert"]](text=paste("The following",nbSelectedSeries,
                                   "tickers have been selected:\n\n"))
          }
        else
          {
            txw.report[["insert"]](text="The following ticker has been selected:\n\n")
          }
        txw.report[["insert"]](text=paste(1:nbSelectedSeries,")",lev,sep="",collapse="\n"))
        
        txw.data[["delete"]]()
        dataPlot <- list()
        for (ticker in lev)
          {
            tmp <- list()
            tmp[["ticker"]] <- ticker
            tmp[["company"]] <- df.selected[ticker,"Company"]
            tmp[["df.data"]] <- df.result[df.result[,"Ticker"] == ticker,-1]
            tmp[["nbObservations"]] <- nrow(tmp[["df.data"]])
            tmp[["firstDate"]] <- tmp[["df.data"]][1,"Date"]
            tmp[["lastDate"]] <- tmp[["df.data"]][tmp[["nbObservations"]],"Date"]
            isNa <- is.na(tmp[["df.data"]][,"Close"])
            tmp[["nbMissingValues"]] <- sum(isNa)
            tmp[["missingDates"]] <- tmp[["df.data"]][isNa,"Date"]
            
            attach(tmp)
            percent <- round(nbMissingValues/nbObservations*100,2)
            toPrint <- paste(df.data[,"Date"],df.data[,"Close"], sep="\t",collapse="\n")
            
            # insert the values in the report tab of the notebook
            string <- paste("\n\n\n",ticker,": ",company,"\n\n",
                            "First date: \t",firstDate,"\n","Last date: \t",
                            lastDate,"\n","Number of observations: ",
                            nbObservations,"\n",
                            "Number of missing obs.: ",nbMissingValues,
                            " (",percent,"%)\n","Missing dates :\n",
                            sep=""
                           )

            miss <- paste("\t",missingDates," ",weekdays(as.Date(missingDates)),sep="",collapse="\n")
            string <- paste(string,miss)
            txw.report[["insert"]](text=string)
            detach(tmp)
            rm(string,miss)
            
            # Insert the values in the data tab of the notebook
            txw.data[["insert"]](text=paste("Data values for ",ticker,":\n\n",sep=""))
            txw.data[["insert"]](text=paste(toPrint,"\n\n\n"))
            dataPlot[[1+length(dataPlot)]] = tmp
          }
        # insert the values in the plot
        funcplot <- function()
          {
            oldParams <- par(bg=background)
            plot(dataPlot[[1]][["df.data"]][,"Close"],type="l",ylab="",main=dataPlot[[1]][["company"]])
            par(oldParams)
          }
        plot.graphic[["tkrreplot"]](func=funcplot,hscale=1.4,vscale=1.4)
      }
      
    # define the function creating the reports in the tabnotebook
    createReports <- function()
      {
        # 1) determine the kind of risk factor
        selectedRf <- lr.riskFactors[["get.selection"]]()

        # 2) get the selected values
        df.selected <- sltl.riskFactor[["get.selected"]]()
        dataRange <- c(de.startDate[["get.date"]](),de.endDate[["get.date"]]())

        if (nrow(df.selected)==0)
          {
            tkmessageBox(message="No risk factor selected",
                      icon="error",type="ok",parent=topWindow)
            return()
          }
          
        # 3) construct the Report
        if (selectedRf == "1") report.equityRiskFactor(df.selected,dataRange)
        if (selectedRf == "2") report.interestRatesRiskFactor(df.selected,dataRange)
        if (selectedRf == "3") report.fxRiskFactor(df.selected,dataRange)
        if (selectedRf == "4") report.preciousMetalsRiskFactor(df.selected,dataRange)
        

        # Modify the summary
      }
      
                                          # create top window
    topWindow <- tktoplevel()  #used in get.riskFactors
    tktitle(topWindow) <- "FIS - Model selection"

    # create the first labelframe containing the Data sources
    lf.dataSources <- create_labelFrame(parent=topWindow,text="Data sources")

    # create the labeled Radio using the data in the following query
    query <- "SELECT ID, riskFactor FROM DBRiskFactors"
    df.riskFactors <- get.table(odbcCon, query=query)
    
    l.labelRiskFactor <- create_label(parent=lf.dataSources[["labelFrame"]],value="Risk factor")
    lr.riskFactors <- create_labeledRadio(parent=lf.dataSources[["labelFrame"]],
                                          values=df.riskFactors[,"ID"],
                                          labels=df.riskFactors[,"riskFactor"],
                                          command=onRiskFactorSelection
                                         )
    # create the two dateentry
    de.startDate <- create_dateentry(parent=lf.dataSources[["labelFrame"]],label="Start date:")
    de.startDate[["set.date"]](as.character(Sys.Date()-30))
    de.endDate <- create_dateentry(parent=lf.dataSources[["labelFrame"]],label="End date:")
    
    # select the equity Risk Factor (default)
    df.riskFactor <- get.equityRiskFactor()
    df.selectedRiskFactor <- df.riskFactor[-(1:nrow(df.riskFactor)),]
    # create the selection tablelist
    sltl.riskFactor <- create_selectionTablelist(parent=lf.dataSources[["labelFrame"]],dataFrame1=df.riskFactor,
                              title="",dataFrame2=df.selectedRiskFactor,width=30,height=20,
                              alignCols=rep("left",4),alignColsLabel=rep("center",4),withScrollBarX=TRUE,withScrollBarY=TRUE,
                              multiSortColumn=T,selectmode="multiple",rTypes=c("integer",rep("character",3)),
                              grabWindow=topWindow)
    rm(df.riskFactor,df.selectedRiskFactor)
    
    # create the Ok button under the selection tablelist
    b.okCreateReport <- create_button (parent=lf.dataSources[["labelFrame"]],
                                       command=createReports)
    
    # create the tabnotebook widget
    tbn.notebook <- create_tabnotebook (parent=topWindow,label=c("Report","Data","Graphic",
                                        "Project summary"),height=600,width=600,
                                        tabpos="n",tabbackground="white",background="systemButtonFace")
    txw.report <- create_textwindow (parent=tbn.notebook[["frame1"]],width=50,height=25,
                                        withScrollBarY=TRUE,withScrollBarX=TRUE)

    txw.data <- create_textwindow (parent=tbn.notebook[["frame2"]],width=50,height=25,
                                        withScrollBarY=TRUE,withScrollBarX=TRUE)
    
    f.plot.graphic <- tkframe(tbn.notebook[["frame3"]])                                    
    plot.graphic <- create_tkrplot(parent=f.plot.graphic,hscale=hscale,vscale=vscale,
                                background=background,exportButton=T)
    b.exportGraphic <- create_button(f.plot.graphic,text="Export",command=plot.graphic[["export"]])

    
    # INSERT OF THE CREATED WIDGETS
    # insert objects in the labelframe dataSources
    tkgrid(lf.dataSources[["labelFrame"]],tbn.notebook[["tabnotebook"]],padx=padx,
           pady=pady,sticky="n")
    tkgrid(l.labelRiskFactor[["label"]],padx=padx,pady=pady,sticky="e")
    tkgrid(lr.riskFactors[["frame"]],row=0,column=1,padx=padx,pady=pady,sticky="w")
    tkgrid(sltl.riskFactor[["frame"]],columnspan=2)
    tkgrid(b.okCreateReport[["button"]],padx=padx,pady=pady,columnspan=2)
    tkgrid(de.startDate[["dateentry"]],de.endDate[["dateentry"]],padx=padx,pady=c(15,40))

    # insert the elements in the widget
    tkgrid(txw.report[["frame"]],padx=padx,pady=pady) # first frame
    tkgrid(txw.data[["frame"]],padx=padx,pady=pady) # second frame
    
    tkgrid(plot.graphic[["tkrplot"]]) # third frame
    # tbn.notebook[["select.tab"]]()
      tkgrid(f.plot.graphic,padx=padx,pady=pady)
        tkgrid(b.exportGraphic[["button"]],padx=padx,pady=pady)
    return()
  }

                                        # program's start

                                        # set the default windows options
padx=10
pady=5
# define the global variables

env <- topenv(new.env())

env[["selectedClientId"]] <- 0


a <- start_window()