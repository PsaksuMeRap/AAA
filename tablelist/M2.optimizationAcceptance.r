optimizationAcceptance <- function(optimResults)
{
  acceptOptimization <- tktoplevel()
  
  ## local variable
  hscale <- 1.3 # graph
  vscale <- 1.3 # graph
  background <- "white" # graph
  
  name <- as.character(acceptOptimization)
  .Tcl(paste("focus",name))
  tktitle(acceptOptimization) <- "Optimization's results"

  odbcCon <- setOdbcConnection("DBMarkowitz")
  
  ## create the frame containing the parametrisation, i.e. client, varLimit and VarConfidenceLevel
  f.parametrization <- create_labelFrame(parent=acceptOptimization,text="Parametrization")
  ## create the label client
  l.client <- create_label(parent=f.parametrization[["labelFrame"]],value=
                           paste("Client:  ",optimResults[["client"]]," - ",optimResults[["referenceCurrency"]],sep=""))

  ## create the label VaR Limit
  l.varLimit <- create_label(parent=f.parametrization[["labelFrame"]],value="VAR limit:")
  ##e.varLimit <- create_entry(parent=f.parametrization[["labelFrame"]],value=paste(env[["selectedVarLimit"]]*100,"%",sep=""),
  ##                           width="10",dataFormat=list(type="%",option=-1))
  e.varLimit <- create_entry(parent=f.parametrization[["labelFrame"]],value=env[["selectedVarLimit"]],
                             width="10")

  ## crete the label VaR confidence
  l.varConfidenceLevel <- create_label(parent=f.parametrization[["labelFrame"]],
                                       value="VAR confidence level:")
  e.varConfidenceLevel <- create_entry(parent=f.parametrization[["labelFrame"]],
                                       value=optimResults[["varConfidenceLevel"]],width="10")


  
  ## create the tabnotebook widget
  tbn.notebook <- create_tabnotebook (parent=acceptOptimization,label=c("Optimal weights","Efficient frontier","Efficient VaR",
                                        "Warning messages"),height=600,width=600,
                                        tabpos="n",tabbackground="white",background="systemButtonFace")
  ## create the tablelist with the optimal weights
  f.optimalWeights <- tkframe(tbn.notebook[["frame1"]])
    ## create the widget
    portfolio <- t(optimResults[["portfolio"]])
    nbEfficientPoints <- nrow(optimResults[["portfolio"]]) - 2
    tmp <- paste("Er=",round(optimResults[["mu"]]*100, digits = 2),"%",sep="")
    dimnames(portfolio) <- list(NULL,c("Currency","Sector",tmp))
    tbl.portfolioStructure <- create_tablelist(parent=f.optimalWeights,
                  dataFrame=as.data.frame(portfolio),width=90,height=25,
                  withScrollBarX=TRUE,withScrollBarY=TRUE,
                  rTypes=c("character","character",rep("double",nbEfficientPoints)),
                  colFormats=c("no","no",rep("round4",nbEfficientPoints)))
    rm(nbEfficientPoints)
                       
  ## create the frame and the tkrplot widget for the efficient frontier
  f.plotEfficientFrontier <- tkframe(tbn.notebook[["frame2"]])
    plot.efficientFrontier <- create_tkrplot(parent=f.plotEfficientFrontier,hscale=hscale,vscale=vscale,
                                background=background,exportButton=T)
    ## create the button "Export" for the efficien frontier plot
    b.exportEfficientFrontier <- create_button(parent=f.plotEfficientFrontier,text="Export",command=plot.efficientFrontier[["export"]])

  ## create the frame and the tkrplot widget for the efficient VaR  
  f.plotEfficientVaR <- tkframe(tbn.notebook[["frame3"]]); tkgrid(f.plotEfficientVaR)
    plot.efficientVaR <- create_tkrplot(parent=f.plotEfficientVaR,hscale=hscale,vscale=vscale,
                                background=background,exportButton=T)
    ## create the button "Export" for the efficien frontier plot
    b.exportEfficientVaR <- create_button(parent=f.plotEfficientVaR,text="Export",command=plot.efficientVaR[["export"]])  
  

  ## create the text window for the error and warning messages  
  txw.warningMessages <- create_textwindow (parent=tbn.notebook[["frame4"]],width=50,height=25,
                                        withScrollBarY=TRUE,withScrollBarX=TRUE); tkgrid(txw.warningMessages[["textwindow"]])
  messages  <- sql.get.table(odbcCon,query=paste("SELECT * FROM Messaggi_da_R WHERE Cliente LIKE '", optimResults[["client"]], "'",sep="")) 
  tmp <- paste("Message number ",messages[,"NumeroMessaggio"]," with code ",messages[,"Codice"],"\nMessage:\n",
               messages[,"Messaggio"],sep="",collapse="\n\n")
  txw.warningMessages[["insert"]](text=tmp)
  rm(tmp)
  
  ## create the function for the plot of the efficient frontier 
  plotEfficientFrontier <- function()
    {
      oldPar <- par(bg=background)
      plot(x=optimResults[["sigma"]]*100,
           y=optimResults[["mu"]]*100,
           main="Mean - Stdv",
           # type="l",ylab="Expected Return (%)",xlab=expression(sigma),
           type="l",ylab="Expected Return (%)",xlab="Stdv",
           lwd=2
          )
      par(oldPar)
    }
    
  ## create the function for the plot of the efficient VAR
  plotEfficientVaR <- function()
    {
      oldPar <- par(bg=background)
      plot(x=-optimResults[["VaRisk"]],
           y=optimResults[["mu"]]*100,
           main="Mean - VaR",
           # type="l",ylab="Expected Return (%)",xlab=expression(sigma),
           type="l",ylab="Expected Return (%)",xlab="VaR",
           lwd=2
          )
      par(oldPar)
    }
    
  ## create the function used by the Accept button
  onAccept <- function()
    {
      close(odbcCon)
      tkdestroy(acceptOptimization)
    }
  onReject <- function()
    {
      close(odbcCon)
      tkdestroy(acceptOptimization) 
    }
    
  ## create the frame containing the buttons
  f.buttons <- tkframe(acceptOptimization)
  ## create the button "Accept"
  b.accept <- create_button(parent=f.buttons,text="Accept",command=onAccept)
  ## create the button "Reject"
  b.reject <- create_button(parent=f.buttons,text="Reject",command=onReject)

  ## insert the labelframe parametrisation
  tkgrid(f.parametrization[["labelFrame"]],padx=padx,pady=pady,sticky="n",row=0,column=0)
  ## insert the client label
  tkgrid(l.client[["label"]],sticky="w",padx=padx,pady=pady,columnspan=2)
  ## insert the VAR limit
  tkgrid(l.varLimit[["label"]],e.varLimit[["entry"]],sticky="w",padx=padx,pady=pady)
  ## insert the VAR Confidence Level
  tkgrid(l.varConfidenceLevel[["label"]],e.varConfidenceLevel[["entry"]],sticky="w",padx=padx,pady=pady)

  ## insert the tabnotebook widget and the corresponding tabs
  tkgrid(tbn.notebook[["tabnotebook"]],row=0,column=1,rowspan=2,padx=padx,pady=pady)
    tkgrid(f.optimalWeights,padx=padx,pady=pady)       # tab optimal weights
      tkgrid(tbl.portfolioStructure[["frame"]])
    tkgrid(f.plotEfficientFrontier,padx=padx,pady=pady)
      tkgrid(plot.efficientFrontier[["tkrplot"]])
      tkgrid(b.exportEfficientFrontier[["button"]],padx=padx,pady=pady)  
    tkgrid(f.plotEfficientVaR,padx=padx,pady=pady)
      tkgrid(plot.efficientVaR[["tkrplot"]])
      tkgrid(b.exportEfficientVaR[["button"]],padx=padx,pady=pady)   
    tkgrid(txw.warningMessages[["frame"]],padx=padx,pady=pady)
    
  # plot the efficient frontier
  plot.efficientFrontier[["tkrreplot"]](func=plotEfficientFrontier)

  # plot the efficient VaR
  plot.efficientVaR[["tkrreplot"]](func=plotEfficientVaR)
    
  
  ## insert the buttons frame
  tkgrid(f.buttons,padx=padx,pady=pady,sticky="n",row=1,column=0)
  tkgrid(b.accept[["button"]],b.reject[["button"]],padx=padx,pady=pady)
  tkgrid.rowconfigure(acceptOptimization,1,weight=2)
  
  tkgrab(acceptOptimization)
}