

onConfigureReport <- function() {
  
  onOk <- function() {
    selectedLevels <- slb.requiredStatisticsLevels[["get.selected"]]()
    selectedReturns <- slb.requiredStatisticsReturns[["get.selected"]]()
    assign("statistics",data.frame(available=I(statistics[[1]]),desiredLevels=is.element(statistics[[1]],selectedLevels),desiredReturns=is.element(statistics[[1]],selectedReturns)),envir=env) 
                                        # answer <- tkmessageBox(message="Exit configuration window?",icon="question",type="yesno",parent=configurationWindow)
                                        # if (tclvalue(answer) == "yes") tkdestroy(configurationWindow)
    tkdestroy(configurationWindow)
  }

  onCancel <- function() {
    tkdestroy(configurationWindow)
  }


  configurationWindow <- tktoplevel()
  tktitle(configurationWindow) <- "Required statistics"
  tkconfigure(configurationWindow,background="#D8D8D8") #

  values2Levels <- statistics[statistics[,"desiredLevels"],"available"]
  values2Returns <- statistics[statistics[,"desiredReturns"],"available"]

  slb.requiredStatisticsLevels <- create_selectionListBox(parent=configurationWindow,
                                                          title="\nSelection for levels\n",values1=statistics[[1]],values2=values2Levels,mode="multiple",height=14,width=15,
                                                          withScrollBarY=TRUE,align="h",grabWindow=configurationWindow,order=c(FALSE))

  slb.requiredStatisticsReturns <- create_selectionListBox(parent=configurationWindow,
                                                           title="\nSelection for returns\n",values1=statistics[[1]],values2=values2Returns,mode="multiple",height=14,width=15,
                                                           withScrollBarY=TRUE,align="h",grabWindow=configurationWindow,order=c(FALSE))

                                        # se metti colClass "logical" appare l'uno!!
  tkgrid(slb.requiredStatisticsLevels[["frame"]],slb.requiredStatisticsReturns[["frame"]])

  b.okCancel <- create_okCancelButton(parent=configurationWindow,onOk=onOk,onCancel=onCancel)

  tkgrid(b.okCancel,columnspan=2)

  tkgrab(configurationWindow)
  tkfocus(configurationWindow)
}

b.configureReport <- create_button(parent=f.buttons,text="Configure report",command=onConfigureReport)

