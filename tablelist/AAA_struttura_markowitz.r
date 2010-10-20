#timeSeries = list()
#  timeSeries$dateStart
#  timeSeries$dateEnd
#  timeSeries$prices # data.frame with first column = dates
#  timeSeries$currencies # data.frame la cui prima colonna è il ticker della serie
                        # e la seconda la moneta corrispondente



rm(list=ls(all=TRUE))
library("tcltk")
library("ayrton")
library("tkutilities")
library("odbcutilities")
source("C:/R/R_tablelist/MarkowitzSqlProcedures.R")

multipleConstraints <- data.frame(Id=integer(0),=character(0),Type=character(0),Value=numeric(0))
timeSeries <- list()
timeSeries$currencies = data.frame(

create.constraints.window <- function()
    {

      ## open a connection to DBMarkowitz
      odbcCon = setOdbcConnection("DBMarkowitz")

      ## Define function used by the multiple constraints buttons
      verifyMultipleConstrSelection <- function()
        {
          selectedValue <- cb.constraintId[["get.selection"]]()
          if (selectedValue == "")
            {
              tkmessageBox(message="No constraint selected",icon="error",type="ok")
              return (FALSE)
            }
          return (TRUE)
        }

      onNew <- function()
        {
          onCancel <- function()
            {
              tkdestroy(topWindow)
              tkgrab(window.constraints)
              tkfocus(window.constraints)
              return()
            }

          onInsert <- function()
            {
              ## finish editing
              tbl.values[["finishediting"]]()
              constrValue = entry.value[["get.value"]]()

              ## verify that the entry field
              if (constrValue=="")
                {
                  tkmessageBox(message="Invalid constraint value",icon="error",type="ok")
                  return ()
                }

              ## get the number of constraint
              if (tbl.multipleConstr[["get.nbrows"]]() > 0)
                {

                  nb.constraint = max(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]])) + 1
                }
              else
                {
                  nb.constraint = 1
                }


              ## verify that at least one is selected
              isRequired <- values[["dataFrame"]][,"Required"] == 1
              if (sum(isRequired)==0)
                {
                  tkmessageBox(message="The selection is empty",icon="error",type="ok")
                  return ()
                }

              ## construct the query
              equality <- radio.equality[["get.selection"]]()
              query <- paste("INSERT INTO B_Restrizioni_multiple ",
                             "(NumeroRestrizione,Moneta,Branche,TipoRestrizione,ValoreRestrizione,Selezionato) ",
                             "VALUES(",nb.constraint,",'",values[["dataFrame"]][,"Currency"],"',",
                             "'",values[["dataFrame"]][,"Sector"],"',","'",equality,"',",constrValue,",",
                             values[["dataFrame"]][,"Required"],")",
                             sep="",collapse=";"
                             )
              result <- sqlCommand(channel=odbcCon,query=query)

              ## update the combobox with the number of constraints and the tcl widget
              cb.constraintId[["modify.values"]](values=1:nb.constraint)
              dataFrameToAdd <- cbind(Id=nb.constraint,values[["dataFrame"]][isRequired,c("Currency","Sector")],
                                      Type=I(equality),Value=I(constrValue)
                                      )

              nbNewRows = tbl.multipleConstr[["get.nbrows"]]()
              tbl.multipleConstr[["add.rows"]](dataFrameToAdd)
              tkdestroy(topWindow)
              tkfocus(window.constraints)
              return()
            } ## end onInsert

          onCheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "1"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }

          onUncheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "0"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }

          query <- paste("SELECT Moneta, Branche, Selezionato",
                         "FROM B_monete_e_settori_selezionati ",
                         "ORDER BY Moneta, Branche"
                         )

          valuesSqlType <- c("nvarchar","nvarchar","bit")
          names(valuesSqlType) <- c("Moneta","Branche","Selezionato")
          values <- tcl.get.table(odbcConnection=odbcCon,tableName="B_monete_e_settori_selezionati",query=query,
                                  requiredFields=list(Currency="Moneta",Sector="Branche",Required="Selezionato"),
                                  updateDb=F,odbcFieldTypes=valuesSqlType)

          topWindow <- tktoplevel()
          tktitle(topWindow) <- "New multiple constraint"


          ## create the tk tablelist widget
          frame.values <- tkframe(parent=topWindow)
          tbl.values <- create_odbcTablelist(parent=frame.values,data=values,withScrollBarX=TRUE,
                                             withScrollBarY=TRUE,width=40,editable=list(Selezionato="checkbutton"),
                                             height=30,colFormats=c("no","no","logical"))


          ## create the check and uncheck buttons
          frame.checkButtons <- tkframe(parent=topWindow)
          b.checkAll <- create_button(parent=frame.checkButtons,text="Check all",command=onCheckAll)
          b.uncheckAll <- create_button(parent=frame.checkButtons,text="Uncheck all",command=onUncheckAll)


          ## create the labeled radio widget
          radio.equality <- create_labeledRadio(parent=topWindow,values=c(">=","<="," ="))
          entry.value <- create_entry(parent=topWindow,value="",width="6")

          ## create the insert and cancel buttons
          frame.buttons <- tkframe(parent=topWindow)
          b.insert <- create_button(parent=frame.buttons,text="Insert",command=onInsert)
          b.cancel <- create_button(parent=frame.buttons,text="Cancel",command=onCancel)

          ## insert in the window
          ## the tablelist
          tkgrid(tbl.values[["frame"]],padx=padx,pady=pady)
          tkgrid(frame.values,padx=padx,pady=pady,columnspan=2)

          ## the checkAll buttons
          tkgrid(b.checkAll[["button"]],b.uncheckAll[["button"]],padx=padx,pady=pady)
          tkgrid(frame.checkButtons,padx=padx,pady=pady,columnspan=2)

          ## the radio and entry
          tkgrid(radio.equality[["frame"]],padx=padx,pady=pady,row=2,sticky="e")
          tkgrid(entry.value[["entry"]],padx=padx,pady=pady,row=2,column=1,sticky="w")

          ## the insert and cancel buttons
          tkgrid(b.insert[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
          tkgrid(frame.buttons,padx=padx,pady=pady,columnspan=2)

          ## set the grab and focus
          tkgrab(topWindow)
          tkfocus(topWindow)

        }
      ## end onNew

      onModify <- function()
        {
          if (!verifyMultipleConstrSelection()) return ()

          ## get the number of the restriction
          constraintId <- cb.constraintId[["get.selection"]]()

          onCancel <- function()
            {
              tkdestroy(topWindow)
              tkgrab(window.constraints)
              tkfocus(window.constraints)
              return()
            }

          onUpdate <- function()
            {
              ## finish editing
              tbl.values[["finishediting"]]()
              constrValue = entry.value[["get.value"]]()

              ## verify that the entry field
              if (constrValue=="")
                {
                  tkmessageBox(message="Invalid constraint value",icon="error",type="ok")
                  return ()
                }


              ## verify that at least one is selected
              isRequired <- values[["dataFrame"]][,"Required"] == 1
              if (sum(isRequired)==0)
                {
                  tkmessageBox(message="The selection is empty",icon="error",type="ok")
                  return ()
                }

              ## remove previous data
              query <- paste("DELETE FROM B_Restrizioni_multiple ",
                             "WHERE NumeroRestrizione =",constraintId
                             )
              result <- sqlCommand(channel=odbcCon,query=query)

              ## insert the new values
              equality <- radio.equality[["get.selection"]]()
              query <- paste("INSERT INTO B_Restrizioni_multiple ",
                             "(NumeroRestrizione,Moneta,Branche,TipoRestrizione,ValoreRestrizione,Selezionato) ",
                             "VALUES(",constraintId,",'",values[["dataFrame"]][,"Currency"],"',",
                             "'",values[["dataFrame"]][,"Sector"],"',","'",equality,"',",constrValue,",",
                             values[["dataFrame"]][,"Required"],")",
                             sep="",collapse=";"
                             )
              result <- sqlCommand(channel=odbcCon,query=query)

              ## sort the tbl.multipleConstr widget accordint to the NumeroRestrizione, Moneta, Branche columns
              ## tbl.multipleConstr[["SortByColumns"]](index=c(1,2,3))

              ## remove all rows with the selected constraintId
              colConstraintId <- tbl.multipleConstr[["get.columns"]](1)
              toRemove <- colConstraintId[,1] == constraintId
              idToRemove <- (1:length(toRemove))[toRemove]
              tbl.multipleConstr[["remove.rows"]](index=idToRemove)

              ## insert the new values for the corresponding constraintId
              toInsert <- values[["dataFrame"]][,"Required"] == 1
              newValues <- data.frame(Id=I(constraintId),values[["dataFrame"]][toInsert,1:2],Type=I(equality),Value=I(constrValue))
              tbl.multipleConstr[["add.rows"]](newRows=newValues)

              ## adjust the ordering
              tbl.multipleConstr[["SortByColumns"]](index=c(1,2,3))
              tbl.multipleConstr[["resetSort"]]()

              tkdestroy(topWindow)
              tkfocus(window.constraints)
              return()
            } ## end onUpdate

          onCheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "1"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }
          onUncheckAll <- function()
            {
              nb.rows = nrow(values[["dataFrame"]])
              if (nb.rows > 0)
                {
                  values[["dataFrame"]][,"Required"] <<- "0"
                  tbl.values[["remove.rows"]](index=1:nb.rows)
                  tbl.values[["insert.data.frame"]](dataFrame=values[["dataFrame"]])
                }
            }


          query <- paste("SELECT Moneta, Branche, Selezionato",
                         "FROM B_Restrizioni_multiple",
                         "WHERE NumeroRestrizione =",constraintId,
                         "ORDER BY A.Moneta, A.Branche"
                         )

          valuesSqlType <- c("nvarchar","nvarchar","bit")
          names(valuesSqlType) <- c("Moneta","Branche","Selezionato")
          values <- tcl.get.table(odbcConnection=odbcCon,tableName="B_monete_e_settori_selezionati",query=query,
                                  requiredFields=list(Currency="Moneta",Sector="Branche",Required="Selezionato"),
                                  updateDb=F,odbcFieldTypes=valuesSqlType)

          topWindow <- tktoplevel()
          tktitle(topWindow) <- paste("Update multiple constraint",constraintId)


          ## create the tk tablelist widget
          frame.values <- tkframe(parent=topWindow)
          tbl.values <- create_odbcTablelist(parent=frame.values,data=values,withScrollBarX=TRUE,
                                             withScrollBarY=TRUE,width=40,editable=list(Selezionato="checkbutton"),
                                             height=30,colFormats=c("no","no","logical"))


          ## create the check and uncheck buttons
          frame.checkButtons <- tkframe(parent=topWindow)
          b.checkAll <- create_button(parent=frame.checkButtons,text="Check all",command=onCheckAll)
          b.uncheckAll <- create_button(parent=frame.checkButtons,text="Uncheck all",command=onUncheckAll)


          ## create the labeled radio and entry widgets after having selected the
          ## actual selected values
          query <- paste("SELECT TipoRestrizione AS TypeOfConstraint, ValoreRestrizione AS constraintValue",
                         "FROM B_Restrizioni_multiple",
                         "WHERE NumeroRestrizione =",constraintId
                         )
          result <- sql.get.table(odbcCon, query=query)
          radio.equality <- create_labeledRadio(parent=topWindow,values=c(">=","<="," ="))
          radio.equality[["set.selection"]](value=result[1,"TypeOfConstraint"])
          entry.value <- create_entry(parent=topWindow,value=result[1,"constraintValue"],width="6")

          ## create the insert and cancel buttons
          frame.buttons <- tkframe(parent=topWindow)
          b.update <- create_button(parent=frame.buttons,text="Update",command=onUpdate)
          b.cancel <- create_button(parent=frame.buttons,text="Cancel",command=onCancel)

          ## insert in the window
          ## the tablelist
          tkgrid(tbl.values[["frame"]],padx=padx,pady=pady)
          tkgrid(frame.values,padx=padx,pady=pady,columnspan=2)

          ## the checkAll buttons
          tkgrid(b.checkAll[["button"]],b.uncheckAll[["button"]],padx=padx,pady=pady)
          tkgrid(frame.checkButtons,padx=padx,pady=pady,columnspan=2)

          ## the radio and entry
          tkgrid(radio.equality[["frame"]],padx=padx,pady=pady,row=2,sticky="e")
          tkgrid(entry.value[["entry"]],padx=padx,pady=pady,row=2,column=1,sticky="w")

          ## the insert and cancel buttons
          tkgrid(b.update[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
          tkgrid(frame.buttons,padx=padx,pady=pady,columnspan=2)

          ## set the grab and focus
          tkgrab(topWindow)
          tkfocus(topWindow)

        }
      ## end OnModify


      onRemove <- function()
        {
          ## verify the selection
          if (!verifyMultipleConstrSelection()) return ()

          ## remove from the database
          constraintId <- cb.constraintId[["get.selection"]]()
          query <- paste("DELETE FROM B_Restrizioni_multiple WHERE NumeroRestrizione = ",constraintId)
          result <- sqlCommand(channel=odbcCon,query=query)

          ## use the length function in order to determine if the result of the
          ## expression is numeric(0)
          nb.constraint <- length(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]]))
          if (nb.constraint != 0) nb.constraint <- max(as.numeric(tbl.multipleConstr[["get.columns"]](1)[[1]]))

          ## update the number of constraints
          if (nb.constraint > 1)
            {
              cb.constraintId[["modify.values"]](values=1:(nb.constraint-1))
            }
          else
            {
              cb.constraintId[["modify.values"]](values=NULL)
            }
          cb.constraintId[["set.selection"]](value="")

          ## update the tk widget
          if (nb.constraint > 0)
            {
              ## get the tablelist column of Id
              columnId <- tbl.multipleConstr[["get.columns"]](1)
              toRemove <- columnId[,1] == constraintId

              ## select the rows to keep
              newId <- as.numeric(as.vector(columnId[!toRemove,1]))

              ## compute the indices to remove
              tmp <- 1:length(toRemove)
              toRemove <- tmp[toRemove]

              ## remove from the tablelist widget
              tbl.multipleConstr[["remove.rows"]](toRemove)

              ## if remaining rows adjust the Id
              if (length(newId)>0)
                {
                  ## determine which row has Id > the those removed
                  gtConstraintId <- newId > constraintId
                  if (sum(gtConstraintId) > 0)
                    {
                      newId[gtConstraintId] <- newId[gtConstraintId] - 1
                      tbl.multipleConstr[["set.column"]](1,newId)
                    }
                }
            }
          return()
        } ## end onRemove

      ## end function used by the multiple constraints buttons


      ## 0) construct the main window
      window.constraints <- tktoplevel()
      tktitle(window.constraints) <- "Constraints selection"

      ## Construct the widgets



      ## 2) <currency> constraints
      ## 2.1) get the data
      currencySqlType <- c("nvarchar","float","float")
      names(currencySqlType) <- c("Moneta","Min","Max")
      query <- "ORDER BY Moneta"
      currencyConstr <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="B_monete_selezionate",
                                      requiredFields=list(Currency="Moneta","Min","Max"),
                                      orderBy=query,updateDb=T,pkFieldNames=c("Moneta"),
                                      odbcFieldTypes=currencySqlType)

      ## 2.2) create the tk tablelist widget
      lf.currency <- create_labelFrame(parent=window.constraints,text="Currency constraints",labelanchor="n")
      tbl.currencyConstr <- create_odbcTablelist(parent=lf.currency[["labelFrame"]],data=currencyConstr,withScrollBarX=TRUE,
                                                 alignCols=c("center","right","right"),withScrollBarY=TRUE,width=40,
                                                 height=10,editable=list(Min="Entry",Max="Entry"),
                                                 colFormats=c("no","percent2","percent2"),updateDb=T)


      ## 5) <multiple> constraints
      ## 5.1) get the data
      multipleConstraintsSqlType <- c("int","nvarchar","nvarchar","nvarchar","float")
      names(multipleConstraintsSqlType) <- c("NumeroRestrizione","Moneta","Branche","TipoRestrizione","ValoreRestrizione")
      query <- "WHERE Selezionato=1 ORDER BY NumeroRestrizione,Moneta,Branche"
      multipleConstr <- tcl.get.table(odbcConnection=odbcCon,
                                      tableName="B_Restrizioni_multiple",
                                      requiredFields=list(Id="NumeroRestrizione",Currency="Moneta",Sector="Branche",
                                        Type="TipoRestrizione",Value="ValoreRestrizione"),
                                      orderBy=query,updateDb=F,odbcFieldTypes=multipleConstraintsSqlType)

      ## 5.2) create the tk tablelist widget
      lf.multipleConstr <- create_labelFrame(parent=window.constraints,text="Multiple constraints",labelanchor="n")
      tbl.multipleConstr <- create_odbcTablelist(parent=lf.multipleConstr[["labelFrame"]],data=multipleConstr,withScrollBarX=TRUE,
                                                 alignCols=c("center","center","center","center","right"),withScrollBarY=TRUE,width=60,
                                                 height=40,colFormats=c("no","no","no","no","percent2"))

      ## 5.3) create the button frame and the buttons
      f.multipleConstrButtons <- tkframe(lf.multipleConstr[["labelFrame"]])
      b.New <- create_button(parent=f.multipleConstrButtons,text="New",command=onNew)
      b.Modify <- create_button(parent=f.multipleConstrButtons,text="Modify",command=onModify)
      b.Remove <- create_button(parent=f.multipleConstrButtons,text="Remove",command=onRemove)

      ## determine the number of multiple constraints
      query <- "SELECT MAX(NumeroRestrizione) AS maxid FROM B_Restrizioni_multiple"
      result <- sql.get.table(odbcCon, query=query)
      if (is.na(result[1,1]))
        {
          cb.constraintId <- create_combo(parent=window.constraints,startValue=NA,editable=FALSE,autocomplete=FALSE)
        }
      else
        {
          cb.constraintId <- create_combo(parent=window.constraints,values=c(1:as.numeric(result[1,1])),
                                          startValue=NA,editable=FALSE,autocomplete=FALSE)
        }
      ## fill the button frame
      tkgrid(b.New[["button"]],cb.constraintId[["combo"]],
             b.Modify[["button"]],b.Remove[["button"]],padx=padx,pady=pady)


      ## 6) create the last button frame with the Save and Cancel buttons
      ## 6.1 create the onSave and onCancel functions
      onSave <- function()
        {

          tkmessageBox(message="Data have been saved!",icon="info",type="ok",parent=window.constraints)
          tkdestroy(window.constraints)
          start_window(updateSelection=TRUE)
        }

      onCancel <- function() {tkdestroy(window.constraints); start_window(updateSelection=TRUE)}


      f.mainButtons <- tkframe(window.constraints)
      b.Save <- create_button(parent=f.mainButtons,text="Save",command=onSave)
      b.Cancel <- create_button(parent=f.mainButtons,text="Cancel",command=onCancel)
      tkgrid(b.Save[["button"]],b.Cancel[["button"]],padx=padx,pady=pady)

      ## setup the widgets
      tkgrid(l.client[["label"]],padx=padx,pady=pady,columnspan=3,sticky="we")

      tkgrid(tbl.currencyConstr[["frame"]],padx=padx,pady=pady)
      tkgrid(lf.currency[["labelFrame"]],padx=padx,pady=pady,column=0,row=2,sticky="ns")

      tkgrid(tbl.multipleConstr[["frame"]],padx=padx,pady=pady,sticky="n")
      tkgrid(lf.multipleConstr[["labelFrame"]],padx=padx,pady=pady,column=2,row=1,rowspan=3,sticky="n")
      tkgrid(f.multipleConstrButtons,padx=padx,pady=pady)

      tkgrid(f.mainButtons,padx=padx,pady=pady,columnspan=3,sticky="we")

      ## focus on the constraints windows
      tkgrab(window.constraints)
      tkfocus(window.constraints)

    }
  
  
