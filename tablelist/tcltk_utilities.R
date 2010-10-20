                                        # importa le librerie necessarie
require(tcltk)
tclRequire("BWidget")
tclRequire("Iwidgets")
tclRequire("Tktable")
tclRequire("Tablelist")
require(tkrplot)
source("C:/R/R_tablelist/utilities.R")
#source("/home/tega/Documents/R_tablelist/utilities.R")

                                        # -----------------------------------------------------------------------------
                                        # this procedures have been taken from the tcltk GUI examples from R-site and
                                        # are used by create_table()
tclArrayVar <- function(Rarray=NULL)
{
  if (!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray))!=2)
    stop("Array must be one-dimensional or two-dimensional.")
  require(tcltk)
  n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env)
  name <- paste("::RTcl", n,sep = "")
  l <- list(env = new.env(),nrow=0,ncol=0,ndim=0)
  assign(name, NULL, envir = l$env)
  reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
  class(l) <- "tclArrayVar"
  if (is.null(Rarray))
    {
      ndim <- 2
      .Tcl(paste("set ",name,"(0,0) \"\"",sep=""))
    }
  else
    {
      if (is.vector(Rarray))
        {
          ndim <- 1
          Rarray <- as.data.frame(Rarray)
        }
      else
        ndim <- 2
      for (i in (1:nrow(Rarray)))
        if (ndim==2)
          for (j in (1:ncol(Rarray)))
            .Tcl(paste("set ",name,"(",i,",",j,") \"",paste(Rarray[i,j]),"\"",sep=""))
        else
          .Tcl(paste("set ",name,"(",i,",",1,") \"",paste(Rarray[i,1]),"\"",sep=""))
      if (!is.null(rownames(Rarray)))
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"",rownames(Rarray)[i],"\"",sep=""))
      else
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"\"",sep=""))
      if (!is.null(colnames(Rarray)))
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"",colnames(Rarray)[j],"\"",sep=""))
      else
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"\"",sep=""))
      l$nrow <- nrow(Rarray)
      l$ncol <- ncol(Rarray)
    }
  l$ndim <- ndim
  l
}

assign("[.tclArrayVar",
       function(object, i, j=NULL) {
         require(tcltk)
         if (is.null(j) && object$ndim!=1)
           stop("Object is not a one-dimensional Tcl array")
         if (!is.null(j) && object$ndim!=2)
           stop("Object is not a two-dimensional Tcl array")
         if (object$ndim==1)
           j <- 1
         tclArrayName <- ls(object$env)
         tclvalue(paste(tclArrayName,"(",i,",",j,")",sep=""))
       })

assign("[<-.tclArrayVar",
       function(object, i, j=NULL,value) {
         require(tcltk)
         if (is.null(j) && object$ndim!=1)
           stop("Object is not a one-dimensional Tcl array")
         if (!is.null(j) && object$ndim!=2)
           stop("Object is not a two-dimensional Tcl array")
         if (object$ndim==1)
           j <- 1
         tclArrayName <- ls(object$env)
         .Tcl(paste("set ",tclArrayName,"(",i,",",j,") ",value,sep=""))
         if (i>object$nrow) object$nrow <- i
         return(object)
       })
                                        # end tcltk tclArrayVar -------------------------------------------------------

                                        # tcl/tk constructor functions
                                        # start of the constructor functions
create_label <- function(parent=topWindow,value="")
{
  label <- list()	# create list containing the widget
                                        # create the label
  label[["label"]] <- tklabel(parent,text=value)
  label[["set.label"]] <- function(value)
    {
      tkconfigure(label[["label"]],text=value)
    }
  return(label)
}



create_entry <- function(parent=topWindow,value="",width="40",justify="right",dataFormat=list(type="",option=-1))
{
  entry <- list()  #create list containing the widget
  tkVariable <- tclVar(value)
  entry[["entry"]] <- tkentry(parent,width=width,textvariable=tkVariable,justify=justify)
  entry[["get.value"]] <- function()
  {
   return(tclvalue(tkVariable))
  }
  
  entry[["set.value"]] <- function(newValue="")
    {
    tclvalue(tkVariable) <<- newValue
    }

  outValidation <- function()
    {
      if (dataFormat[["type"]] == "%")
        {
          len <- nchar(tclvalue(tkVariable))
          if (len > 0)
            {
              x <- validate[["double"]](txt=tclvalue(tkVariable),
                                        form=dataFormat[["option"]])
              if (x[[1]])
                {
                  if (dataFormat[["option"]] > -1)
                    {
                      x[[2]] <- round(as.numeric(x[[2]]),digits=dataFormat[["option"]])
                    }
                  previousValue <- paste(x[[2]],"%",sep="")
                  tclvalue(tkVariable)=previousValue
                }
              else
                {
                  topLevel <- tkwinfo("toplevel",entry[["entry"]])
                  tkmessageBox(message="Wrong format!",icon="error",type="ok",parent=topLevel)
                  tclvalue(tkVariable)=previousValue
                }
            }
        }
    }


  inValidation <- function()
    {
      tclvalue(tkVariable) <- chop(tclvalue(tkVariable))
    }
    
    
  if (dataFormat[["type"]] != "")
    {
      tkbind(entry[["entry"]],"<FocusOut>",outValidation)
      tkbind(entry[["entry"]],"<FocusIn>",inValidation)
      previousValue=value
    }

  return(entry)
}


create_feedback <- function(parent=topWindow,label="Status:                   ",
                            steps=10)
{
  feedback <- list()
  labelVariable <- tclVar(label)
  
                                        # create the feedback widget
  feedback[["feedback"]] <- .Tk.subwin(parent)
  .Tcl(paste("iwidgets::feedback",feedback[["feedback"]]$ID))
  
  tkconfigure(feedback[["feedback"]],labelvariable=labelVariable,steps=steps)
  status = 0
  
  feedback[["step"]] <- function (step=1)
    {
      if (status + step > steps)
        {
          step = steps - status
          status <<- steps
        } else {
          status <<- status + step
        }
      .Tcl(paste(feedback[["feedback"]],"step",step))
      return(status)
    }

  feedback[["status"]] <- function (value=status)
    {
      if (value > steps)
        {
          step = steps - status
          status <<- steps
        } else {
          step = value - status
          status <<- value
        }
      
      .Tcl(paste(feedback[["feedback"]],"step",step))
      return(status)
    }

  feedback[["get.status"]] <- function() return(status)
  
  feedback[["reset"]] <- function ()
    {
      tcl(feedback[["feedback"]],"reset")
      return()
    }
  
  feedback[["set.label"]] <- function(label=label, sameWidth=T)
    {
      if (sameWidth)
        {
          oldWidth <- nchar(tclvalue(labelVariable))
          tmp <- paste(rep(" ",oldWidth),collapse="")
          label <- substr(paste(label,tmp,sep=""),1,oldWidth)
        }

      tclvalue(labelVariable) <- label
    }
  
  return(feedback)
}



create_tkplot <- function(parent=topWindow,func=function(){plot(1:10)},hscale=1,
                          vscale=1)
{
  tkplot <- list()
  tkplot[["tkplot"]] <- tkrplot(parent,fun=func,hscale=hscale,vscale=vscale)
  tkplot[["hscale"]] <- hscale
  tkplot[["vscale"]] <- vscale
  tkplot[["replot"]] <- function(func,hscale=tkplot$hscale,vscale=tkplot$vscale)
    {
      if (missing(func))
        {
          tkrreplot(tkplot[["tkplot"]],hsc=hscale,vsc=vscale)
        } else {
          tkrreplot(tkplot[["tkplot"]],fun=func,hsc=hscale,vsc=vscale)
        }
    }
  return(tkplot)
}


create_panedwindow <- function(parent=topWindow,width="300",height="200",
                               fraction,orient="vertical")
{
  # don't use grid, pack or place to control widget in the panes. In that case
  # create a frame in put the frame in the pane and the widgets in the frame

  panedWindow <- list()
  panedWindow[["panedWindow"]] <- .Tk.subwin(parent)
  .Tcl(paste("iwidgets::panedwindow",panedWindow[["panedWindow"]]$ID,"-width",
             width,"-height",height))
  
                                        # calculate the number of panes dipending on the lenght of the vector
                                        # fraction. If fraction is missing, then only one pane is assumed.
  
  if (missing(fraction))
    {
      nbPanes = 1
    } else {
      nbPanes = length(fraction)
    }
  
  for (i in 1:nbPanes)
    {
      paneName = paste("p",i,sep="")
      tmp <- as.character(tkadd(panedWindow[["panedWindow"]], paneName))
      panedWindow[[i+1]] <- .Tk.subwin(parent)
      panedWindow[[i+1]]$ID <- tmp
    tkgrid(tklabel(panedWindow[[i+1]],text="put some"))
    }
  
  tmp <- paste(fraction,collapse=" ")
  .Tcl(paste(panedWindow[["panedWindow"]]$ID, "fraction", tmp))
  tkconfigure(panedWindow[["panedWindow"]],orient=orient)
  rm(tmp)
  return(panedWindow)
}


create_button <- function(parent=topWindow,text="Ok",width=max(nchar(text),10),command=onClick)
{
  button <- list() # create list containing the widget
  button[["button"]] <- tkbutton(parent,text=text,width=width,command=command)
  button[["set.label"]] <- function(text="Ok")
    {
      tkconfigure(button[["button"]],text=text)
    }
  return(button)
}



create_radio <- function(parent=topWindow,values=c(""),position=NULL)
{
  nb.radios <- length(values)
  variable <- tclVar(values[1])
  radio <- list()	# list containing the widgets
                                        # create the buttons
  for (i in 1:nb.radios)
    {
      radio[[i]] <- tkradiobutton(parent)
      tkconfigure(radio[[i]],variable=variable,value=values[i])
    }
  
  radio[["nb.radios"]] = nb.radios
  
  radio[["get.selection"]] <- function()
    {
      return(tclvalue(variable))
    }
  
  radio[["set.selection"]] <- function(new.value)
    {
      tclvalue(variable) <<- new.value
    }
  
  radio[["position"]] <- position
  return(radio)
}


create_labeledRadio <- function(parent=topWindow,values=c(""),labs=values,position=NULL)
{
  nb.radios <- length(values)
  variable <- tclVar(values[1])
  radio <- list()	# list containing the widgets
  radio[["frame"]] <- tkframe(parent)
  
                                        # create the buttons
  j = 2
  for (i in 1:nb.radios)
    {
      radio[[j]] <- tklabel(radio[["frame"]],text=labs[i])
      radio[[j+1]] <- tkradiobutton(radio[["frame"]])
      tkconfigure(radio[[j+1]],variable=variable,value=values[i])
      tkgrid(radio[[j+1]],radio[[j]])
      tkgrid.configure(radio[[j]],sticky="w")
      j = j + 2
    }
  
  radio[["nb.radios"]] = nb.radios
  
  radio[["get.selection"]] <- function()
    {
      return(tclvalue(variable))
    }
  
  radio[["set.selection"]] <- function(new.value)
    {
      tclvalue(variable) <<- new.value
    }
  
  radio[["position"]] <- position
  return(radio)
}


create_combo <- function(parent=topWindow,values=c(""),startValue=NA,editable=FALSE)
{
  nb.elements <- length(values)
                                        # compute the longest name
  cb.width = max(nchar(values)) + 1

  combo <- list()  # create list containing the widget
  combo[["combo"]] <- tkwidget(parent,"ComboBox",editable=editable,values=values)

  textVariable = tclVar()
  if (!is.na(startValue))
    {
      values = union(startValue,values)
      tclvalue(textVariable) = startValue
    }
    
  tkconfigure(combo[["combo"]], width=cb.width, textvariable=textVariable)
  
  combo[["get.selection"]] <- function()
    {
      # return(values[as.numeric(tclvalue(tkcmd(combo[["combo"]],"getvalue")))+1])
      return(tclvalue(textVariable))
    }

  combo[["get.id"]] <- function()
    {
      return(as.numeric(tclvalue(tkcmd(combo[["combo"]],"getvalue")))+1)
    }
    
  combo[["get.tclId"]] <- function()
    {
      return(tclvalue(tkcmd(combo[["combo"]],"getvalue")))	
    }
  
  combo[["set.selection"]] <- function(new.value)
    {
      tclvalue(textVariable) <<- new.value
    }

  combo[["modify.values"]] <- function(new.values)
    {
      tkconfigure(combo[["combo"]], values=new.values)
      values <<- new.values
    }
    
  return(combo)
}




                                        # start tablelist widget
                                        # da terminare
listboxOptionWindow <- function(listbox,parent)
{
  padx=10
  pady=5
  top <- tktoplevel()

  frameSelectionMode <- tkframe(top)
  label1 <- create_label(parent=frameSelectionMode,
                         value="Selection mode:")
  radio1 <- create_labeledRadio(parent=frameSelectionMode,
                                values=c("single","browse","multiple","extended"))

  tkgrid(label1[[1]])
  tkgrid(radio1[["frame"]])
  tkgrid(frameSelectionMode,columnspan=2,padx=padx,pady=pady)

  onOk <- function()
    {
      tkconfigure(listbox[["listbox"]],selectmode=radio1[["get.selection"]]())
      tkdestroy(top)
      tkraise(parent)
      tkfocus(parent)
    }
  onCancel <- function() tkdestroy(top)

  ok <- create_button(parent=top,command=onOk)
  cancel <- create_button(parent=top,text="Cancel",command=onCancel)

  tkgrid(ok[[1]],cancel[[1]],padx=padx,pady=pady)

  tkraise(top)
  tkfocus(top)
  tkgrab(top)
}


create_listbox <- function(parent=topWindow,values=NULL,mode="single",
                           height=6,width=8,withScrollBarX=FALSE,
                           withScrollBarY=FALSE,showDefault=0,background="linen")
{
  nb.elements <- length(values)
  if (nb.elements > 0)
    {
      listvariable <- tclVar(values)
    }
  else
    {
      listvariable <- tclVar()
    }
  tclListVariableName <- as.character(listvariable)
  listbox <- list() # create list containing the widget
  listbox[["frame"]] <- tkframe(parent)
  listbox[["listbox"]] <- tklistbox(listbox[["frame"]],listvariable=listvariable,
                          height=height,width=width,selectmode=mode,background=background)
  tclListboxName <- as.character(listbox[["listbox"]])
  tkgrid(listbox[["listbox"]],row=0,column=0)

  if (withScrollBarY)
    {
      yscr <- tkscrollbar(listbox[["frame"]], repeatinterval=5,
                                       command=function(...)tkyview(listbox[["listbox"]],...))
      tkconfigure(listbox[["listbox"]],yscrollcommand=function(...) tkset(yscr,...))
      tkgrid(yscr,row=0,column=1,sticky="ns")
    }

  if (withScrollBarX)
    {
      xscr <- tkscrollbar(listbox[["frame"]], repeatinterval=5,orient="horizontal",
                                       command=function(...)tkxview(listbox[["listbox"]],...))
      tkconfigure(listbox[["listbox"]],xscrollcommand=function(...) tkset(xscr,...))
      tkgrid(xscr,row=1,column=0,sticky="ew")
    }

  listbox[["set.values"]] <- function(newValues,setOrder=FALSE)
    {
      nb <- length(newValues)
      if (is.na(newValues[1]))
        {
          tclvalue(listvariable) <- ""
          return()
        }

      if (setOrder)
        {
          values <<- newValues[order(newValues)]
        }
      else
        {
          values <<- newValues
        }
      if (nb==1)
        {
          tkinsert(listbox[["listbox"]],"0",paste("{",newValue,"}",sep=""))
        }
      else
        {
          tclvalue(listvariable) <<- newValues
        }
    }

                                        # set the default values, if any
  if (showDefault != 0) 
    {
      if (lenth(showDefault)==1)
        {
          tkselection.set(listbox[["listbox"]],showDefault-1)
        } else {
          for (i in showDefault)
            {
              tkselection.set(listbox[["listbox"]],i-1) # index starts at zero
            }
        }	
    }
    
  listbox[["reset"]] <- function()
    {
      nb <- length(values)-1
      values <<- NULL
      tclvalue(listvariable) <<- ""
      if (nb >= 0)
        {
          .Tcl(paste(tclListboxName,"selection clear 0",nb))
        }
    }

  listbox[["add.values"]] <- function(addValues,position="end",setOrder=FALSE)
    {
      if (setOrder)
        {
          tmp <<- c(values,addValues)
          values <<- tmp[order(tmp)]
          nb <- length(values)
          if (nb > 1)
            {
              tclvalue(listvariable) <<- values
            }
          else
            {
              tkinsert(listbox[["listbox"]],"end",values)
            }
        }
      else
        {
          tmp = paste("{",addValues,"}",sep="")
          for (value in tmp)
            {
              tkinsert(listbox[["listbox"]],"end",value)
            }
          values <<- c(values,addValues)
        }
    }
    
  listbox[["get.id.selection"]] <- function()
    {
      return (as.integer(tkcurselection(listbox[["listbox"]]))+1)
    }
  
  listbox[["get.selection"]] <- function()
    {
      return (values[as.integer(tkcurselection(listbox[["listbox"]]))+1])
    }
  
  listbox[["get.nb.selected"]] <- function()
    {
      return (length(as.integer(tkcurselection(listbox[["listbox"]]))))
    }
  
  listbox[["delete.elements"]] <- function(index)
    {
      for (i in (sort(index,decreasing=TRUE)))
        {
          tkdelete(listbox[["listbox"]],i-1)
          values <<- values[-i]
        }
    }
  
  listbox[["delete.selected"]] <- function()
    {
      index <- listbox[["get.id.selection"]]()
      listbox[["delete.elements"]](index)
    }
    
  listbox[["values"]] <- function() return(values)
  
  optionWindow <- function() listboxOptionWindow(listbox=listbox,parent=parent)
  tkbind(listbox[["listbox"]],"<Button-3>",optionWindow)
  return (listbox)
}



create_selectionListBox <- function(parent=topWindow,title="",labels=list(label1="Available",label2="Selected"),
                                    values1=NULL,values2=NULL,mode="single",
                                    height=8,width=12,withScrollBarX=FALSE,
                                    withScrollBarY=FALSE,grabWindow)
  {
    #nb.elements <- length(values1)
    #if (nb.elements > 0)
    #  {
    #    namesToIndex <- 1:nb.elements
    #    names(namesToIndex) <- universe
    #  }
  
    listbox <- list() # create list containing the widget
    listbox[["frame"]] <- tkframe(parent)
  
    # check that values2 is not empty
    nbValues2 <- length(values2)
    if (nbValues2==0)
      {
        listbox1 <-  create_listbox (parent=listbox[["frame"]],values=values1,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             withScrollBarY=withScrollBarY,background="#e0e8f0")

        listbox2 <-  create_listbox (parent=listbox[["frame"]],values=values2,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             withScrollBarY=withScrollBarY)
        rm(nbValues2)
      }
    else
      {
        # determine the subset of non selected elements
        values2 <- intersect(values1,values2)
        values1 <- setdiff(values1,values2)
        listbox1 <-  create_listbox (parent=listbox[["frame"]],values=values1,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             withScrollBarY=withScrollBarY,background="#e0e8f0")

        listbox2 <-  create_listbox (parent=listbox[["frame"]],values=values2,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             withScrollBarY=withScrollBarY)
      }

    labelTitle <- tklabel(listbox[["frame"]],text=title)
    label1 <- tklabel(listbox[["frame"]],text=labels[["label1"]])
    label2 <- tklabel(listbox[["frame"]],text=labels[["label2"]])

    onInsert <- function()
      {
        # get the list of selected elements
        selection <- listbox1[["get.selection"]]()
        if (length(selection) == 0)
          {
            tkmessageBox(message="No items selected",icon="error",type="ok",parent=grabWindow)
            return()
          }
        # add the selection to the listbox2
        listbox2[["add.values"]](addValues=selection,setOrder=TRUE)
        
        # remove the selection from the listbox1
        listbox1[["delete.selected"]]()
        #selected <<- listbox2[["values"]]()
        #available <<- setdiff(available, selected)
        #available <<- listbox1[["values"]]()
      }
    
    onRemove <- function()
      {
        # get the list of selected elements
        selection <- listbox2[["get.selection"]]()
        if (length(selection) == 0)
          {
            tkmessageBox(message="No items selected",icon="error",type="ok",parent=grabWindow)
            return()
          }
        # add the selection to the listbox1
        listbox1[["add.values"]](addValues=selection,setOrder=TRUE)

        # remove the selection from the listbox2
        listbox2[["delete.selected"]]()
        #selected <<- listbox2[["values"]]()
        #available <<- setdiff(available, selected)
        #available <<- listbox1[["values"]]()
      }

    listbox[["get.available"]] <- function() return(listbox1[["values"]]())
    listbox[["get.selected"]] <- function() return(listbox2[["values"]]())
    listbox[["get.universe"]] <- function()
      {
        return(union(listbox1[["values"]](),listbox2[["values"]]()))
      }
    listbox[["reset"]] <- function(widget=1)
      {
        if (widget==1) listbox1[["reset"]]()
        if (widget==2) listbox2[["reset"]]()
      }
    listbox[["set.values"]] <- function(newValues=values,setOrder=FALSE,widget=1)
      {
        if (widget == 1)
          {
            listbox1[["set.values"]](newValues=newValues,setOrder=setOrder)
          }
        else
          {
            listbox2[["set.values"]](newValues=newValues,setOrder=setOrder)
          }
      }

        
    b.insert <- create_button(parent=listbox[["frame"]],text="Insert >>",command=onInsert)
    b.remove <- create_button(parent=listbox[["frame"]],text="<< Remove",command=onRemove)

    if (title != "")
      {
        tkgrid(labelTitle,columnspan=2)
      }
    tkgrid(label1,label2)
    tkgrid(listbox1[["frame"]],listbox2[["frame"]],padx=10)
    tkgrid(b.insert[["button"]],b.remove[["button"]],pady=5)
    return(listbox)
  }
  
  
  
                                        # start table widget
create_table <- function(parent=topWindow,tclArray,height=10,width=10,
                         selectmode="extended",withScrollBarX=FALSE,withScrollBarY=FALSE,
                         titlerows="1", titlecols="1",background="linen")
{
  tclArrayName <- ls(tclArray$env)
  table <- list() # create list containing the widget
  table[["frame"]] <- tkframe(parent)
  table[["table"]] <- tkwidget(table[["frame"]],"table",rows=paste(tclArray$nrow+1),
                               cols=paste(tclArray$ncol+1),titlerows=titlerows,
                               titlecols=titlecols,height=paste(height+1),
                               width=paste(width+1))
  
  
    if (withScrollBarY)
    {
      yscr <- tkscrollbar(table[["frame"]], repeatinterval=5,
                                       command=function(...)tkyview(table[["table"]],...))
      tkconfigure(table[["table"]],yscrollcommand=function(...) tkset(yscr,...))
      tkgrid(yscr,row=0,column=1,sticky="ns")
    }

  if (withScrollBarX)
    {
      xscr <- tkscrollbar(table[["frame"]], repeatinterval=5,orient="horizontal",
                                       command=function(...)tkxview(table[["table"]],...))
      tkconfigure(table[["table"]],xscrollcommand=function(...) tkset(xscr,...))
      tkgrid(xscr,row=1,column=0,sticky="ew")
    }


  tkconfigure(table[["table"]],rowseparator="\"\n\"",colseparator="\"\t\"")
  tkconfigure(table[["table"]],variable=tclArrayName,background=background,
              selectmode=selectmode)
  return(table)
}


create_checkButton <- function(parent=topWindow,default="0",position=NULL)
{
  value <- tclVar(default)
  checkButton <- list() # create list containing the widget
  checkButton[["checkButton"]] <- tkcheckbutton(parent,variable=value)
  
  checkButton[["get.value"]] <- function()
    {
      return(as.character(tclvalue(value)))
    }

  checkButton[["set.value"]] <- function(x)
    {
      tclvalue(value) <- x
    }
  
  checkButton[["position"]] <- position
  return(checkButton)
}


                                        # start tablelist widget
                                        # da terminare
tablelistOptionWindow <- function(tablelist,parent)
{
  padx=10
  pady=5
  top <- tktoplevel()

  frameSelectionMode <- tkframe(top)
  label1 <- create_label(parent=frameSelectionMode,
                         value="Selection mode:")
  radio1 <- create_labeledRadio(parent=frameSelectionMode,
                                values=c("single","browse","multiple","extended"))
  
  tkgrid(label1[[1]])
  tkgrid(radio1[["frame"]])
  tkgrid(frameSelectionMode,columnspan=2,padx=padx,pady=pady)
  
  onOk <- function()
    {
      tkconfigure(tablelist[["tablelist"]],selectmode=radio1[["get.selection"]]())
      tkdestroy(top)
      tkraise(parent)
      tkfocus(parent)
    }
  onCancel <- function() tkdestroy(top)
  
  ok <- create_button(parent=top,command=onOk)
  cancel <- create_button(parent=top,text="Cancel",command=onCancel)
  
  tkgrid(ok[[1]],cancel[[1]],padx=padx,pady=pady)
  
  tkraise(top)
  tkfocus(top)
  tkgrab(top)
}
                                        # da terminare

                                        # tcl format functions and corresponding
                                        # R list with the tcl function name
tclColFormat <- list()
.Tcl("proc formatRound0 {arg} {return [expr round({$arg})]}")
.Tcl("proc formatRound1 {arg} {set x 10.0; return [expr {round([expr {$arg * $x}]) / $x}]}")
.Tcl("proc formatRound2 {arg} {set x 100.0; return [expr {round([expr {$arg * $x}]) / $x}]}")
.Tcl("proc formatRound3 {arg} {set x 1000.0; return [expr {round([expr {$arg * $x}]) / $x}]}")
.Tcl("proc formatRound4 {arg} {set x 10000.0; return [expr {round([expr {$arg * $x}]) / $x}]}")
.Tcl("proc formatRound5 {arg} {set x 100000.0; return [expr {round([expr {$arg * $x}]) / $x}]}")
.Tcl("proc formatRound6 {arg} {set x 1000000.0; return [expr {round([expr {$arg * $x}]) / $x}]}")

tclColFormat[["round0"]] <- "formatRound0"
tclColFormat[["round1"]] <- "formatRound1"
tclColFormat[["round2"]] <- "formatRound2"
tclColFormat[["round3"]] <- "formatRound3"
tclColFormat[["round4"]] <- "formatRound4"
tclColFormat[["round5"]] <- "formatRound5"
tclColFormat[["round6"]] <- "formatRound6"

.Tcl("proc formatPercent {arg} {return [expr {$arg * 100}]\%}")
.Tcl("proc formatPercent0 {arg} {return [expr {[formatRound2 $arg] * 100}]\%}")
.Tcl("proc formatPercent1 {arg} {return [expr {[formatRound3 $arg] * 100}]\%}")
.Tcl("proc formatPercent2 {arg} {return [expr {[formatRound4 $arg] * 100}]\%}")
.Tcl("proc formatPercent3 {arg} {return [expr {[formatRound5 $arg] * 100}]\%}")
.Tcl("proc formatPercent4 {arg} {return [expr {[formatRound6 $arg] * 100}]\%}")

tclColFormat[["percent"]] <- "formatPercent"
tclColFormat[["percent0"]] <- "formatPercent0"
tclColFormat[["percent1"]] <- "formatPercent1"
tclColFormat[["percent2"]] <- "formatPercent2"
tclColFormat[["percent3"]] <- "formatPercent3"
tclColFormat[["percent4"]] <- "formatPercent4"

.Tcl('proc emptyString val { return "" }')
tclColFormat[["logical"]] <- "emptyString"

                                        # set the validate functions
validate <- list()
validate[["integer"]] <- function(txt,form)
  {
    # remove leading and trailing spaces
    x <- sub(' *$','',sub('^ *','',txt))
    if (regexpr('^-?[0-9]{1,}$',x)[1] > 0) return(list(TRUE,x))
    return(list(FALSE))
  }
validate[["double"]] <- function(txt,form)
  {
    # remove leading and trailing spaces
    x <- sub(' *$','',sub('^ *','',txt))
    if (regexpr('^-?[0-9]*\\.?[0-9]*$',x)[1] > 0) return(list(TRUE,x))
    return(list(FALSE))
  }
validate[["logical"]] <- function(txt,form)
  {
    x <- sub(' *$','',sub('^ *','',txt))
    if (x=="1" | x=="0") return(list(TRUE,x))
    return(list(FALSE))
  }
validate[["character"]] <- function(txt,form)
  {
    return(list(TRUE,txt))
  }
validate[["date"]] <- function(txt,form)
  {
    d <- unlist(strsplit(txt,"-"))
    l <- nchar(d)
    
                                        # check the year part
    if (regexpr('^[0-9]{4}$',d[1])[1] < 0) return(list(FALSE,txt))
    
                                        # check the month part
    if (regexpr('^[0-1]?[0-2]$',d[2])[1] < 0) return(list(FALSE,txt))
    if (l[2] == 1) d[2] <- paste("0",d[2])
    
                                        # check the day part
    if (regexpr('^[0-3]?[0-9]$',d[3])[1] < 0) return(list(FALSE,txt))
    if (l[3] == 1) d[3] <- paste("0",d[3])
    
    return(list(TRUE,paste(d[1],d[2],d[3],sep="-")))
  }


create_odbcTablelist <- function(
                             parent=topWindow,data=data,width=60,height=15,
                             alignCols=rep("center",ncols),alignColsLabel=alignCols,
                             withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=F,
                             multiSortColumn=T,selectmode="single",
                             editable=rep("no",ncols),editStartValue=list(),
                             colFormats=rep("no",ncols),color="standard",updateDb=F
                             )
{
  parentFrame <- parent.frame()
  frameName <- deparse(substitute(data))

  # setup the rTypes, tclTypes of the requiredFields
  rTypes <- data[["rTypes"]][data$requiredFields]

  # possible tclTypes: "character","date","double","integer","logical"
  tclTypes <- data[["tclTypes"]][data$requiredFields]
  
  tablelist <- list(tablelist="") # create list containing the widget
  
                                        # create the frame containing the tablelist and the vert. horiz. scrollbar
  tablelist[["frame"]] <- tkframe(parent)
  
                                        # create the Tk.ID of the tablelist
  tablelist[["tablelist"]] <- .Tk.subwin(tablelist[["frame"]])
  .Tcl(paste("tablelist::tablelist",tablelist[["tablelist"]]$ID))
  tkgrid(tablelist[["tablelist"]],row=0,column=0)
  
                                        # create a variable with the tcl name of the tablelist widget
  tclTableName <- tablelist[["tablelist"]]$ID
  
                                        # get the dimensions of the dataFrame
  dimension <- dim(data[["dataFrame"]])
  nrows <- dimension[1]
  ncols <- length(data[["requiredFields"]])
  rm(dimension)
  
                                        # create the listvariable
  tablelist[["listvariable"]] <- tclVar()
  tkconfigure(tclTableName, listvariable=tablelist[["listvariable"]])
  
                                        # create the columns and the columnlabels
  columnlabels <- data[["requiredColumns"]]
  tablelist[["columnlabels"]] <- columnlabels
  columns <- paste(rep("0 ",ncols),"{",columnlabels,"} ",alignCols,sep="",collapse=" ")
  .Tcl(paste(tclTableName,"insertcolumns end",columns))
  
                                        # setup the editability
  already=list(checkbutton=F,Entry=T,SpinBox=T,ComboBox=T,no=F)
  execute=list(
    Entry="tablelist::addBWidgetEntry",
    SpinBox="tablelist::addBWidgetSpinBox",
    ComboBox="tablelist::addBWidgetComboBox",
    )
  # if editable is a list than modify it as vector
  if (is.list(editable))
  {
    editableTmp <- rep("no",ncols)
    names(editableTmp) <- data[["requiredFields"]]
    tmp <- names(editable)
    for (i in 1:length(tmp))
    {
      editableTmp[tmp[i]] = editable[[i]]
    }
    editable <- editableTmp
    rm(editableTmp)
  }
  for (i in 1:ncols)
    {
      if (already[[editable[i]]])
        {
          .Tcl(execute[[editable[i]]])
          already[[editable[i]]] <- F
        }
    }
  rm(already,execute)
  
                                         # create the variable containing the check/uncheckbox images

  .Tcl(paste("set dir C:/R/R_tablelist"))
  .Tcl(paste("set checkedImg [image create photo -file [file join $dir checked.gif]]"))
  .Tcl(paste("set uncheckedImg [image create photo -file [file join $dir unchecked.gif]]"))
  checkedImg = c("$::uncheckedImg","$::checkedImg")
  names(checkedImg) = c("0","1")
  updateImage <- function(row,col,text)
    {
	    .Tcl(paste(tclTableName," cellconfigure ",row,",",col," -image ",checkedImg[text],sep=""))
    }

                                        # align the columnlabels and assign the formatComd
  editCommandsRequired=F
  for (i in 1:ncols)
    {                                   # assign the columnnames
      .Tcl(paste(tclTableName," columnconfigure ", i-1,
                 " -name \"",columnlabels[i],
                 "\" -labelalign ",alignColsLabel[i],sep=""))
      if (editable[i] !="no")
        {
          .Tcl(paste(tclTableName,"columnconfigure", i-1, "-editable yes -editwindow",editable[i]))
          editCommandsRequired <- T
        }
      
      if (colFormats[i]!="no")
        {
          .Tcl(paste(tclTableName,"columnconfigure", i-1, "-formatcommand",tclColFormat[[colFormats[i]]]))
        }
    }
  
  if (editCommandsRequired)
    {
                                        # prepare the editstartcommand function
      editStartCmd <- function(...)
        {
          arg <- list(...)
          names(arg) <- c("tbl","row","col","text")
                                        # get the tclname of the tcl widget used for editing
          tclWidgetName <- as.character(.Tcl(paste(arg[["tbl"]],"editwinpath")))
          
          selectedColumn <- as.numeric(arg[["col"]]) + 1
          
                                        # configure the corresponding widget
          if (editable[selectedColumn] == "ComboBox") {
            v <- paste("{",editStartValue[[columnlabels[selectedColumn]]],"}",sep="",collapse=" ")
            .Tcl(paste(tclWidgetName,"configure -values {",v,"}"))
            .Tcl(paste(tclWidgetName,"configure -editable true"))
          }
                                        # save the txt variable in a tcl variable
          .Tcl(paste("return [",arg[["tbl"]]," cellcget ",arg[["row"]],",",
                     arg[["col"]]," -text]",sep=""))
        }
      
      tkconfigure(tablelist[["tablelist"]],editstartcommand=editStartCmd)
    }
  
  
  if (editCommandsRequired)
    {
     # construct the vector of quotes for the pk values
     needQuote <- sqlNeedQuote(data[["odbcConnection"]])
     pkQuotes <- needQuote[data[["odbcTypes"]][data$pkFieldNames]]

     # construct the vector of pkNames to be used in the query
     PkNamesForQuery = paste("[",data[["pkFieldNames"]],"]=",pkQuotes,sep="")

     # construct the vector of positions of the pkNames within the
     # fieldNames vector
     tmp <- 1:length(data[["fieldNames"]])
     names(tmp) <- data[["fieldNames"]]
     pkFieldPositions <- tmp[data[["pkFieldNames"]]]
     rm(tmp)

                                        # define the EditEndCmd
      editEndCmd <- function(...)
        {
                                        # get the argument list
          arg <- list(...)
          names(arg) <- c("tbl","row","col","text")
                                        # set the column number and column type
          column <- as.numeric(arg[["col"]])+1
          rColType = rTypes[column]
          tclColType = tclTypes[column]
          
                                        # validate the input

          validResult <- validate[[tclColType]](arg[["text"]],colFormats[column])

                                        # if the input is valid write it to the original table
          if (validResult[[1]])
          {
            b = "\""
            selectedRowName <- as.character(.Tcl(paste(arg[["tbl"]],"rowcget",arg[["row"]],"-name")))
            selectedColName <- as.character(.Tcl(paste(arg[["tbl"]],"columncget",arg[["col"]],"-name")))
            selectedColName <- paste(selectedColName,collapse=" ")
            
            # update the image if colFormats is logical
            if (colFormats[column] == "logical") updateImage(arg[["row"]],arg[["col"]],arg[["text"]])

            if (updateDb)
            {
              # retrive the pk values
              command <- parse(text=paste(frameName,"$dataFrame[",b,selectedRowName,b,", ]",
                               sep=""))
              pkValues <- eval(command, env=parentFrame)[1,pkFieldPositions]

              # construct the update query
              whereQueryPart = paste(PkNamesForQuery,pkValues,pkQuotes,sep="")
              whereQueryPart = paste(whereQueryPart,collapse=" AND ")
              quotes <- needQuote[data[["odbcTypes"]][column]]

              query <- paste("UPDATE ",data[["tableName"]]," SET [",
                 data[["requiredFields"]][column],"] = ",quotes,
                 validResult[[2]],quotes," WHERE ",whereQueryPart,sep="")
              errorText <- sqlCommand(data[["odbcConnection"]],query)
            }

            if (errorText==1)
            {
                                        # return the tcl value the the external "data" data.frame
              if (rColType=="character") c = "\"" else c=""
              command <- parse(text=paste(frameName,"$dataFrame[",b,selectedRowName,b,",",b,
                               selectedColName,b,"] <- ",c,validResult[[2]],c,sep=""))
              eval(command, env=parentFrame)

                                        # return the tcl value the the internal "data" data.frame
              data[["dataFrame"]][selectedRowName,selectedColName] <- as.character(validResult[[2]])
            
                                        # return the validated value to tcl
              tcltmp <- tclVar(validResult[[2]])
              .Tcl(paste("return $",as.character(tcltmp),sep=""))
            }
            else
            {
              .Tcl(paste(tclTableName,"rejectinput"))
              .Tcl(paste(tclTableName,"cancelediting"))
              if (colFormats[column] == "logical") updateImage(arg[["row"]],arg[["col"]],arg[["text"]])
            }
          }
          else
          {
              .Tcl(paste(tclTableName,"rejectinput"))
              .Tcl(paste(tclTableName,"cancelediting"))
              if (colFormats[column] == "logical") updateImage(arg[["row"]],arg[["col"]],arg[["text"]])
          }
        }

                                        # setup the EditEndCommand
      tkconfigure(tablelist[["tablelist"]],editendcommand=editEndCmd)
    }
  

  tablelist[["insert.data.frame"]] <- function(dataFrame=data,position="end",decoration=1)
    {
      # columnNames are not used in this function. The data are only inserted into
      # the tablelist.
      if (is.data.frame(dataFrame))
        {
          nrows=dim(dataFrame)[[1]]
        }
      else
        {
          requiredColumns = dataFrame[["requiredColumns"]]
          if (length(requiredColumns) == 1)
            {
             dataFrame=data.frame(I(dataFrame[["dataFrame"]][,requiredColumns]))
             nrows=dim(dataFrame)[[1]]
            }
          else
            {
              dataFrame=dataFrame[["dataFrame"]][,requiredColumns]
              nrows=dim(dataFrame)[[1]]
            }
        }

      if (dim(dataFrame)[[2]] != ncols)
        {
          tkmessageBox(message = "Incorrect number of columns!",icon = "error",
                       type = "ok")
          return(invisible())
        }
      if (position != "end")
        {
          if (is.numeric(position))
            {
              position = max(0,as.integer(position - 1))
            } else {
              tkmessageBox(message = "Position must be \"end\" or a positive integer!",
                           icon = "error", type = "ok")
              return(invisible())
            }
        }
                       # verify that the dataframe is not empty
      removeFirst = FALSE
      if (all(is.na(dataFrame)))
        {
          dataFrame[1,]= rep("",ncols)
          nrows = 1
          removeFirst=TRUE
        }

                                        # assign the values to the listvariable
      tmp <- paste("{",dataFrame[,1], sep="")
      if (ncols > 1)
        {
          for (i in 2:ncols)
            {
              tmp <- paste(tmp,dataFrame[,i], sep="} {")
            }
        }
      tmp <- paste(tmp,"}",sep="")
      tmp <- paste(tmp,collapse="} {")
      
                                        #.Tcl(paste("set ", as.character(tablelist[["listvariable"]])," {{",tmp,"}}", sep=""))
      .Tcl(paste("set ", as.character(tablelist[["listvariable"]])," [list {",tmp,"} ]", sep=""))
      
                                        # configure the rownames
      rnames <- dimnames(dataFrame)[[1]]
      for (i in 1:nrows)
        {
          .Tcl(paste(tclTableName,"rowconfigure",i-1,"-name", rnames[i]))
        }
      if (removeFirst)
        {
          .Tcl(paste(tclTableName,"delete 0 0"))
          return()
        }
                                                    # check if necessary to change set the image in a logical field
      for (i in 1:ncols)
        {
          if (colFormats[i]=="logical")
            {
              colValues <- dataFrame[,i]
              for (j in 1:nrows)
                {
                  updateImage(j-1,i-1,colValues[j])
                }
            }
        }
    }
  
                                        # define the rowcount variable used to construct the labelnames. This variable
                                        # is very because new raws are named with the "r"&rowcount convention
  rowcount <- nrows
  tablelist[["get.rowcount"]] <- function() return(rowcount)
  tablelist[["set.rowcount"]] <- function(value) {rowcount <<- value}
  
                                        #.Tcl("option add *Font \"Helvetica -36\"")
                                        # set the decoration
  if (color=="standard")
    {
      tkconfigure(tablelist[["tablelist"]],background="linen",selectbackground="navy",
              selectforeground="white",stripebackground="#e0e8f0",showseparators="yes")
    }
  else
    {
      tkconfigure(tablelist[["tablelist"]],background=color,showseparators="yes")
    }
                                        # define the sortmode of the columns
  if (sortColumn)
    {
      tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::sortByColumn")
                                              # get the sortmode of the data before the convertion to character
      sortMode <- get.tclSortModes(tclTypes)
      sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),
        "-sortmode",sortMode)
      .Tcl(paste(sortMode,collapse=" ; "))
    }
  
  
                                        # define the addToSortColumns command
  if (multiSortColumn & !sortColumn)
    {
      tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::addToSortColumns")
      if (!sortColumn)
        {
          sortMode <- get.tclSortModes(tclTypes)
          sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),
            "-sortmode",sortMode)
          .Tcl(paste(sortMode,collapse=" ; "))
        }
    }

  if (multiSortColumn | sortColumn)
    {
      resetSortInfo <- function()
        {
          .Tcl(paste(tclTableName,"resetsortinfo"))
        }
      tkbind(tclvalue(.Tcl(paste(tclTableName, "bodytag"))),
             "<Double-Button-1>",resetSortInfo)
    }
                                        # define the optionWindow used with button 3
  optionWindow <- function() tablelistOptionWindow(tablelist,parent=parent)
  
  tkbind(tclvalue(.Tcl(paste(tclTableName, "bodytag"))),
         "<Button-3>",optionWindow)
  
  
                                        # insert the values in the tablelist
  tablelist[["insert.data.frame"]]()
  
                                        # other configurations options
  tkconfigure(tablelist[["tablelist"]], height=height, width=width, stretch="all",
              selectmode=selectmode)

  if (withScrollBarX)
    {
      xscr <- tkscrollbar(tablelist[["frame"]],orient="horizontal", command=function(...)tkxview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],xscrollcommand=function(...) tkset(xscr,...))
      tablelist[["xscr"]] <- xscr
      tkgrid(tablelist[["xscr"]],row=1,column=0,sticky="ew")
    }
  
  if (withScrollBarY)
    {
      yscr <- tkscrollbar(tablelist[["frame"]],orient="vertical", command=function(...)tkyview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],yscrollcommand=function(...) tkset(yscr,...))
      tablelist[["yscr"]] <- yscr
      tkgrid(tablelist[["yscr"]],row=0,column=1,sticky="ns")
    }

   tablelist[["get.listvariable"]] <- function ()
    {
      return(tclvalue(tablelist[["listvariable"]]))
    }
  tablelist[["get.tclname.listvariable"]] <- function ()
    {
      return(as.character(tablelist[["listvariable"]]))
    }
  tablelist[["get.row"]] <- function(i=1,adjust=TRUE)
    {
      if (adjust) i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      nrows = length(i)
      # create a matrix
      m <- matrix(nrow=nrows,ncol=ncols)
      for (j in 1:nrows)
        {
          m[j,] = as.character(.Tcl(paste("lindex $",id," ",i[j],sep="")))
        }
      colnames(m) <- columnlabels
      return(m)
    }
  tablelist[["get.raw.row"]] <- function (i=1, adjust=TRUE)
    {
      if (adjust) i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      nrows = length(i)
      # create a matrix
      m <- vector(mode="character",length=nrows)
      for (j in 1:nrows)
        {
          m[j] = tclvalue(.Tcl(paste("lindex $",id," ",i[j],sep="")))
        }
      return(m)
    }
  tablelist[["get.component"]] <- function(i=1,j=1,adjust=TRUE)
    {
      if (adjust) {i <- i -1 ; j <- j - 1}
      id <- tablelist[["get.tclname.listvariable"]]()
      return(.Tcl(paste("lindex [lindex $",id," ",i,"] ",j,sep="")))
    }
  tablelist[["remove.rows"]] <- function(index=1, adjust=TRUE)
    {
      if (adjust) index = index - 1
      tmp = paste(index,collapse=" ")
      tmp = paste("{",tmp,"}",sep="")
      .Tcl(paste(tclTableName,"delete",tmp))
      return()
    }
  tablelist[["add.raw.row"]] <- function(newRows,index="end")
    {
     nrows = length(newRows)
     if (nrows==0) return()
     tmp <- paste(newRows,collapse="} {")
     tmp <- paste("{",tmp,"}",sep="")
     .Tcl(paste(tclTableName,"insert",index,tmp))
    }
  tablelist[["get.table"]] <- function()
    {
      listname <- as.character(tablelist[["listvariable"]])
      listLength <- as.numeric(.Tcl("llength $" %+% listname))
      
      if (listLength > 0)
        {
                                        # determine the length of the sublist
          sublistLength = as.numeric(.Tcl(paste("llength [lindex $" %+% listname,"0 ]")))
          if (sublistLength > 0)
            {
              rnames <- vector(mode="character",length=listLength)
              M <- matrix(nrow=listLength,ncol=sublistLength)
              for (i in 1:listLength)
                {
                  rnames[i] <- tclvalue(.Tcl(paste(tclTableName,"rowcget",i-1, "-name")))
                  for (j in 1:sublistLength)
                    {
                      M[i,j] <- tclvalue(.Tcl(paste("lindex [lindex $",listname," ",i-1,"] ",
                                                    j-1,sep="")))
                    }
                }
            }
          dataFrame <- data.frame(I(M[,1]))
          if (sublistLength > 1)
          {
            for (i in 2:sublistLength)
            {
              dataFrame[[i]] <- M[,i]
            }
          }
          # M <- as.data.frame(M)
          #dimnames(M) <- list(rnames,tablelist[["columnlabels"]])
          dimnames(dataFrame) <- list(rnames,tablelist[["columnlabels"]])
          return(dataFrame)
        }
      else
        {
          return(data.frame())
        }
    }
  tablelist[["get.selection.rowNumber"]] <- function(adjust=TRUE)
    {
      selection <- as.numeric(tcl(tablelist[["tablelist"]],"curselection"))
      if (adjust) selection <- selection + 1
      return(selection)
    }
  tablelist[["row.select"]] <- function(id=1,adjust=TRUE)
    {
      if (adjust) id = id-1
      .Tcl(paste(tclTableName,"activate",id))
      return()
    }
  tablelist[["get.selection.rowName"]] <- function(adjust=TRUE)
    {
      selection <- tablelist[["get.selection.rowNumber"]]()
      nrows <- length(selection)
      if (nrows>0)
        {
          if (adjust) selection <- selection-1
          rowName <- vector(mode="numeric",length=nrows)
          for (i in 1:nrows)
            {
             rowName[i] <- as.numeric(tclvalue(.Tcl(paste(tclTableName,"rowcget",selection[i],"-name"))))
            }
          return(rowName)
        }
      else
        {
          return(selection)
        }
    }
  tablelist[["selection.clear"]] <- function(start=0,end="end")
    {
     .Tcl(paste(tclTableName,"selection clear {",start,end,"}"))
    }

                                        # return the list
  return(tablelist)
}
                                        # end odbcTablelist widget
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

create_tablelist <- function(
                             parent=topWindow,dataFrame=dataFrame,width=60,height=15,
                             alignCols=rep("center",ncols),alignColsLabel=alignCols,
                             withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=F,
                             multiSortColumn=T,selectmode="single",
                             editable=rep("no",ncols),editStartValue=list(),
                             colFormats=rep("no",ncols),rTypes,color="standard"
                             )
{

  # setup the rTypes and the tclTypes
  # the rTypes are diffent from the true R modes. Allowed type names are
  # exactly the same as the defined tclTypes, i.e.
  # "character","date","double","integer","logical"
  # when the dataFrame contains types of kind date, logical it is required
  # to specify the rTypes vector
  if (missing(rTypes))
    {
      rTypes <- getStorageMode(dataFrame)
    }
    

  # possible tclTypes: "character","date","double","integer","logical"
  tclTypes <- rTypes

  tablelist <- list(tablelist="") # create list containing the widget

                                        # create the frame containing the tablelist and the vert. horiz. scrollbar
  tablelist[["frame"]] <- tkframe(parent)

                                        # create the Tk.ID of the tablelist
  tablelist[["tablelist"]] <- .Tk.subwin(tablelist[["frame"]])
  .Tcl(paste("tablelist::tablelist",tablelist[["tablelist"]]$ID))
  tkgrid(tablelist[["tablelist"]],row=0,column=0)
    
                                        # create a variable with the tcl name of the tablelist widget
  tclTableName <- tablelist[["tablelist"]]$ID

                                        # get the dimensions of the dataFrame
  dimension <- dim(dataFrame)
  nrows <- dimension[1]
  ncols <- dimension[2]
  rm(dimension)

                                        # create the listvariable
  tablelist[["listvariable"]] <- tclVar()
  tkconfigure(tclTableName, listvariable=tablelist[["listvariable"]])

                                        # create the columns and the columnlabels
  columnlabels <- dimnames(dataFrame)[[2]]
  tablelist[["columnlabels"]] <- columnlabels
  columns <- paste(rep("0 ",ncols),"{",columnlabels,"} ",alignCols,sep="",collapse=" ")
  .Tcl(paste(tclTableName,"insertcolumns end",columns))

                                        # setup the editability
  already=list(checkbutton=F,Entry=T,SpinBox=T,ComboBox=T,no=F)
  execute=list(
    Entry="tablelist::addBWidgetEntry",
    SpinBox="tablelist::addBWidgetSpinBox",
    ComboBox="tablelist::addBWidgetComboBox",
    )
  # if editable is a list than modify it as vector
  if (is.list(editable))
  {
    editableTmp <- rep("no",ncols)
    names(editableTmp) <- columnlabels
    tmp <- names(editable)
    for (i in 1:length(tmp))
    {
      editableTmp[tmp[i]] = editable[[i]]
    }
    editable <- editableTmp
    rm(editableTmp)
  }
  for (i in 1:ncols)
    {
      if (already[[editable[i]]])
        {
          .Tcl(execute[[editable[i]]])
          already[[editable[i]]] <- F
        }
    }
  rm(already,execute)


                                         # create the variable containing the check/uncheckbox images

  .Tcl(paste("set dir C:/R/R_tablelist"))
  .Tcl(paste("set checkedImg [image create photo -file [file join $dir checked.gif]]"))
  .Tcl(paste("set uncheckedImg [image create photo -file [file join $dir unchecked.gif]]"))
  checkedImg = c("$::uncheckedImg","$::checkedImg")
  names(checkedImg) = c("0","1")
  updateImage <- function(row,col,text)
    {
	    .Tcl(paste(tclTableName," cellconfigure ",row,",",col," -image ",checkedImg[text],sep=""))
    }


                                        # align the columnlabels and assign the formatComd
  editCommandsRequired=F
  for (i in 1:ncols)
    {                                   # assign the columnnames
      .Tcl(paste(tclTableName," columnconfigure ", i-1,
                 " -name \"",columnlabels[i],
                 "\" -labelalign ",alignColsLabel[i],sep=""))
      if (editable[i] !="no")
        {
          .Tcl(paste(tclTableName,"columnconfigure", i-1, "-editable yes -editwindow",editable[i]))
          editCommandsRequired <- T
        }

      if (colFormats[i]!="no")
        {
          .Tcl(paste(tclTableName,"columnconfigure", i-1, "-formatcommand",tclColFormat[[colFormats[i]]]))
        }
    }

  if (editCommandsRequired)
    {
                                        # prepare the editstartcommand function
      editStartCmd <- function(...)
        {
          arg <- list(...)
          names(arg) <- c("tbl","row","col","text")
                                        # get the tclname of the tcl widget used for editing
          tclWidgetName <- as.character(.Tcl(paste(arg[["tbl"]],"editwinpath")))

          selectedColumn <- as.numeric(arg[["col"]]) + 1

                                        # configure the corresponding widget
          if (editable[selectedColumn] == "ComboBox") {
            v <- paste("{",editStartValue[[columnlabels[selectedColumn]]],"}",sep="",collapse=" ")
            .Tcl(paste(tclWidgetName,"configure -values {",v,"}"))
            .Tcl(paste(tclWidgetName,"configure -editable true"))
          }
                                        # save the txt variable in a tcl variable
          .Tcl(paste("return [",arg[["tbl"]]," cellcget ",arg[["row"]],",",
                     arg[["col"]]," -text]",sep=""))
        }

      tkconfigure(tablelist[["tablelist"]],editstartcommand=editStartCmd)
    }


  if (editCommandsRequired)
    {

                                        # define the EditEndCmd
      editEndCmd <- function(...)
        {
                                        # get the argument list
          arg <- list(...)
          names(arg) <- c("tbl","row","col","text")
                                        # set the column number and column type
          column <- as.numeric(arg[["col"]])+1
          rColType = rTypes[column]
          tclColType = tclTypes[column]

                                        # validate the input

          validResult <- validate[[tclColType]](arg[["text"]],colFormats[column])

                                        # if the input is valid write it to the original table
          if (validResult[[1]])
            {
                                        # update the image if colFormats is logical
              if (colFormats[column] == "logical") updateImage(arg[["row"]],arg[["col"]],arg[["text"]])
                                        # return the validated value to tcl
              tcltmp <- tclVar(validResult[[2]])
              .Tcl(paste("return $",as.character(tcltmp),sep=""))
            }
          else
            {
              .Tcl(paste(tclTableName,"rejectinput"))
              .Tcl(paste(tclTableName,"cancelediting"))
            }
        }
                                        # setup the EditEndCommand
      tkconfigure(tablelist[["tablelist"]],editendcommand=editEndCmd)
    }


  tablelist[["insert.data.frame"]] <- function(data=dataFrame,position="end",decoration=1)
    {
      # columnNames are not used in this function. The data are only inserted into
      # the tablelist.

      if (dim(data)[[2]] != ncols)
        {
          tkmessageBox(message = "Incorrect number of columns!",icon = "error",
                       type = "ok")
          return(invisible())
        }
      if (position != "end")
        {
          if (is.numeric(position))
            {
              position = max(0,as.integer(position - 1))
            } else {
              tkmessageBox(message = "Position must be \"end\" or a positive integer!",
                           icon = "error", type = "ok")
              return(invisible())
            }
        }

                                        # verify that the dataframe is not empty
      removeFirst = FALSE
      if (all(is.na(data)))
        {
          data[1,]= rep("",ncols)
          nrows = 1
          removeFirst=TRUE
        }
      else
        {
          nrows=dim(data)[[1]]
        }

                                        # assign the values to the listvariable
      tmp <- paste("{",data[,1], sep="")
      if (ncols > 1)
        {
          for (i in 2:ncols)
            {
              tmp <- paste(tmp,data[,i], sep="} {")
            }
        }
      tmp <- paste(tmp,"}",sep="")
      tmp <- paste(tmp,collapse="} {")


                                        #.Tcl(paste("set ", as.character(tablelist[["listvariable"]])," {{",tmp,"}}", sep=""))
      .Tcl(paste("set ", as.character(tablelist[["listvariable"]])," [list {",tmp,"} ]", sep=""))

                                        # configure the rownames
      rnames <- dimnames(data)[[1]]
      for (i in 1:nrows)
        {
          .Tcl(paste(tclTableName,"rowconfigure",i-1,"-name", rnames[i]))
        }
      if (removeFirst)
        {
          .Tcl(paste(tclTableName,"delete 0 0"))
          return()
        }
                                                    # check if necessary to change set the image in a logical field
      for (i in 1:ncols)
        {
          if (colFormats[i]=="logical")
            {
              colValues <- dataFrame[,i]
              for (j in 1:nrows)
                {
                  updateImage(j-1,i-1,colValues[j])
                }
            }
        }
    }

                                        # define the rowcount variable used to construct the labelnames. This variable
                                        # is very because new raws are named with the "r"&rowcount convention
  rowcount <- nrows
  tablelist[["get.rowcount"]] <- function() return(rowcount)
  tablelist[["set.rowcount"]] <- function(value) {rowcount <<- value}

                                        #.Tcl("option add *Font \"Helvetica -36\"")
                                        # set the decoration
  if (color=="standard")
    {
      tkconfigure(tablelist[["tablelist"]],background="linen",selectbackground="navy",
              selectforeground="white",stripebackground="#e0e8f0",showseparators="yes")
    }
  else
    {
      tkconfigure(tablelist[["tablelist"]],background=color,showseparators="yes")
    }

                                        # define the sortmode of the columns
  if (sortColumn)
    {
      tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::sortByColumn")
                                              # get the sortmode of the data before the convertion to character
      sortMode <- get.tclSortModes(tclTypes)
      sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),
        "-sortmode",sortMode)
      .Tcl(paste(sortMode,collapse=" ; "))
    }


                                        # define the addToSortColumns command
  if (multiSortColumn & !sortColumn)
    {
      tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::addToSortColumns")
      if (!sortColumn)
        {
          sortMode <- get.tclSortModes(tclTypes)
          sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),
            "-sortmode",sortMode)
          .Tcl(paste(sortMode,collapse=" ; "))
        }
    }
  if (multiSortColumn | sortColumn)
    {
      resetSortInfo <- function()
        {
          .Tcl(paste(tclTableName,"resetsortinfo"))
        }
      tkbind(tclvalue(.Tcl(paste(tclTableName, "bodytag"))),
             "<Double-Button-1>",resetSortInfo)
    }

                                        # define the optionWindow used with button 3
  optionWindow <- function() tablelistOptionWindow(tablelist,parent=parent)

  tkbind(tclvalue(.Tcl(paste(tclTableName, "bodytag"))),
         "<Button-3>",optionWindow)


                                        # insert the values in the tablelist
  tablelist[["insert.data.frame"]]()
                                        # other configurations options
  tkconfigure(tablelist[["tablelist"]], height=height, width=width, stretch="all",
              selectmode=selectmode)

  if (withScrollBarX)
    {
      xscr <- tkscrollbar(tablelist[["frame"]],orient="horizontal", command=function(...)tkxview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],xscrollcommand=function(...) tkset(xscr,...))
      tablelist[["xscr"]] <- xscr
      tkgrid(tablelist[["xscr"]],row=1,column=0,sticky="ew")
    }

  if (withScrollBarY)
    {
      yscr <- tkscrollbar(tablelist[["frame"]],orient="vertical", command=function(...)tkyview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],yscrollcommand=function(...) tkset(yscr,...))
      tablelist[["yscr"]] <- yscr
      tkgrid(tablelist[["yscr"]],row=0,column=1,sticky="ns")
    }

  tablelist[["get.listvariable"]] <- function ()
    {
      return(tclvalue(tablelist[["listvariable"]]))
    }
  tablelist[["get.tclname.listvariable"]] <- function ()
    {
      return(as.character(tablelist[["listvariable"]]))
    }
  tablelist[["get.row"]] <- function(i=1,adjust=TRUE)
    {
      if (adjust) i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      nrows = length(i)
      # create a matrix
      m <- matrix(nrow=nrows,ncol=ncols)
      for (j in 1:nrows)
        {
          m[j,] = as.character(.Tcl(paste("lindex $",id," ",i[j],sep="")))
        }
      colnames(m) <- columnlabels
      return(m)
    }
  tablelist[["get.raw.row"]] <- function (i=1, adjust=TRUE)
    {
      if (adjust) i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      nrows = length(i)
      # create a matrix
      m <- vector(mode="character",length=nrows)
      for (j in 1:nrows)
        {
          m[j] = tclvalue(.Tcl(paste("lindex $",id," ",i[j],sep="")))
        }
      return(m)
    }
  tablelist[["get.component"]] <- function(i=1,j=1,adjust=TRUE)
    {
      if (adjust) {i <- i -1 ; j <- j - 1}
      id <- tablelist[["get.tclname.listvariable"]]()
      return(.Tcl(paste("lindex [lindex $",id," ",i,"] ",j,sep="")))
    }
  tablelist[["remove.rows"]] <- function(index=1, adjust=TRUE)
    {
      if (adjust) index = index - 1
      tmp = paste(index,collapse=" ")
      tmp = paste("{",tmp,"}",sep="")
      .Tcl(paste(tclTableName,"delete",tmp))
      return()
    }
  tablelist[["add.raw.row"]] <- function(newRows,index="end")
    {
     nrows = length(newRows)
     if (nrows==0) return()
     tmp <- paste(newRows,collapse="} {")
     tmp <- paste("{",tmp,"}",sep="")
     .Tcl(paste(tclTableName,"insert",index,tmp))
    }
  tablelist[["get.table"]] <- function()
    {
      listname <- as.character(tablelist[["listvariable"]])
      listLength <- as.numeric(.Tcl("llength $" %+% listname))

      if (listLength > 0)
        {
                                        # determine the length of the sublist
          sublistLength = as.numeric(.Tcl(paste("llength [lindex $" %+% listname,"0 ]")))
          if (sublistLength > 0)
            {
              rnames <- vector(mode="character",length=listLength)
              M <- matrix(nrow=listLength,ncol=sublistLength)
              for (i in 1:listLength)
                {
                  rnames[i] <- tclvalue(.Tcl(paste(tclTableName,"rowcget",i-1, "-name")))
                  for (j in 1:sublistLength)
                    {
                      M[i,j] <- tclvalue(.Tcl(paste("lindex [lindex $",listname," ",i-1,"] ",
                                                    j-1,sep="")))
                    }
                }
            }
          dataFrame <- data.frame(I(M[,1]))
          if (sublistLength > 1)
          {
            for (i in 2:sublistLength)
            {
              dataFrame[[i]] <- M[,i]
            }
          }
          # M <- as.data.frame(M)
          #dimnames(M) <- list(rnames,tablelist[["columnlabels"]])
          dimnames(dataFrame) <- list(rnames,tablelist[["columnlabels"]])
          return(dataFrame)
        }
      else
        {
          return(data.frame())
        }
    }
  tablelist[["get.selection.rowNumber"]] <- function(adjust=TRUE)
    {
      selection <- as.numeric(tcl(tablelist[["tablelist"]],"curselection"))
      if (adjust) selection <- selection + 1
      return(selection)
    }
  tablelist[["row.select"]] <- function(id=1,adjust=TRUE)
    {
      if (adjust) id = id-1
      .Tcl(paste(tablelist[["tablelist"]][["ID"]],"activate",id))
      return()
    }
  tablelist[["get.selection.rowName"]] <- function(adjust=TRUE)
    {
      selection <- tablelist[["get.selection.rowNumber"]]()
      nrows <- length(selection)
      if (nrows>0)
        {
          if (adjust) selection <- selection-1
          rowName <- vector(mode="numeric",length=nrows)
          for (i in 1:nrows)
            {
             rowName[i] <- as.numeric(tclvalue(.Tcl(paste(tclTableName,"rowcget",selection[i],"-name"))))
            }
          return(rowName)
        }
      else
        {
          return(selection)
        }
    }
  tablelist[["selection.clear"]] <- function(start=0,end="end")
    {
     .Tcl(paste(tclTableName,"selection clear {",start,end,"}"))
    }
                                        # return the list
  return(tablelist)
}
                                        # end tablelist widget






create_selectionTablelist <- function(
                             parent=topWindow,dataFrame1,dataFrame2,title="",width=60,height=15,
                             alignCols=rep("center",ncols),alignColsLabel=alignCols,
                             withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=F,
                             multiSortColumn=T,selectmode="multiple",
                             editable=rep("no",ncols),editStartValue=list(),
                             colFormats=rep("no",ncols),rTypes,grabWindow)
  {
    tablelist <- list() # create list containing the widget
    tablelist[["frame"]] <- tkframe(parent)
                                          # get the dimensions of the dataFrame
    ncols <- dim(dataFrame1)[2]
    columnlabels <- dimnames(dataFrame1)[[2]]

    # split according to the available and desired fields
    tmp <- available.desired.dataFrames(dataFrame1,dataFrame2)
    dataFrame1 <- tmp[["dataFrame1"]]
    dataFrame2 <- tmp[["dataFrame2"]]
    rm(tmp)
    
    # construct the tablelist widgets
    tablelist1 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame1,
                        width=width,height=height,alignCols=alignCols,alignColsLabel=alignColsLabel,
                        withScrollBarX=withScrollBarX,withScrollBarY=withScrollBarY,
                        sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode,editable=editable,editStartValue=editStartValue,
                        colFormats=colFormats,rTypes) #,color="#e0e8f0"

    tablelist2 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame2,
                        width=width,height=height,alignCols=alignCols,alignColsLabel=alignColsLabel,
                        withScrollBarX=withScrollBarX,withScrollBarY=withScrollBarY,
                        sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode,editable=editable,editStartValue=editStartValue,
                        colFormats=colFormats,rTypes) #,color="linen"

    labelTitle <- tklabel(tablelist[["frame"]],text=title)

    onInsert <- function()
      {
        # get the list of selected elements
        selection <- tablelist1[["get.selection.rowNumber"]]()
        if (length(selection) == 0)
          {
            tkmessageBox(message="No items selected",icon="error",type="ok",parent=grabWindow)
            return()
          }
        selectedValues <- tablelist1[["get.raw.row"]](i=selection)
        # remove the selected rows and clear the selection
        tablelist1[["remove.rows"]](i=selection)
        tablelist1[["selection.clear"]]()

        # delete the selection and add the selection to the tablelist2
        tablelist2[["selection.clear"]]()
        tablelist2[["add.raw.row"]](selectedValues)
      }

    onRemove <- function()
      {
        # get the list of selected elements
        selection <- tablelist2[["get.selection.rowNumber"]]()
        if (length(selection) == 0)
          {
            tkmessageBox(message="No items selected",icon="error",type="ok",parent=grabWindow)
            return()
          }
        selectedValues <- tablelist2[["get.raw.row"]](i=selection)

        # remove the selected rows and clear the selection
        tablelist2[["remove.rows"]](i=selection)
        tablelist2[["selection.clear"]]()

        # delete the selection and add the selection to the tablelist1
        tablelist1[["selection.clear"]]()
        tablelist1[["add.raw.row"]](selectedValues)
      }

    tablelist[["get.available"]] <- function() return(tablelist1[["get.table"]]())
    tablelist[["get.selected"]] <- function() return(tablelist2[["get.table"]]())
    tablelist[["get.universe"]] <- function()
      {
        a <- tablelist1[["get.table"]]()
        b <- tablelist2[["get.table"]]()
        a <- rbind(a,b)
        dimnames(a)[[1]] <- 1:nrow(a)
        return(a)
      }
      
    tablelist[["insert.data.frame"]] <- function(newValues,setOrder=FALSE,widget=1)
      {
        if (widget == 1)
          {
            tablelist1[["insert.data.frame"]](data=newValues)
          }
        else
          {
            tablelist2[["insert.data.frame"]](data=newValues)
          }
      }
      
      
    tablelist[["reset"]] <- function(widget=1)
      {
        if (widget==1)
          {
            # set the tclListVariable
            tclListVariableName <- tablelist1[["get.tclname.listvariable"]]()
            # set the tclTablelistName
            tclTablelistName <- as.character(tablelist1[["tablelist"]])
          }
        if (widget==2)
          {
            # set the tclListVariable
            tclListVariableName <- tablelist2[["get.tclname.listvariable"]]()
            # set the tclTablelistName
            tclTablelistName <- as.character(tablelist2[["tablelist"]])
          }
        # set the tclListVariable equal to the empty list
        .Tcl(paste("set",tclListVariableName,"{}"))
        # set the selection equal to the empty list
        .Tcl(paste(tclTablelistName,"selection clear {0 end}"))
      }


    label1 <- tklabel(tablelist[["frame"]],text="Available")
    label2 <- tklabel(tablelist[["frame"]],text="Selected")
    
    b.insert <- create_button(parent=tablelist[["frame"]],text="Insert >>",command=onInsert)
    b.remove <- create_button(parent=tablelist[["frame"]],text="<< Remove",command=onRemove)

    if (title != "")
      {
        tkgrid(labelTitle,columnspan=2)
      }

    tkgrid(label1,label2)
    tkgrid(tablelist1[["frame"]],tablelist2[["frame"]],padx=10)
    tkgrid(b.insert[["button"]],b.remove[["button"]],pady=5)
    return(tablelist)
  }
  
  
  
  






create_slider <- function(parent=topWindow,position=NULL,orient="horizontal",
                          showvalue=T,from=0,to=100,tickInterval=10,initialValue=from,
                          label="")
{
  slider <- list()
  sliderValue <- tclVar(initialValue)
  slider[["slider"]] <- tkscale(parent,from=from,to=to,showvalue=showvalue,
                                variable=sliderValue,resolution=1,orient=orient,
                                tickinterval=tickInterval,label=label)
  return(slider)
}



create_frame <- function(parent=topWindow)
{
  widget <- list() # create list containing the widget, used "widget" instead of
                                        # "frame" because frame is a reserved name
  widget[["frame"]] <- tkframe(parent)
  return(widget)
}

create_labelFrame <- function(parent=topWindow,text="",labelanchor="nw")
{
  labelFrame <- list() # create list containing the widget
  labelFrame[["labelFrame"]] <- tkwidget(parent,"labelframe",text=text,
                                labelanchor=labelanchor)
  return(labelFrame)
}

saveDataFrame <- function(dataFrame)
{
  filename <- tclvalue(tkgetSaveFile())
  n <- nchar(filename)
  if (n == 0) return(invisible())
  if (!identical(substr(filename,max(n-3,1),n),".csv"))
    {
      filename <- paste(filename,".csv",sep="")
      write.csv(dataFrame,file=filename)
    }
  else
    {
      write.csv(dataFrame,file=filename)
    }
}


create_saveCancelButton <- function(parent=topWindow,onSave=function(){},onCancel=function(){})
{
  myFrame <- tkframe(parent)
  b.save <- create_button(parent=myFrame,text="Save",command=onSave)
  b.cancel <- create_button(parent=myFrame,text="Cancel",command=onCancel)

  tkgrid(b.save[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
  return(myFrame)
}


create_okCancelButton <- function(parent=topWindow,onOk=function(){},onCancel=function(){})
{
  myFrame <- tkframe(parent)
  b.ok <- create_button(parent=myFrame,command=onOk)
  b.cancel <- create_button(parent=myFrame,text="Cancel",command=onCancel)

  tkgrid(b.ok[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
  return(myFrame)
}


inputMaskOkCancel <- function(parent,labelText="",onOk=function(){},onCancel=function(){})
{
  Mask <- list()
  Mask[["topWindow"]] <- tktoplevel()
  Mask[["label"]] <- create_label(parent=Mask[["topWindow"]],value=labelText)
  Mask[["entry"]] <- create_entry(parent=Mask[["topWindow"]])
  Mask[["buttonFrame"]] <- create_frame(parent=Mask[["topWindow"]],onOk=onOk,onCancel=onCancel)

  tkgrid(Mask[["label"]][["label"]])
  tkgrid(Mask[["entry"]][["entry"]])
  tkgrid(Mask[["buttonFrame"]][["frame"]])
  tkgrab(parent)
}
