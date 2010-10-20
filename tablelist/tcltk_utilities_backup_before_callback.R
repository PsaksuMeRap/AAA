# importa le librerie necessarie
require(tcltk)
tclRequire("BWidget")
tclRequire("Iwidgets")
tclRequire("Tktable")
tclRequire("Tablelist")
require(tkrplot)
source("C:/Documents and Settings/claudio.AYRTON/Desktop/R_tablelist/utilities.R")

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
                      fraction,nbPanes,orient="vertical")
{
  #
  panedWindow <- list()
  panedWindow[["panedWindow"]] <- .Tk.subwin(parent)
 .Tcl(paste("iwidgets::panedwindow",panedWindow[["panedWindow"]]$ID,"-width",
      width,"-height",height))

  panelist <- list()
  
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
      # tkpack(tklabel(panedWindow[[i+1]],text="put some widgets here"),fill="both",expand="yes")
    }

  tmp <- paste(fraction,collapse=" ")
  .Tcl(paste(panedWindow[["panedWindow"]]$ID, "fraction", tmp))
  tkconfigure(panedWindow[["panedWindow"]],orient=orient)
  rm(tmp)
  return(panedWindow)
}


create_button <- function(parent=topWindow,text="Ok",width=max(nchar(text),10),command=onClick,position=NULL)
{
  button <- list() # create list containing the widget
  button[["button"]] <- tkbutton(parent,text=text,width=width,command=command)
  button[["set.label"]] <- function(text="Ok")
  {
    tkconfigure(button[["button"]],text=text)
  }
  button[["position"]] <- position
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


create_combo <- function(parent=topWindow,values=c(""),position=NULL)
{
	nb.elements <- length(values)
	# compute the longest name
  cb.width = max(nchar(values)) + 1
	combo <- list()  # create list containing the widget
 	combo[["combo"]] <- tkwidget(parent,"ComboBox",editable=FALSE,values=values)
  textVariable = tclVar()
  tkconfigure(combo[["combo"]], width=cb.width, textvariable=textVariable)

	combo[["get.selection"]] <- function()
	{
		return(values[as.numeric(tclvalue(tkcmd(combo[["combo"]],"getvalue")))+1])
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
	combo[["position"]] <- position
	return(combo)
}


create_listbox <- function(parent=topWindow,values=c(""),mode="single", 
                  withScrollBar=FALSE,showDefault=0,position=NULL)
{
	nb.elements <- length(values)
	listbox <- list() # create list containing the widget
	listbox[["listbox"]] <- tklistbox(parent, selectmode=mode)

	if (withScrollBar)
	{
		listbox[["yscr"]] <- tkscrollbar(parent, repeatinterval=5, 
                         command=function(...)tkyview(listbox[["listbox"]],...))
		tkconfigure(listbox[["listbox"]],yscrollcommand=function(...) tkset(yscr,...))
	}

	for (i in 1:nb.elements)
	{
		tkinsert(listbox[["listbox"]],"end",values[i])
	}
	# set the default values, if any
 	if (showDefault != 0) 
        {
		if (lenth(showDefault)==1)
		{
			tkselection.set(listbox[["listbox"]],showDefault)
		} else {
			for (i in showDefault)
			{
				tkselection.set(listbox[["listbox"]],i-1) # index starts at zero
			}
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
		for (i in (sort(index)-seq(0,length.out=length(index))))
		{
			tkdelete(listbox[["listbox"]],i-1)
		}
	}

	listbox[["delete.selected"]] <- function()
	{
		index <- get.id.selection()
		delete.elements(index)
	}

  listbox[["position"]] <- position
	return (listbox)
}


# start table widget
create_table <- function(parent=topWindow,tclArray,height=10,width=10,
                selectmode="extended",withScrollBarX=FALSE,withScrollBarY=FALSE,
                titlerows="1", titlecols="1",position=NULL)
{
	tclArrayName <- ls(tclArray$env)
	table <- list() # create list containing the widget
	table[["table"]] <- tkwidget(parent,"table",rows=paste(tclArray$nrow+1),
                      cols=paste(tclArray$ncol+1),titlerows=titlerows,
                      titlecols=titlecols,height=paste(height+1),
                      width=paste(width+1))
			   
	if (withScrollBarX)
	{
		tkconfigure(table[["table"]],xscrollcommand=function(...) tkset(xscr,...))
		table[["xscr"]] <- tkscrollbar(parent,orient="horizontal", 
                       command=function(...)tkxview(table[["table"]],...))
	}
	
	if (withScrollBarY)
	{
		tkconfigure(table[["table"]],yscrollcommand=function(...) tkset(yscr,...))
		table[["yscr"]] <- tkscrollbar(parent,command=function(...)tkyview(table[["table"]],...))
	}
	
	table[["position"]] <- position
	tkconfigure(table[["table"]],rowseparator="\"\n\"",colseparator="\"\t\"")
	tkconfigure(table[["table"]],variable=tclArrayName,background="white",
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
    print(radio1[["get.selection"]]())
      tkconfigure(tablelist[["tablelist"]],selectmode=radio1[["get.selection"]]())
      tkdestroy(top)
      tkraise(parent)
      tkfocus(parent)
    }
  onCancel <- function() tkdestroy(top)

  ok <- create_button(parent=top,command=onOk)
  cancel <- create_button(parent=top,text="Cancel",command=onCancel)

  tkgrid(ok[[1]],cancel[[1]],padx=padx,pady=pady)
  tkgrab(top)
  tkfocus(top)
}
# da terminare


create_tablelist <- function(
                    parent=topWindow,data=data,width=60,height=15,
                    alignCols=rep("center",ncols),alignColsLabel=rep("left",ncols),
                    withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=F,
                    multiSortColumn=T,selectmode="single",position=NULL,
                    editable=rep("no",ncols),editStartCmdName="editStartCmd",
                    editStartValue=list()
                    )
{
  tablelist <- list(tablelist="") # create list containing the widget
  
  # create the frame containing the tablelist and the vert. horiz. scrollbar
  tablelist[["frame"]] <- tkframe(parent)

  # create the Tk.ID of the tablelist
  tablelist[["tablelist"]] <- .Tk.subwin(tablelist[["frame"]])
  .Tcl(paste("tablelist::tablelist",tablelist[["tablelist"]]$ID))
  tablelist[["position"]] <- position
  tkgrid(tablelist[["tablelist"]],row=1,column=1)
  
  # get the dimensions of the dataframe
  dimension <- dim(data)
  nrows <- dimension[1]
  ncols <- dimension[2]

  # get the sortmode of the data before the convertion to character
  sortMode <- rTclStorageMode(getStorageMode(data))
  
  # convert all elements of data to characters
  for (i in 1:ncols)
  {
    if (!is.character(data[,i]))
      {
        data[,i] <- as.character(data[,i])
      }
  }
  
  # create the listvariable
  tablelist[["listvariable"]] <- tclVar()
  tkconfigure(tablelist[["tablelist"]]$ID, listvariable=tablelist[["listvariable"]])
  
  # create the columns and the columnlabels
  columnlabels <- dimnames(data)[[2]]
  tablelist[["columnlabels"]] <- columnlabels
  columns <- paste(rep("0 ",ncols),"{",columnlabels,"} ",alignCols,sep="",collapse=" ")
  .Tcl(paste(tablelist[["tablelist"]]$ID,"insertcolumns end",columns))
  
  # setup the editability
  already=list(checkbutton=F,Entry=T,SpinBox=T,ComboBox=T,no=F)
  execute=list(
               Entry="tablelist::addBWidgetEntry",
               SpinBox="tablelist::addBWidgetSpinBox",
               ComboBox="tablelist::addBWidgetComboBox"
               )
  for (i in 1:ncols) {
    if (already[[editable[i]]]) {
       .Tcl(execute[[editable[i]]])
       already[[editable[i]]] <- F
      }
    }
  rm(already,execute)
  
  
  # align the columnlabels
  # prepare the editstartcommand function
  editCmdString <- paste(
               "proc",editStartCmdName,"{tbl row col text} {\n",
               "set w [$tbl editwinpath] \n",
               "switch [$tbl columncget $col -name] { \n"
              )
  editStartCmdRequired <- F
  for (i in 1:ncols)
    {
      # assign the columnnames
      .Tcl(paste(tablelist[["tablelist"]]$ID,"columnconfigure", i-1, "-name",
                 columnlabels[i], "-labelalign", alignColsLabel[i]))
      if (editable[i] !="no") {
        .Tcl(paste(tablelist[["tablelist"]]$ID,"columnconfigure", i-1, "-editable yes -editwindow",editable[i]))
        if (editable[i] == "ComboBox") {
          editStartCmdRequired <- T

          v <- paste("{",editStartValue[[columnlabels[i]]],"}",sep="",collapse=" ")


          # construct the command for the corresponding edit widget
          editCmdString <- paste(editCmdString, columnlabels[i], "{\n",
                       "$w configure -values {",v,"}\n",
                       "} \n",
                       "} \n"
                      )
         }
       }
     }
  if (editStartCmdRequired) {
        editCmdString <- paste(editCmdString,"return $text\n }")
        .Tcl(editCmdString)
        #.Tcl(paste(tablelist[["tablelist"]]$ID,"configure -editstartcommand", editStartCmdName))
        tkconfigure(tablelist[["tablelist"]],editstartcommand=editStartCmdName)
   }
  rm(editCmdString)


  tablelist[["insert.data.frame"]] <- function(dataframe=data,
                                      nrows=dim(dataframe)[[1]],position="end",
                                      decoration=1)
  {
    if (dim(dataframe)[[2]] != ncols)
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


     # assign the values to the listvariable
     tmp <- paste("{",dataframe[,1], sep="")
     if (ncols > 1)
       {
         for (i in 2:ncols)
          {
           tmp <- paste(tmp,dataframe[,i], sep="} {")
          }
       }
     tmp <- paste(tmp,"}",sep="")
     tmp <- paste(tmp,collapse="} {")

    #.Tcl(paste("set ", as.character(tablelist[["listvariable"]])," {{",tmp,"}}", sep=""))
    .Tcl(paste("set ", as.character(tablelist[["listvariable"]])," [list {",tmp,"} ]", sep=""))

    # configure the rownames
    #rnames <- paste("r",1:nrows,sep="")
    for (i in 1:nrows)
    {
      .Tcl(paste(tablelist[["tablelist"]]$ID,"rowconfigure",i-1,"-name", i))
    }
  }

  # define the rowcount variable used to construct the labelnames. This variable
  # is very because new raws are named with the "r"&rowcount convention
  rowcount <- nrows
  tablelist[["get.rowcount"]] <- function() return(rowcount)
  tablelist[["set.rowcount"]] <- function(value) {rowcount <<- value}

  #.Tcl("option add *Font \"Helvetica -36\"")
  # set the decoration
  tkconfigure(tablelist[["tablelist"]],background="linen",selectbackground="navy",
              selectforeground="white",stripebackground="#e0e8f0",showseparators="yes")

  # define the sortmode of the columns
  if (sortColumn)
  {
    tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::sortByColumn")
    sortMode = paste(tablelist[["tablelist"]]$ID,"columnconfigure",0:(ncols-1),
               "-sortmode",sortMode)
    .Tcl(paste(sortMode,collapse=" ; "))
  }

  
  # define the addToSortColumns command
  if (multiSortColumn & !sortColumn)
  {
    tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::addToSortColumns")
    if (!sortColumn)
    {
     #for (i in 0:(ncols-1))
     # {
     #   sortMode[i+1] = paste(tablelist[["tablelist"]]$ID,"columnconfigure",i,
     #   "-sortmode",sortMode[i+1])
     # }
     
     sortMode = paste(tablelist[["tablelist"]]$ID,"columnconfigure",0:(ncols-1),
                         "-sortmode",sortMode)
     .Tcl(paste(sortMode,collapse=" ; "))
    }
    resetSortInfo <- function()
      {
        .Tcl(paste(tablelist[["tablelist"]]$ID,"resetsortinfo"))
      }
    tkbind(tclvalue(.Tcl(paste(tablelist[["tablelist"]]$ID, "bodytag"))),
          "<Double-Button-1>",resetSortInfo)
  }

  # define the optionWindow used with button 3
  optionWindow <- function() tablelistOptionWindow(tablelist,parent=parent)

  tkbind(tclvalue(.Tcl(paste(tablelist[["tablelist"]]$ID, "bodytag"))),
          "<Button-3>",optionWindow)
  
  
  # insert the values by row in the tablelist
  tablelist[["insert.data.frame"]](data)
  
  # other configurations options
  tkconfigure(tablelist[["tablelist"]], height=height, width=width, stretch="all",
             selectmode=selectmode)
  
  if (withScrollBarX)
    {
      xscr <- tkscrollbar(tablelist[["frame"]],orient="horizontal", command=function(...)tkxview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],xscrollcommand=function(...) tkset(xscr,...))
      tablelist[["xscr"]] <- xscr
      tkgrid(tablelist[["xscr"]],row=2,column=1,sticky="ew")
    }
  
  if (withScrollBarY)
    {
      yscr <- tkscrollbar(tablelist[["frame"]],orient="vertical", command=function(...)tkyview(tablelist[["tablelist"]],...))
      tkconfigure(tablelist[["tablelist"]],yscrollcommand=function(...) tkset(yscr,...))
      tablelist[["yscr"]] <- yscr
      tkgrid(tablelist[["yscr"]],row=1,column=2,sticky="ns")
    }
  
  tablelist[["get.listvariable"]] <- function ()
    {
      return(tclvalue(tablelist[["listvariable"]]))
    }
  tablelist[["get.tclname.listvariable"]] <- function ()
    {
      return(as.character(tablelist[["listvariable"]]))
    }
  tablelist[["get.row"]] <- function(i=0)
    {
      i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      return(.Tcl(paste("lindex $",id," ",i,sep="")))
    }
  tablelist[["get.component"]] <- function(i=0,j=0)
    {
      i <- i -1 ; j <- j - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      return(.Tcl(paste("lindex [lindex $",id," ",i,"] ",j,sep="")))
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
                  rnames[i] <- tclvalue(.Tcl(paste(tablelist[["tablelist"]]$ID,"rowcget",i-1, "-name")))
                  for (j in 1:sublistLength)
                    {
                      M[i,j] <- tclvalue(.Tcl(paste("lindex [lindex $",listname," ",i-1,"] ",
                                                    j-1,sep="")))
                    }
                }
            }
          M <- as.data.frame(M)
          dimnames(M) <- list(rnames,tablelist[["columnlabels"]])
          return(M)
        } else {
          return(data.frame())
        }
    }

                                        # return the list
  return(tablelist)
}
                                        # end tablelist widget


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



create_frame <- function(parent=topWindow,position=NULL)
{
  widget <- list() # create list containing the widget, used "widget" instead of
                   # "frame" because frame is a reserved name
  widget[["frame"]] <- tkframe(parent)
  widget[["position"]] <- position
  return(widget)
}

create_labelFrame <- function(parent=topWindow,text="",labelanchor="nw",position=NULL)
{
  labelFrame <- list() # create list containing the widget
  labelFrame[["labelFrame"]] <- tkwidget(parent,"labelframe",text=text,
                                labelanchor=labelanchor)
  labelFrame[["position"]] <- position
  
  return(labelFrame)
}

saveDataFrame <- function(DataFrame)
{
  filename <- tclvalue(tkgetSaveFile())
  n <- nchar(filename)
  if (n == 0) return(invisible())
  if (!identical(substr(filename,max(n-3,1),n),".csv"))
  {
    filename <- paste(filename,".csv",sep="")
    write.csv(DataFrame,file=filename)
  } else {
    write.csv(DataFrame,file=filename)
  }
}


