require(tcltk)
tclRequire("BWidget")
tclRequire("Tktable")
tclRequire("Tablelist")

Nomi=c("Ettore","Fabio","Patrick","Ilaria","Mamma","Papa","Luca","Claudio")
Eta=c(11,110,55,40,73,72,40,37)
Sesso=c("u","u","u","d","d","u","u","u")
Dati=data.frame(Nomi=Nomi,Eta=Eta,Sesso=Sesso)




# ------------------------------------------------------
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
#Questa procedura la puoi togliere
print.tclArrayVar <- function(tclArray,height=-1,width=-1)
{
  require(tcltk)
  tt <- tktoplevel()
  tclRequire("Tktable")
  tclArrayName <- ls(tclArray$env)
  tkwm.title(tt,tclArrayName)
  table1 <- tkwidget(tt,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),titlerows="1",titlecols="1",
                     height=paste(height+1),width=paste(width+1),
                     xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
  xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")
  tkconfigure(table1,variable=tclArrayName,background="white",selectmode="extended")
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
# ------------------------------------------------------

create_label <- function(parent=topWindow, values=c(""))
{
	nb.labels <- length(values)
        label <- list()	# list containing the buttons
	# create the labels
	for (i in 1:nb.labels)
	{
		label[[i]] <- tklabel(parent, text=values[i])
	}

	label[["nb.labels"]] = nb.labels

	return(label)

}

create_button <- function(parent=topWindow,text="Ok",command=OnClick)
{
        variable <- tclVar(values[1])
        button <- tkbutton(parent,text=text,command=command)
	return(button)
}



create_radio <- function(parent=topWindow,values=c(""))
{
	nb.radios <- length(values)
        variable <- tclVar(values[1])
        radio <- list()	# list containing the buttons
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

	return(radio)
}


create_combo <- function(parent=topWindow, values=c(""))
{
	nb.elements <- length(values)

 	comboBox <- tkwidget(parent,"ComboBox",editable=FALSE,values=values)
        textVariable = tclVar()
        tkconfigure(comboBox, textvariable=textVariable)

	comboBox[["get.selection"]] <- function()
	{
		return(values[as.numeric(tclvalue(tkcmd(comboBox,"getvalue")))+1])
	}

	comboBox[["get.tclId"]] <- function()
        {
	        return(tclvalue(tkcmd(comboBox,"getvalue")))	
        }

        comboBox[["set.selection"]] <- function(new.value)
	{
		tclvalue(textVariable) <<- new.value
	}

        comboBox[["modify.values"]] <- function(new.values)
	{
		tkconfigure(comboBox, values=new.values)
                values <<- new.values
	}

	return(comboBox)
}


create_listbox <- function(parent=topWindow, values=c(""), mode="single", withScrollBar=FALSE, showDefault=0)
{
	nb.elements <- length(values)

	listbox <- tklistbox(parent, selectmode=mode)
	
	if (withScrollBar)
	{
		listbox[["yscr"]] <- tkscrollbar(parent, repeatinterval=5, 
                       command=function(...)tkyview(listbox,...))
		tkconfigure(listbox,yscrollcommand=function(...) tkset(yscr,...))
	}

	for (i in 1:nb.elements)
	{
		tkinsert(listbox,"end",values[i])
	}
	# set the default values, if any
 	if (showDefault != 0) 
        {
		if (lenth(showDefault)==1)
		{
			tkselection.set(listbox,showDefault)
		} else {
			for (i in showDefault)
			{
				tkselection.set(listbox,i-1) # index starts at zero
			}
		}	
	}

	listbox[["get.id.selection"]] <- function()
	{
		return (as.integer(tkcurselection(listbox))+1)
	}

	listbox[["get.selection"]] <- function()
	{
		return (values[as.integer(tkcurselection(listbox))+1])
	}

	listbox[["get.nb.selected"]] <- function()
	{
		return (length(as.integer(tkcurselection(listbox))))
		
	}

	listbox[["delete.elements"]] <- function(index)
	{
		for (i in (sort(index)-seq(0,length.out=length(index))))
		{
			tkdelete(listbox,i-1)
		}
	}

	listbox[["delete.selected"]] <- function()
	{
		index <- get.id.selection()
		delete.elements(index)
	}

	return (listbox)
}

create_table <- function(parent=topWindow,tclArray,height=10,width=10,selectmode="extended",withScrollBarX=FALSE,withScrollBarY=FALSE,
                         titlerows="1", titlecols="1")
{
	tclArrayName <- ls(tclArray$env)

	table <- tkwidget(parent,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),titlerows=titlerows,titlecols=titlecols,
                           height=paste(height+1),width=paste(width+1))
			   
	if (withScrollBarX)
	{
		tkconfigure(table,xscrollcommand=function(...) tkset(xscr,...))
		table[["xscr"]] <- tkscrollbar(parent,orient="horizontal", command=function(...)tkxview(table,...))
	}
	
	if (withScrollBarY)
	{
		tkconfigure(table,yscrollcommand=function(...) tkset(yscr,...))
		table[["yscr"]] <- tkscrollbar(parent,command=function(...)tkyview(table,...))
	}
	
	tkconfigure(table,rowseparator="\"\n\"",colseparator="\"\t\"")
	tkconfigure(table,variable=tclArrayName,background="white",selectmode=selectmode)
	return(table)
}

create_check_button <- function(parent=topWindow,default="0")
{
	value <- tclVar(default)
	cb <- tkcheckbutton(parent,variable=value)

	cb[["get.value"]] <- function()
	{
		return(as.character(tclvalue(value)))
	}

	cb[["set.value"]] <- function(x)
	{
		tclvalue(value) <- x
	}
	
	return(cb)
}

#remm <- function()
#  {

    # Global variables
    # Building blocks of the structure "project" (to be removed after generation of "project")

    topWindow <- tktoplevel()

    valori = c("Claudio", "Reto", "Arturo")

    combo1 = create_combo(topWindow, valori)
    tkgrid(combo1[["object"]]) # inserisci il ComboBox

    scrivi <- function() {print(combo1[["get.selection"]]())}
    modifica <- tkbutton(topWindow, text="scrivi", command=scrivi)
    tkgrid(modifica)

    b1 = create_radio(topWindow, values=c(1:3))
    l1 = create_label(topWindow, values=c("Claudio","Luca","lindo"))
    for (i in 1:3)
    {
	tkgrid(l1[[i]], b1[[i]])
    }

    mylist1 <- create_listbox(parent=topWindow, values=rnorm(20),withScrollBar=TRUE, mode="multiple")
    tkgrid(mylist1[["listbox"]],mylist1[["scr"]])
    tkgrid.configure(mylist1[["scr"]],sticky="nsw",columnspan=2)
    
    mylist2 <- create_listbox(parent=topWindow, values=rnorm(20),withScrollBar=TRUE, mode="multiple")
    tkgrid(mylist2[["listbox"]],mylist2[["scr"]])
    tkgrid.configure(mylist2[["scr"]],sticky="nsw")

    
    #mytable <- create_table (parent=topWindow,tclArray=tclArrayVar(Dati),withScrollBarX=FALSE,withScrollBarY=TRUE,
    #                     ,height=5,titlerows="1", titlecols="1")
    #tkgrid(mytable[["table"]],mytable[["yscr"]])
    #tkgrid.configure(mytable[["yscr"]],sticky="nsw")
    cb <- create_check_button(parent=topWindow)
    tkgrid(cb[["cb"]])
    tkfocus(topWindow)
#}


