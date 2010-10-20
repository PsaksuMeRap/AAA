.onLoad <- function(libname="tkutilities", pkgname="tkutilities")
  {
    #tclRequire("BWidget")
    #tclRequire("Iwidgets")
    #tclRequire("Tktable")
     tclRequire("Tablelist")

    .Tcl("proc formatRound0 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr round({$arg})]}")
    .Tcl("proc formatRound1 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 10.0; return [format \"%.1f\" [expr {round([expr {$arg * $x}]) / $x}]]}")
    .Tcl("proc formatRound2 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 100.0; return [format \"%.2f\" [expr {round([expr {$arg * $x}]) / $x}]]}")
    .Tcl("proc formatRound3 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 1000.0; return [format \"%.3f\" [expr {round([expr {$arg * $x}]) / $x}]]}")
    .Tcl("proc formatRound4 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 10000.0; return [format \"%.4f\" [expr {round([expr {$arg * $x}]) / $x}]]}")
    .Tcl("proc formatRound5 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 100000.0; return [format \"%.5f\" [expr {round([expr {$arg * $x}]) / $x}]]}")
    .Tcl("proc formatRound6 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};set x 1000000.0; return [format \"%.6f\" [expr {round([expr {$arg * $x}]) / $x}]]}")

    .Tcl("proc formatPercent {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {$arg * 100}]\\%}")
    .Tcl("proc formatPercent0 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {[formatRound2 $arg] * 100}]\\%}")
    .Tcl("proc formatPercent1 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {[formatRound3 $arg] * 100}]\\%}")
    .Tcl("proc formatPercent2 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {[formatRound4 $arg] * 100}]\\%}")
    .Tcl("proc formatPercent3 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {[formatRound5 $arg] * 100}]\\%}")
    .Tcl("proc formatPercent4 {arg} {if {[string equal $arg \"\"]} {return \"\"};if {[string equal $arg \"NA\"]} {return \"NA\"};return [expr {[formatRound6 $arg] * 100}]\\%}")

    .Tcl("proc emptyString {val} { return }")

    ## the function usde to compare number with NA
    .Tcl("proc RnumberCompare {item1 item2} {if {[string compare $item1 $item2] == 0} {return 0};if {[string compare $item1 'NA'] == 0} {return -1} elseif {[string compare $item2 'NA'] == 0} {return 1} else {if {$item1 == $item2} {return 0};if {$item1 < $item2} {return -1}; return 1}}")
  }

  
                                        # tcl format functions and corresponding
                                        # R list with the tcl function name
tclColFormat <- list()

tclColFormat[["round0"]] <- "formatRound0"
tclColFormat[["round1"]] <- "formatRound1"
tclColFormat[["round2"]] <- "formatRound2"
tclColFormat[["round3"]] <- "formatRound3"
tclColFormat[["round4"]] <- "formatRound4"
tclColFormat[["round5"]] <- "formatRound5"
tclColFormat[["round6"]] <- "formatRound6"

tclColFormat[["percent"]] <- "formatPercent"
tclColFormat[["percent0"]] <- "formatPercent0"
tclColFormat[["percent1"]] <- "formatPercent1"
tclColFormat[["percent2"]] <- "formatPercent2"
tclColFormat[["percent3"]] <- "formatPercent3"
tclColFormat[["percent4"]] <- "formatPercent4"

tclColFormat[["logical"]] <- "emptyString"


convertRtoTcl <- function(storageMode,vec)
  {
    return(
          switch(storageMode, 
            character = vec,
            double = as.character(vec),
            integer = as.character(vec),
            logical = as.character(as.integer(vec)),
            tkmessage(message("Invalid mode in tablelist::insert.data.frame",
            icon="error",type="ok"))
            )
          ) 
  }

convertTcltoR <- function(storageMode,vec)
  {
    return(
          switch(storageMode, 
            character = vec,
            double = as.double(vec),
            integer = as.integer(vec),
            logical = as.logical(as.integer(vec)),
            tkmessage(message("Invalid mode in tablelist::convertTcltoR",
            icon="error",type="ok"))
            )
          ) 
  }

zeroType <- function(storageMode)
  {
    l <- length(storageMode)
    if (l==1)
      {
        return(
          switch(storageMode, 
            character = character(0),
            double = double(0),
            integer = integer(0),
            logical = logical(0),
            tkmessage(message("Invalid mode in zeroType",
            icon="error",type="ok"))
            )
          )
      }
    
    x <- list("character"=character(0),"double"=double(0),"integer"=integer(0),
              "logical"=logical(0))
              
    return(x[storageMode])  
  }

#convertRtoTclObj <- function(x)
#  {
#    ## x is an R vector
#    if (is.tclObj(x)) return(x)
#    storageMode <- storage.mode(x)
#    y <- switch(storageMode, character = .External("RTcl_ObjFromCharVector", 
#        x, drop, PACKAGE = "tcltk"), double = .External("RTcl_ObjFromDoubleVector", 
#        x, drop, PACKAGE = "tcltk"), integer = .External("RTcl_ObjFromIntVector", 
#        x, drop, PACKAGE = "tcltk"), logical = .External("RTcl_ObjFromIntVector", 
#        as.integer(x), drop, PACKAGE = "tcltk"), stop(gettextf("cannot handle object of mode '%s'", 
#        storage.mode(x)), domain = NA))
#    class(y) <- "tclObj"
#    z <- tclvalue(y)
#    y <- switch(storageMode, character = z,
#                          double = gsub("1\.#QNAN","NA",z),
#                         integer = gsub("-2147483648","NA",z),
#                         logical = gsub("-2147483648","NA",z)
#               )
#    y
#  }


#The following two widgets have been disabled in order to avoid the iwidget
#create_datefield <- function(parent,label,labelPos="w",command)
#  {
#    widget <- list()
#    widget[["datefield"]] <- .Tk.subwin(parent)
#    .Tcl(paste("iwidgets::datefield",widget[["datefield"]]$ID))
#    tkconfigure(widget[["datefield"]],iq="high",int="1")
#
#    if (!missing(label))
#      {
#        tkconfigure(widget[["datefield"]],labeltext=label,labelpos=labelPos)
#      }
#      
#    if (!missing(command))
#      {
#        tkconfigure(widget[["datefield"]],command=command)
#      }
#      
#    widget[["get.date"]] <- function()
#      {
#        return(as.character(.Tcl(paste(widget[["datefield"]]$ID,"get -string"))))
#      }
#      
#    widget[["set.date"]] <- function(newDate)
#      {
#        .Tcl(paste(widget[["datefield"]]$ID,"show",newDate))
#      }
#      
#    widget[["set.now"]] <- function()
#      {
#        .Tcl(paste(widget[["datefield"]]$ID,"show now"))
#      }
#    return(widget)
#  }

#create_dateentry <- function(parent,label,labelPos="w",command)
#  {
#    widget <- list()
#    widget[["dateentry"]] <- .Tk.subwin(parent)
#    .Tcl(paste("iwidgets::dateentry",widget[["dateentry"]]$ID))
#    tkconfigure(widget[["dateentry"]],iq="high",int="1")
#
#    if (!missing(label))
#      {
#        tkconfigure(widget[["dateentry"]],labeltext=label,labelpos=labelPos)
#      }
#
#    if (!missing(command))
#      {
#        tkconfigure(widget[["dateentry"]],command=command)
#      }
#
#    widget[["get.date"]] <- function()
#      {
#        return(as.character(.Tcl(paste(widget[["dateentry"]]$ID,"get -string"))))
#      }
#
#    widget[["set.date"]] <- function(newDate)
#      {
#        .Tcl(paste(widget[["dateentry"]]$ID,"show",newDate))
#      }
#
#    widget[["set.now"]] <- function()
#      {
#        .Tcl(paste(widget[["dateentry"]]$ID,"show now"))
#      }
#    return(widget)
#  }

create_selectionbox  <- function(parent,iq="high")
  {
    widget <- list()
    # create the Tk.ID of the selectionbox
    widget[["selectionbox"]] <- .Tk.subwin(parent)
    #.Tcl(paste("tablelist::tablelist",tablelist[["tablelist"]]$ID))

  }
  

create_label <- function(parent=topWindow,value="",font="Helvetica 10")
{
#font="{MS Sans Serif} 8
  label <- list()	# create list containing the widget
  # create the label
  label[["label"]] <- ttklabel(parent,text=value,font=font)
  label[["set.label"]] <- function(value,font="Helvetica 10")
    {
      tkconfigure(label[["label"]],text=value,font=font)
    }
  return(label)
}


create_textwindow <- function(parent,bg="white",font="courier",state="disabled",
                              height=40,width=80,withScrollBarX=FALSE,withScrollBarY=FALSE)
  {
    widget <- list()
    widget[["frame"]] <- ttkframe(parent)
    widget[["textwindow"]] <- tktext(widget[["frame"]],bg="white",font=font,
                                     height=height,width=width,state=state)

    tkgrid(widget[["textwindow"]],row=0,column=0)

    if (withScrollBarY)
      {
        yscr <- tkscrollbar(widget[["frame"]], repeatinterval=5,
                                       command=function(...)tkyview(widget[["textwindow"]],...))
        tkconfigure(widget[["textwindow"]],yscrollcommand=function(...) tkset(yscr,...))
        tkgrid(yscr,row=0,column=1,sticky="ns")
      }

    if (withScrollBarX)
      {
        xscr <- tkscrollbar(widget[["frame"]], repeatinterval=5,orient="horizontal",
                                       command=function(...)tkxview(widget[["textwindow"]],...))
        tkconfigure(widget[["textwindow"]],xscrollcommand=function(...) tkset(xscr,...))
        tkgrid(xscr,row=1,column=0,sticky="ew")
      }
      
    widget[["insert"]] <- function(text,pos="end",tag="")
      {
        tkconfigure(widget[["textwindow"]],state="normal")
        text <- gsub("\\[", '\\\\[', text)
        text <- gsub("\\]", '\\\\]', text)
      
        .Tcl(paste(widget[["textwindow"]][["ID"]]," insert ",pos," \"",text,"\" ",tag,sep=""))
        # tkinsert(widget[["textwindow"]],pos,text,tag)
        tkconfigure(widget[["textwindow"]],state=state)
      }
      
    widget[["delete"]] <- function(index1="1.0",index2="end")
      {
       tkconfigure(widget[["textwindow"]],state="normal")
       .Tcl(paste(widget[["textwindow"]][["ID"]],"delete",index1,index2))
       tkconfigure(widget[["textwindow"]],state=state)
      }
      
    widget[["get.value"]] <- function(from="1.0", to="end")
      {
        return(tclvalue(.Tcl(paste(widget[["textwindow"]][["ID"]],"get",from,to))))
      }
      
    widget[["setFont"]] <- function(font,size)
    {
        .Tcl(paste(widget[["textwindow"]][["ID"]]," configure -font {{",font,"} ",size,"}",sep=""))
    }
    widget[["setTag"]] <- function(tagName,tagStructure)
    {
      ## .t tag configure bold_italics -font {-family courier -size 12 -weight bold -slant italic}
      ## .t tag configure big -font {-family helvetica -size 24 -weight bold}
      ## .t tag configure color1 -foreground red
      ## .t tag configure sunken -relief sunken -borderwidth 1
      ## .t tag bind Ouch <1> {.t insert end "Ouch! "}
      ## Now insert text that has the property of the tags
      ## .t insert end "Here are a few text styles.\n"
      ## .t insert end "1. Bold italic text.\n" bold_italics

        .Tcl(paste(widget[["textwindow"]][["ID"]]," tag configure ",tagName," ",tagStructure,sep=""))
    }
    return(widget)
  }
  
  
create_tabnotebook <- function(parent,text,width=600,height=400)
  {
    # text is the vector of tabnames
    ## create the widget
    ###tbn <- list()
    tbn <- new.env()
    tbn[["tabnotebook"]] <- ttknotebook(parent,width=width,height=height)

    tbn[["pages"]] <- list()

    tbn[["nbTabs"]] <- 0
    if(missing(text))
    { 
      toInsert <- 0 
    } else { 
      toInsert <- length(text)
    }

    tbn[["select.tab"]] <- function(i=1,adjust=TRUE)
      {
        if (adjust) i <- i - 1
        tkselect(tbn[["tabnotebook"]], i)
        return()
      }

    tbn[["delete.tab"]] <- function(from=1,to=from,adjust=TRUE)
      {
        if (adjust) from <- from - 1
        if (missing(to)) to <- from
        if (adjust & !missing(to)) to <- to - 1
        ID <- tbn[["tabnotebook"]]$ID
        .Tcl(paste(ID,"forget",rep(from,length(from:to)),collapse=";"))
        tbn[["nbTabs"]] <<- tbn[["nbTabs"]] - (to - from + 1)
      }

    tbn[["add.tab"]] <- function(text)
      {
        if (missing(text))
          {
            text <- paste("Tab",tbn[["nbTabs"]]+1,sep=" ")
          }

        tbn[["nbTabs"]] <<- tbn[["nbTabs"]] + 1
        i <- tbn[["nbTabs"]]
	tbn[["pages"]][[i]] <<- ttkframe(tbn[["tabnotebook"]])
        .Tcl(paste(tbn[["tabnotebook"]]$ID," add ",tbn[["pages"]][[i]]$ID," -text \"",text,"\"",sep=""))
      }

    if(toInsert>0) 
      {
       for (i in 1:toInsert)
        {
          ## create the i-th tab (page)
          tbn[["add.tab"]](text[i])
        }
      }
    rm(toInsert)
    if (tbn[["nbTabs"]]>0) tbn[["select.tab"]]()

    tkpack(tbn[["tabnotebook"]])
    return(tbn)
  }
 


create_topmenu <- function(parent)
  {
    topMenu <- list()
    topMenu[["menu"]] <- tkmenu(parent)
    tkconfigure(parent,menu=topMenu[["menu"]])
    topMenu[["addCascadeMenu"]] <- function(menu,label)
      {
        tkadd(topMenu[["menu"]],"cascade",label=label,menu=menu)
      }
    return(topMenu)
  }
  

 
create_menu <- function(parentMenu,tearoff=FALSE)
  {
    nbMenus <- 0
    Menu <- list()
    Menu[["menu"]] <- tkmenu(parentMenu,tearoff=FALSE)
    submenu <- list()
    Menu[["addCascadeMenu"]] <- function(menu,label)
      {
        nbMenus <<- nbMenus + 1
        submenu[[nbMenus]] <<- tkadd(Menu[["menu"]],"cascade",label=label,menu=menu)
      }

    Menu[["addCommand"]] <- function(label,command)
      {
        nbMenus <<- nbMenus + 1
        submenu[[nbMenus]] <<- tkadd(Menu[["menu"]],"command",label=label,command=command)
      }

    Menu[["addRadio"]] <- function(label,variable,value)
      {
        nbMenus <<- nbMenus + 1
        submenu[[nbMenus]] <<- tkadd(Menu[["menu"]],"radio",label=label,variable=variable,value=value)
      }

    Menu[["addcheck"]] <- function(label,variable,command)
      {
        nbMenus <<- nbMenus + 1
        submenu[[nbMenus]] <<- tkadd(Menu[["menu"]],"check",label=label,variable=variable,command=command)
      }

    Menu[["addSeparator"]] <- function()
      {
        nbMenus <<- nbMenus + 1
        submenu[[nbMenus]] <<- tkadd(Menu[["menu"]],"separator")
      }

    Menu[["get.submenu"]] <- function(i=0)
      {
        if (i==0) return(submenu)
        if ((i > 0) & (i<=nbMenus)) return(submenu[[i]])
      }
    return(Menu)
  }



create_checkmenu <- function(parentMenu,labels,checkValue=FALSE)
  {
    menu <- list()
    menu[["variable"]] <- tclVar(checkValue)
    nbCheck <- length(labels)
    if (nbCheck > 0)
      {
        for (i in 1:nbCheck)
          {
            menu[[i+1]] <- tkadd(parentMenu,"check",label=labels[i],variable=menu[["variable"]])
          }
      }

    menu[["set.value"]] <- function(value)
      {
        tclvalue(menu[["variable"]]) <- value
      }

    menu[["get.value"]] <- function()
      {
        return(tclvalue(menu[["variable"]]))
      }

    return(menu)
  }


create_radiomenu <- function(parentMenu,labels,radioValue="")
  {
    menu <- list()
    menu[["variable"]] <- tclVar(radioValue)
    nbRadio <- length(labels)
    if (nbRadio>0)
      {
        for (i in 1:nbRadio)
          {
            menu[[i+1]] <- ttkradiobutton(parentMenu,text=labels[i],variable=menu[["variable"]])
          }
      }
    menu[["set.value"]] <- function(value)
      {
        tclvalue(menu[["variable"]]) <- value
      }

    menu[["get.value"]] <- function()
      {
        return(tclvalue(menu[["variable"]]))
      }
    return(menu)
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
  outValidation()
  return(entry)
}


#create_feedback <- function(parent=topWindow,label="Status:                   ",
#                            steps=10)
#{
#  feedback <- list()
#  labelVariable <- tclVar(label)
#  
#                                        # create the feedback widget
#  feedback[["feedback"]] <- .Tk.subwin(parent)
#  .Tcl(paste("iwidgets::feedback",feedback[["feedback"]]$ID))
#  
#  tkconfigure(feedback[["feedback"]],labelvariable=labelVariable,steps=steps)
#  status = 0
#  
#  feedback[["step"]] <- function (step=1)
#    {
#      if (status + step > steps)
#        {
#          step = steps - status
#          status <<- steps
#        } else {
#          status <<- status + step
#        }
#      .Tcl(paste(feedback[["feedback"]],"step",step))
#      return(status)
#    }
#
#  feedback[["status"]] <- function (value=status)
#    {
#      if (value > steps)
#        {
#          step = steps - status
#          status <<- steps
#        } else {
#          step = value - status
#          status <<- value
#        }
#      
#      .Tcl(paste(feedback[["feedback"]],"step",step))
#      return(status)
#    }
#
#  feedback[["get.status"]] <- function() return(status)
#  
#  feedback[["reset"]] <- function ()
#    {
#      tcl(feedback[["feedback"]],"reset")
#      return()
#    }
#  
#  feedback[["set.label"]] <- function(label=label, sameWidth=T)
#    {
#      if (sameWidth)
#        {
#          oldWidth <- nchar(tclvalue(labelVariable))
#          tmp <- paste(rep(" ",oldWidth),collapse="")
#          label <- substr(paste(label,tmp,sep=""),1,oldWidth)
#        }
#
#      tclvalue(labelVariable) <- label
#    }
#  
#  return(feedback)
#}



create_tkrplot <- function(parent=topWindow,func,hscale=1,
                          vscale=1,background="transparent",exportButton=F)
{
  p <- list()
  if (missing(func)) func <- function(){par(bg=background);plot(x=1)}
  p[["tkrplot"]] <- tkrplot(parent,fun=func,hscale=hscale,vscale=vscale)
  p[["hscale"]] <- hscale
  p[["vscale"]] <- vscale
  p[["tkrreplot"]] <- function(func,hscale=p[["hscale"]],vscale=p[["vscale"]])
    {
      tkrreplot(p[["tkrplot"]],fun=func,hsc=p[["hscale"]],vsc=p[["vscale"]])
      func <<- func
    }
  if (exportButton)
    {
      p[["export"]] <- function()
        {
          # identify the parent window of the widget
          windowParent <- tclvalue(tkwinfo("toplevel",p[["tkrplot"]]))
          
          filename <- tclvalue(tkgetSaveFile(parent=windowParent,defaultextension=".jpg",
                      filetypes=paste("{{Jpeg files} {.jpg .jpeg}} {{PDF files} {.pdf}}",
                      "{{Png files} {.png}} {{Windows bitmap files} {.bmp}}")))
          n <- nchar(filename)
          if (n == 0) return(invisible())
          tmp <- strsplit(filename,"\\.")
          nbSplits <- length(tmp[[1]])
          extension <- tmp[[1]][nbSplits]
          
          if (extension == "pdf")
            {
              pdf(file=filename,bg=background)
              func()
              dev.off()
              return()
            }
          if (extension == "jpg" | extension =="jpeg")
            {
              jpeg(filename=filename,quality=75,bg=background)
              func()
              dev.off()
              return()
            }
          if (extension == "png")
            {
              png(filename=filename,bg=background)
              func()
              dev.off()
              return()
            }
          if (extension == "bmp")
            {
              bmp(filename=filename,bg=background)
              func()
              dev.off()
              return()
            }                   
        } # end function p[["export"]]
    } # end if exportButton
    
  return(p)
}


create_panedwindow <- function(parent=topWindow,nbPanes=1,width="300",height="200",orient="vertical")
{
  # don't use grid, pack or place to control widget in the panes. In that case
  # create a frame in put the frame in the pane and the widgets in the frame

  panedWindow <- list()
  panedWindow[["panedWindow"]] <- ttkpanedwindow(parent, width=width,height=height,orient=orient)
 
                                        # calculate the number of panes dipending on the lenght of the vector
                                        # fraction. If fraction is missing, then only one pane is assumed.
  if (nbPanes >0)
    {  
      for (i in 1:nbPanes)
        {
          pname = paste("p",i,sep="")
          panedWindow[[pname]] <- ttkframe(panedWindow[["panedWindow"]])
          .Tcl(paste(panedWindow[["panedWindow"]]$ID,"add",panedWindow[[pname]]$ID))
        }

      rm(pname)
    }
  return(panedWindow)
}


create_button <- function(parent=topWindow,text="Ok",width=max(nchar(text),10),command=onClick)
{
  button <- list() # create list containing the widget
  button[["button"]] <- ttkbutton(parent,text=text,width=width,command=command)
  button[["set.label"]] <- function(text="Ok")
    {
      tkconfigure(button[["button"]],text=text)
    }
  return(button)
}


create_radio <- function(parent=topWindow,values=c(""),text,
                                position="v",command)
{
  nb.radios <- length(values)
  variable <- tclVar(values[1])
  radio <- list()	# list containing the widgets
  radio[["frame"]] <- ttkframe(parent)
  
                                        # create the buttons
  j = 2
  if (!missing(text))
  {
    for (i in 1:nb.radios)
    {
      radio[[j]] <- ttkradiobutton(radio[["frame"]],variable=variable,value=values[i],text=text[i])
      if (!missing(command))
        {
          tkconfigure(radio[[j]],command=command)
        }
      j = j + 1
    }
  }  else {
    for (i in 1:nb.radios)
    {
      radio[[j]] <- ttkradiobutton(radio[["frame"]],variable=variable,value=values[i])
      if (!missing(command))
        {
          tkconfigure(radio[[j]],command=command)
        }
      j = j + 1
    }
  }

  if (position=="v")
  {
    for (j in 2:(nb.radios+1))
    {
      tkgrid(radio[[j]],sticky="w")
    } 
  } else {
    for (j in 2:(nb.radios+1))
    {
        tkgrid(radio[[j]],row=0,column=j-2,padx=c(0,20))
    }
  }

  radio[["nb.radios"]] = nb.radios
  
  radio[["get.selection"]] <- function()
    {
      return(tclvalue(variable))
    }
  
  radio[["set.selection"]] <- function(value)
    {
      tclvalue(variable) <<- value
    }
  
  return(radio)
}


create_combo <- function(parent=topWindow,values=vector("character"),startValue=NA,state="normal",
                         width=3,foreignKey=NA,postcommand)
  {
    ## vector("character") returns character(0)
    ## add the startValue to the list of values
    ## other state are "readonly" and "disabled"
    #if (!is.na(startValue))
    #  {
    #    values = union(startValue,values)
    #  }
    # compute the longest name

    if (missing(width))
      {
        cb.width = max(width,nchar(values)) + 1
        updateWidth = TRUE
      }
    else
      {
        cb.width = width
        updateWidth = FALSE
      }
    # create the textVariable
    textVariable = tclVar()

    combo <- list()  # create list containing the widget
    combo[["combo"]] <- ttkcombobox(parent,width=cb.width,state=state,
                               textvariable=textVariable)
    
    if (!missing(postcommand))
      {
        tkconfigure(combo[["combo"]],postcommand=postcommand)
      }
      
    combo[["get.selection"]] <- function()
      {
        #return(values[as.numeric(tclvalue(tkcmd(combo[["combo"]],"getvalue")))+1])
        return(tclvalue(textVariable))
      }
      
    combo[["get.values"]] <- function()
      {
        values = as.character(.Tcl(paste(combo[["combo"]]$ID, " cget -values")))
        ## if the values of the combobox are equal to an empty list then the
        ## result is character(0)
        return(values) 
      }
      
    combo[["get.id"]] <- function(tclId=FALSE)
      {
        if (!tclId) return(as.numeric(tclvalue(tcl(combo[["combo"]],"current")))+1)
        return(as.numeric(tclvalue(tcl(combo[["combo"]],"current"))))
      }
    combo[["get.foreignKey"]] <- function()
      {
        tclId = combo[["get.id"]]()
        if (tclId == 0) return(NULL)
        return(foreignKey[tclId]) 
      }
    combo[["set.selection"]] <- function(value)
      {
        tclvalue(textVariable) <<- value
      }

    combo[["reset"]] <- function()
      {
        tclvalue(textVariable) <<- ""      
      }

    combo[["modify.values"]] <- function(values=NULL)
      {
        nbValues <- length(values)
        if (nbValues > 1)
          { 
            if (updateWidth) cb.width <- max(nchar(values)) + 1
            tkconfigure(combo[["combo"]], values=values,width=cb.width)
	    if (!is.element(combo[["get.selection"]](),values)) combo[["reset"]]() 
            return()
          }      
        if (nbValues == 1)
          {
            if (updateWidth) cb.width <- nchar(values) + 1
            tkconfigure(combo[["combo"]], values=paste("{",values,"}",sep=""),
                        width=cb.width)
	    if (!is.element(combo[["get.selection"]](),values)) combo[["reset"]]() 
            return()
          }
        if (updateWidth) cb.width <- 3
        tkconfigure(combo[["combo"]], values="{}",width=cb.width)
	combo[["reset"]]()
      }

    combo[["modify.width"]] <- function(value)
      {
        tkconfigure(combo[["combo"]], width=value)
      }

    # Add the values to the widget
    if (length(values) > 0) combo[["modify.values"]](values)
    rm(values)

    # Select the start value    
    if (!is.na(startValue))
      {
        combo[["set.selection"]](startValue)  
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

  frameSelectionMode <- ttkframe(top)
  label1 <- create_label(parent=frameSelectionMode,
                         value="Selection mode:")
  radio1 <- create_radio(parent=frameSelectionMode,
                                values=c("single","browse","multiple","extended"),
                                text=c("single","browse","multiple","extended"))
  # get the current selectmode
  currentSelectMode = .Tcl(paste(listbox[["listbox"]]$ID,"cget -selectmode"))

  radio1[["set.selection"]](value=currentSelectMode)

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
                           height=6,width=8,withScrollBarX=FALSE,order=FALSE,
                           withScrollBarY=FALSE,showDefault=0,background="linen")
{
  nb.elements <- length(values)
  
  # create the tcl variable containing the values
  listvariable <- tclVar()
  tclListVariableName <- as.character(listvariable)
  
  listbox <- list() # create list containing the widget
  listbox[["frame"]] <- ttkframe(parent)
  listbox[["listbox"]] <- tklistbox(listbox[["frame"]],listvariable=listvariable,
                          height=height,width=width,selectmode=mode,background=background)

  if (nb.elements > 0)
    {
      if (order) values <- values[order(values)]
      for (i in 1:nb.elements)
        {
          tkinsert(listbox[["listbox"]],"end",values[i])
        }
    }

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
          tkinsert(listbox[["listbox"]],"0",paste("{",values,"}",sep=""))
        }
      else
        {
          tclvalue(listvariable) <<- values
        }
    }

                                        # set the default values, if any
  if (showDefault != 0) 
    {
      if (length(showDefault)==1)
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
          #tmp = paste("{",addValues,"}",sep="")
          tmp = addValues
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



create_selectionListBox <- function(parent=topWindow,title="",labels=list(label1="",label2=""),
                                    values1=NULL,values2=NULL,mode="single",
                                    height=8,width=12,withScrollBarX=FALSE,
                                    withScrollBarY=FALSE,align="h",order=c(TRUE,TRUE),grabWindow)
  {
    #nb.elements <- length(values1)
    #if (nb.elements > 0)
    #  {
    #    namesToIndex <- 1:nb.elements
    #    names(namesToIndex) <- universe
    #  }
  
    listbox <- list() # create list containing the widget
    listbox[["frame"]] <- ttkframe(parent)
  
    if (!missing(order))
    {
	if (length(order)==1) order=c(order,order)
    }
    # check that values2 is not empty
    nbValues2 <- length(values2)
    if (nbValues2==0)
      {
        listbox1 <-  create_listbox (parent=listbox[["frame"]],values=values1,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             order=order[1],withScrollBarY=withScrollBarY,background="#e0e8f0")

        listbox2 <-  create_listbox (parent=listbox[["frame"]],values=values2,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             order=order[2],withScrollBarY=withScrollBarY)
        rm(nbValues2)
      }
    else
      {
        # determine the subset of non selected elements
        values2 <- intersect(values1,values2)
        values1 <- setdiff(values1,values2)

        listbox1 <-  create_listbox (parent=listbox[["frame"]],values=values1,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             order=order[1],withScrollBarY=withScrollBarY,background="#e0e8f0")

        listbox2 <-  create_listbox (parent=listbox[["frame"]],values=values2,mode=mode,
                             height=height,width=width,withScrollBarX=withScrollBarX,
                             order=order[2],withScrollBarY=withScrollBarY)
      }

    if (title != "") labelTitle <- ttklabel(listbox[["frame"]],text=title)
    if (labels[["label1"]] != "") label1 <- ttklabel(listbox[["frame"]],text=labels[["label1"]])
    if (labels[["label2"]] != "") label2 <- ttklabel(listbox[["frame"]],text=labels[["label2"]])

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
        listbox2[["add.values"]](addValues=selection,setOrder=order[2])
        
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
        listbox1[["add.values"]](addValues=selection,setOrder=order[1])

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


    listbox[["get.curselection"]] <- function(widget=1)
      {
        if (widget==1) {
          values <- listbox1[["values"]]()
          return (values[as.integer(tkcurselection(listbox1[["listbox"]]))+1])
        } else {
          values <- listbox2[["values"]]()
	  return (values[as.integer(tkcurselection(listbox2[["listbox"]]))+1])
        }
      }

    listbox[["set.values"]] <- function(newValues=values,setOrder=order,widget=1)
      {
        if (widget == 1)
          {
            listbox1[["set.values"]](newValues=newValues,setOrder=setOrder[1])
          }
        else
          {
            listbox2[["set.values"]](newValues=newValues,setOrder=setOrder[2])
          }
      }

        
    b.insert <- create_button(parent=listbox[["frame"]],text="Insert available >>",command=onInsert)
    b.remove <- create_button(parent=listbox[["frame"]],text="<< Remove selected",command=onRemove)

    if (align == "h")
    {
      if (title != "") tkgrid(labelTitle,columnspan=2)
      if ((labels[["label1"]] != "") | (labels[["label1"]] != "")) tkgrid(label1,label2)

      tkgrid(listbox1[["frame"]],listbox2[["frame"]],padx=10)
      tkgrid(b.insert[["button"]],b.remove[["button"]],pady=5)
    } else {
          if (title != "") tkgrid(labelTitle)
          if (labels[["label1"]] != "") tkgrid(label1)
          tkgrid(listbox1[["frame"]])
          tkgrid(b.insert[["button"]],pady=5)
          if (labels[["label2"]] != "") 
          {
	     tkgrid(label2,pady=10)
             tkgrid(listbox2[["frame"]])
          } else {
             tkgrid(listbox2[["frame"]],pady=c(10,0))
          }
          tkgrid(b.remove[["button"]],pady=5)
    }

     return(listbox)
  }
  
  
  
                                        # start table widget
#create_table <- function(parent=topWindow,tclArray,height=10,width=10,
#                         selectmode="extended",withScrollBarX=FALSE,withScrollBarY=FALSE,
#                         titlerows="1", titlecols="1",background="linen")
#{
#  tclArrayName <- ls(tclArray$env)
#  table <- list() # create list containing the widget
#  table[["frame"]] <- ttkframe(parent)
#  table[["table"]] <- tkwidget(table[["frame"]],"table",rows=paste(tclArray$nrow+1),
#                               cols=paste(tclArray$ncol+1),titlerows=titlerows,
#                               titlecols=titlecols,height=paste(height+1),
#                               width=paste(width+1))
  
  
#    if (withScrollBarY)
#    {
#      yscr <- tkscrollbar(table[["frame"]], repeatinterval=5,
#                                       command=function(...)tkyview(table[["table"]],...))
#      tkconfigure(table[["table"]],yscrollcommand=function(...) tkset(yscr,...))
#      tkgrid(yscr,row=0,column=1,sticky="ns")
#    }

#  if (withScrollBarX)
#    {
#      xscr <- tkscrollbar(table[["frame"]], repeatinterval=5,orient="horizontal",
#                                       command=function(...)tkxview(table[["table"]],...))
#      tkconfigure(table[["table"]],xscrollcommand=function(...) tkset(xscr,...))
#      tkgrid(xscr,row=1,column=0,sticky="ew")
#    }


#  tkconfigure(table[["table"]],rowseparator="\"\n\"",colseparator="\"\t\"")
#  tkconfigure(table[["table"]],variable=tclArrayName,background=background,
#              selectmode=selectmode)
#  return(table)
#}


create_checkButton <- function(parent=topWindow,default="0",label,command)
{
  value <- tclVar(default)
  checkButton <- list() # create list containing the widget
  if (!missing(label))
    {
      checkButton[["frame"]] <- ttkframe(parent)
      checkButton[["checkButton"]] <- ttkcheckbutton(checkButton[["frame"]],variable=value)
      checkButton[["label"]] <- ttklabel(checkButton[["frame"]],text=label)
      tkgrid(checkButton[["checkButton"]],sticky="w")
      tkgrid(checkButton[["label"]],row=0,column=1)  
    }
  else
    {  
      checkButton[["checkButton"]] <- ttkcheckbutton(parent,variable=value)
    }
    
  if (!missing(command)) tkconfigure(checkButton[["checkButton"]],command=command) 
  checkButton[["get.value"]] <- function()
    {
      return(as.character(tclvalue(value)))
    }

  checkButton[["set.value"]] <- function(x)
    {
      tclvalue(value) <- x
    }

  return(checkButton)
}


                                        # start tablelist widget
                                        # da terminare
tablelistOptionWindow <- function(tablelist,parent)
{
  padx=10
  pady=5
  top <- tktoplevel()

  frameSelectionMode <- ttkframe(top)
  label1 <- create_label(parent=frameSelectionMode,
                         value="Selection mode:")
  radio1 <- create_radio(parent=frameSelectionMode,
                                values=c("single","browse","multiple","extended"),
                                text=c("single","browse","multiple","extended"))
  
  tkgrid(label1[[1]])
  tkgrid(radio1[["frame"]])
  tkgrid(frameSelectionMode,columnspan=2,padx=padx,pady=pady)

  # get the current selectmode
  currentSelectMode = as.character(.Tcl(paste(tablelist[["tablelist"]]$ID,"configure -selectmode")))
  radio1[["set.selection"]](value=currentSelectMode[5])
  
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
  


updateColumnInfo <- function(columnInfo,columnInfoDefault,dataFrame,storageModes.v,colClasses.v)
{
  # setup the editability defaults for the available widgets
  # text, spinbox (the latter for Tk version 8.4 or higher), checkbutton, ttk::entry, ttk::combobox, or ttk::checkbutton
  defaultEditability=c(text="no",spinbox="yes",checkbutton="yes",entry="yes",combobox="yes")

  columnlabels <- dimnames(dataFrame)[[2]]
  requiredFields = names(columnInfoDefault)

  for (i in columnlabels)
   {
     if (is.null(columnInfo[[i]]))
      {
        columnInfo[[i]] <- columnInfoDefault
        columnInfo[[i]][["storageMode"]] <- storageModes.v[[i]]
        columnInfo[[i]][["colClass"]] <- colClasses.v[[i]]
        if (colClasses.v[[i]]=="logical") columnInfo[[i]][["colFormat"]] <- "logical"
        columnInfo[[i]][["editable"]] <- defaultEditability[[columnInfo[[i]][["widget"]]]]
      } else {
        availableFields = names(columnInfo[[i]])
        x = columnInfoDefault
        for(col in availableFields)
	 {
            x[[col]] <- columnInfo[[i]][[col]]
         }
        columnInfo[[i]] <- x
        if (is.na(columnInfo[[i]][["editable"]])) columnInfo[[i]][["editable"]] <- defaultEditability[[columnInfo[[i]][["widget"]]]]
        if (!is.element("storageMode",availableFields)) columnInfo[[i]][["storageMode"]] <- storageModes.v[[i]]
        if (!is.element("colClass",availableFields)) columnInfo[[i]][["colClass"]] <- colClasses.v[[i]]
        if (!is.element("colClass",availableFields)) if (colClasses.v[[i]]=="logical") columnInfo[[i]][["colFormat"]] <- "logical"
      }
   }
   return(columnInfo)
}


create_odbcTablelist <- function() {}
create_tablelist <- function(
                             parent=topWindow,dataFrame=dataFrame,width=60,height=15,
                             withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=FALSE,
                             multiSortColumn=TRUE,selectmode="single",color="standard",
                             columnInfo=list()
                             )
{
## selectmode:
## color:
## columnInfo: a list of lists, i.e. columnInfo[[i]] is a list whose name must be
##             equal to the column name of the dataFrame. Typically columnInfo[[i]] contains 
##             the following fields:
##             alignCol: "left", "right", "center" - default is "center"
##             alignColLabel: "left", "right", "center" - default is "center" 
##             widget: "text", "entry", "spinbox", "combobox", "checkbutton" - default "text"
##             editable: "yes", "no" - default "no"
##             editStartValues: used if widget=="combox". It must be a vector of strings.
##             storageMode: "character", "logical", "integer", "double"
##             colClass: "character", "logical", "integer", "double", "date"
##             colFormat: "no", "round0", "round1", ..., "round6", 
##                        "percent", "percent0", ..., "percent4", "logical" - default "no"

## storageMode is the result of the R command "storage.mode" applied to the columns of dataFrame
## colClass is the "storage.mode" of the column of dataFrame if its class <> date and date otherwise.
 
  editability.df=data.frame(text=c(yes="normal",no="disabled",disabled="disabled"),
                            entry=c(yes="normal",no="readonly",disabled="disabled"),
                            spinbox=c(yes="normal",no="readonly",disabled="disabled"),
                            combobox=c(yes="normal",no="readonly",disabled="disabled"),
                            checkbutton=c(yes="normal",no="disabled",disabled="disabled")
                           )

  # This variable is different from the create_odbcTablelist
  requireRowName <- FALSE

  columnlabels.v <- dimnames(dataFrame)[[2]]
  storageModes.v <- getStorageMode(dataFrame) ; names(storageModes.v) <- columnlabels.v
  colClasses.v <- getClass(dataFrame) ; names(colClasses.v) <- columnlabels.v
  ## this part of code is necessary in order to correct the differences between the values
  ## returned by getClass and getStorageMode with respect to numeric columns.
  tmp = colClasses.v != "date"
  colClasses.v[tmp] <- storageModes.v[tmp]
  rm(tmp)

  columnInfoDefault=list(alignCol="center",
                      alignColLabel="center",
                      widget="text",
                      editable=editability.df["no","text"],
                      editStartValues=vector("character"),
                      storageMode="character",
                      colClass = "text",
                      colFormat="no")

  columnInfo <- updateColumnInfo(columnInfo,columnInfoDefault,dataFrame,storageModes.v,colClasses.v)
  columnInfo <- columnInfo[columnlabels.v]


  execute = c(
    text="text",
    entry="ttk::entry",
    spinbox="spinbox",
    combobox="ttk::combobox",
    checkbutton = "checkbutton"
    )
                                        # get the dimensions of the dataFrame
  dimension <- dim(dataFrame)
  nrows <- dimension[1]
  ncols <- dimension[2]
  rm(dimension)

  # possible tclTypes: "character","date","double","integer","logical"
  tclTypes <- storageModes.v

  tablelist <- list(tablelist="") # create list containing the widget

                                        # create the frame containing the tablelist and the vert. horiz. scrollbar
  tablelist[["frame"]] <- ttkframe(parent)

                                        # create the Tk.ID of the tablelist
  tablelist[["tablelist"]] <- .Tk.subwin(tablelist[["frame"]])
  .Tcl(paste("tablelist::tablelist",tablelist[["tablelist"]]$ID))
  tkgrid(tablelist[["tablelist"]],row=0,column=0,sticky="n")

                                        # create a variable with the tcl name of the tablelist widget
  tclTableName <- tablelist[["tablelist"]]$ID

                                        # create the listvariable
  tablelist[["listvariable"]] <- tclVar()
  tkconfigure(tclTableName, listvariable=tablelist[["listvariable"]])

                                        # create the columns and the columnlabels
  tablelist[["columnlabels"]] <- columnlabels.v

                                       # construct the matrix of columnInfo
  tmp0 <- c("alignCol","alignColLabel","widget","editable","storageMode","colClass","colFormat")
  tmp1 <- vector("character")
  for (i in columnlabels.v)
   {
     tmp1 <- c(tmp1,unlist(columnInfo[[i]][tmp0]))
   }

  m.columnInfo <- matrix(tmp1,ncol=dim(dataFrame)[2],dimnames=list(tmp0,columnlabels.v))
  rm(tmp0,tmp1)


  columns <- paste(rep("0 ",ncols),"{",columnlabels.v,"} ",m.columnInfo["alignCol",],sep="",collapse=" ")
  .Tcl(paste(tclTableName,"insertcolumns end",columns))

                                         # create the variable containing the check/uncheckbox images

  tmp <- installed.packages()["tkutilities","LibPath"]

  .Tcl(paste("set dir ",tmp,"/tkutilities/graphics",sep=""))
  .Tcl(paste("set checkedImg [image create photo -file [file join $dir checked.gif]]"))
  .Tcl(paste("set uncheckedImg [image create photo -file [file join $dir unchecked.gif]]"))

  rm(tmp)

  checkedImg = c("$::uncheckedImg","$::checkedImg")
  names(checkedImg) = c("0","1")
  updateImage <- function(row,col,text)
    { 
       .Tcl(paste(tclTableName," cellconfigure ",row,",",col," -image ",checkedImg[text],sep=""))
    }

                                        # align the columnlabels and assign the formatComd
                                        # assign the columnnames
  editCommandsRequired=FALSE
  i <- ncols - 1

  .Tcl(paste(tclTableName," columnconfigure ", 0:i,
                 " -name \"",columnlabels.v,
                 "\" -labelalign ",m.columnInfo["alignColLabel",],sep="",collapse=";"))

  ok <- m.columnInfo["editable",] == "yes"

  if (any(ok))
    {
      .Tcl(paste(tclTableName,"columnconfigure",(0:i)[ok],"-editable 1 -editwindow",execute[m.columnInfo["widget",ok]],collapse=";"))
      editCommandsRequired <- T
    }

  ok <- m.columnInfo["colFormat",] != "no"
  if (any(ok))
    {
      .Tcl(paste(tclTableName,"columnconfigure",(0:i)[ok],"-formatcommand",tclColFormat[m.columnInfo["colFormat",ok]],collapse=";"))
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
          if (m.columnInfo["widget",selectedColumn] == "combobox") 
           {
             v <- paste("{",columnInfo[[columnlabels.v[selectedColumn]]][["editStartValues"]],"}",sep="",collapse=" ")
             .Tcl(paste(tclWidgetName,"configure -values {",v,"}"))
             .Tcl(paste(tclWidgetName,"configure -state normal"))
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
          selectedColumn <- as.numeric(arg[["col"]])+1
          rColType = columnInfo[[columnlabels.v[selectedColumn]]][["storageMode"]]
          tclColType = rColType

                                        # validate the input

          validResult <- validate[[tclColType]](arg[["text"]],m.columnInfo["colClass",columnlabels.v[selectedColumn]])

                                        # if the input is valid write it to the original table
          if (validResult[[1]])
            {
                                        # update the image if colClass is logical

              if (m.columnInfo["colClass",columnlabels.v[selectedColumn]] == "logical") updateImage(arg[["row"]],arg[["col"]],arg[["text"]])
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

  tablelist[["insert.data.frame"]] <- function(data=dataFrame,decoration=1)
    {
      # columnNames are not used in this function. The data are only inserted into
      # the tablelist.

      if (dim(data)[2] != ncols)
        {
          tkmessageBox(message = "Incorrect number of columns!",icon = "error",
                       type = "ok")
          return(invisible())
        }

      nrows = dim(data)[1]

      ## create an empty tcl list
      
      if (nrows==0)
        {
          return()
        }

      ## assign the values to the listvariable
      ## assign the values to the listvariable
      storageMode <- sapply(dataFrame,FUN=storage.mode,simplify=TRUE)
      isLogical <- storageMode == "logical"

      if (isLogical[1]) tmp <- paste("{",as.numeric(dataFrame[,1]), sep="") else tmp <- paste("{",dataFrame[,1], sep="")

      if (ncols > 1)
        {
          for (i in 2:ncols)
            {
              if (isLogical[i]) tmp <- paste(tmp,as.numeric(dataFrame[,i]), sep="} {") else tmp <- paste(tmp,dataFrame[,i], sep="} {")
            }
        }
      tmp <- paste(tmp,"}",sep="")
      tmp <- paste(tmp,collapse="} {")

      .Tcl(paste("set ", as.character(tablelist[["listvariable"]])," [list {",tmp,"} ]", sep=""))
### vecchio
#      tmp <- paste(rep("{}",ncols),collapse=" ")
#      tmp <- paste(c("{",tmp,"}"),collapse="")
#      tmp <- paste(rep(tmp,nrows),collapse=" ")
#      tmp <- paste(tmp,collapse="")

      ## assign to the listvariable
#      .Tcl(paste("set ", as.character(tablelist[["listvariable"]])," [list ",tmp," ]", sep="")) 
  
      ## assign the values to the listvariable
#      tmp <- paste("{",convertRtoTclObj(data[,1]),"}", sep="")

#      .Tcl(paste(tclTableName,"columnconfigure 0 -text",tmp))
#      if (ncols > 1)
#        {
#          for (i in 2:ncols)
#            {
#              tmp <- paste("{",convertRtoTclObj(data[,i]),"}", sep="")
#              .Tcl(paste(tclTableName,"columnconfigure",i-1,"-text",tmp))
#            }
#        }

                             # check if necessary to set the image in a logical field
      for (i in 1:ncols)
        {
          if (m.columnInfo["colClass",i] == "logical")
            {
              colValues <- convertRtoTcl("logical",dataFrame[,i])
              cols <- i-1
              .Tcl(paste(tclTableName," cellconfigure ",0:(nrows-1),",",cols,
                         " -image ",checkedImg[colValues],sep="",collapse=";"))
            }
        }
    }


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
      isCommand <- sortMode == "command"
      sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),"-sortmode",sortMode)
      .Tcl(paste(sortMode,collapse=" ; "))
      
      ## specify the command to used for sorting the sortMode == "command" corresponding to doubles 
      if (any(isCommand))
        {
          sortMode = paste(tclTableName,"columnconfigure",(0:(ncols-1))[isCommand],
                     "-sortcommand RnumberCompare")
          .Tcl(paste(sortMode,collapse=" ; "))
        }
      rm(isCommand,sortMode)
    }

                                        # define the addToSortColumns command
  if (multiSortColumn & !sortColumn)
    {
      tkconfigure(tablelist[["tablelist"]],labelcommand="tablelist::addToSortColumns")
      if (!sortColumn)
        {
          sortMode <- get.tclSortModes(tclTypes)
          isCommand <- sortMode == "command"
          sortMode = paste(tclTableName,"columnconfigure",0:(ncols-1),"-sortmode",sortMode)
          .Tcl(paste(sortMode,collapse=" ; "))
          
          ## specify the command to used for sorting the sortMode == "command" corresponding to doubles 
          if (any(isCommand))
            {
              sortMode = paste(tclTableName,"columnconfigure",(0:(ncols-1))[isCommand],
                           "-sortcommand RnumberCompare")
              .Tcl(paste(sortMode,collapse=" ; "))
            }
          rm(isCommand,sortMode)
        }        
    }
 
 
   tablelist[["resetSort"]] <- function()
    {
      .Tcl(paste(tclTableName,"resetsortinfo"))    
    }
    
  if (multiSortColumn | sortColumn)
    {
      #resetSortInfo <- function()
      #  {
      #    .Tcl(paste(tclTableName,"resetsortinfo"))
      #  }
      tkbind(tclvalue(.Tcl(paste(tclTableName, "bodytag"))),
             "<Double-Button-1>",tablelist[["resetSort"]])
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
      #xscr <- tkscrollbar(tablelist[["frame"]],orient="horizontal", command=function(...)tkxview(tablelist[["tablelist"]],...))
      #tkconfigure(tablelist[["tablelist"]],xscrollcommand=function(...) tkset(xscr,...))
      xscr <- tkscrollbar(tablelist[["frame"]],orient="horizontal")
      .Tcl(paste(xscr$ID,"configure -command [list", tclTableName ,"xview]"))      
      .Tcl(paste(tclTableName,"configure -xscrollcommand [list", xscr$ID, "set]"))
            
      tablelist[["xscr"]] <- xscr
      tkgrid(tablelist[["xscr"]],row=1,column=0,sticky="new")
    }

  if (withScrollBarY)
    {
      #yscr <- tkscrollbar(tablelist[["frame"]],orient="vertical", command=function(...)tkyview(tablelist[["tablelist"]],...))
      #tkconfigure(tablelist[["tablelist"]],yscrollcommand=function(...) tkset(yscr,...))
      yscr <- tkscrollbar(tablelist[["frame"]],orient="vertical")
      .Tcl(paste(yscr$ID,"configure -command [list", tclTableName ,"yview]"))      
      .Tcl(paste(tclTableName,"configure -yscrollcommand [list", yscr$ID, "set]"))

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
  tablelist[["get.columns"]] <- function(i=1,adjust=TRUE)
    {
     if (adjust) i <- i-1
     tmp <- as.character(.Tcl(paste(tclTableName," getcolumns {",i[1],"}",sep="")))
     out <- data.frame(I(convertTcltoR(storageModes.v[i[1]+1],tmp)))
     if (length(i)>1)
      {
        for (j in i[-1])
          {
            tmp <- as.character(.Tcl(paste(tclTableName," getcolumns {",j,"}",sep="")))       
            out <- cbind(out,I(convertTcltoR(storageModes.v[j+1],tmp)))
          }
      }
     dimnames(out)[[2]] <- tablelist[["columnlabels"]][i+1]
     return(out)
    }
  tablelist[["get.row"]] <- function(i=1,adjust=TRUE)
    {
      if (length(i) == 0)
        {
          dataFrame <- as.data.frame(zeroType(storageModes.v),stringsAsFactors=FALSE)
          dimnames(dataFrame) <- list(NULL,tablelist[["columnlabels"]])
          return(dataFrame) 
        }     
      if (adjust) i <- i - 1
      id <- tablelist[["get.tclname.listvariable"]]()
      nrows = length(i)
      # create a matrix
      m <- matrix(nrow=nrows,ncol=ncols)
      for (j in 1:nrows)
        {
          m[j,] = as.character(.Tcl(paste("lindex $",id," ",i[j],sep="")))
        }
      dataFrame <- data.frame(I(convertTcltoR(storageModes.v[1],m[,1])))
      if (ncols > 1)
        {
          for (i in 2:ncols)
            {
              dataFrame[[i]] <- convertTcltoR(storageModes.v[i],m[,i])
            }
        }
        colnames(dataFrame) <- columnlabels.v
        return(dataFrame)
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
  tablelist[["add.raw.row"]] <- function(newRows,index="end",rowNames)
    {
      # get the number of rows
      nbrows <- tablelist[["get.nbrows"]]()
      
      # insert the newRows vector in the tablelist
      nbNewRows <- length(newRows)
      if (nbNewRows==0) return()
      tmp <- paste(newRows,collapse="} {")
      tmp <- paste("{",tmp,"}",sep="")
      .Tcl(paste(tclTableName,"insert",index,tmp))
      
      # assign the names to the new rows
      if (requireRowName)
        {
          tmp <- seq(from=nbrows,length.out=nbNewRows)
          if (missing(rowNames))
            {
              .Tcl(paste(tclTableName,"rowconfigure",tmp,"-name",tmp,collapse=";"))
            }
          else
            {
              .Tcl(paste(tclTableName,"rowconfigure",tmp,"-name",rowNames,collapse=";"))
            }
        }
    }
  tablelist[["add.rows"]] <- function(newRows,index="end",rowNames)
    {
      # It is not possible to insert at once giving a vector of indices
      if (length(index)>1)
        {
          tkmessageBox(message="function tablelist[[\"add.rows\"]]: index must be scalar!",icon="error",type="ok")
          return()
        }
        
      # get the number of rows
      nbrows <- tablelist[["get.nbrows"]]()

      # insert the newRows
      if (is.data.frame(newRows) | is.matrix(newRows))
        { 
          # verify the dimensions of newRows
          d <- dim(newRows)
          if (ncols!=d[2])
            {
              tkmessageBox(message=paste("Function tablelist[[\"add.rows\"]]:\n",
                                         "Invalid number of columns: expected ",ncols,
                                         " entered ",d[2]," instead",sep=""
                                         )
                                         ,icon="error",type="ok")
              return()
            }
          if (d[1]==0) return()
          nbNewRows <- d[1] 
        
          # construct the list
          tmp <- convertRtoTcl(storageModes.v[1],newRows[,1])
          if (ncols>1)
            {
              for (i in 2:ncols)
                {
                  tmp <- paste(tmp,convertRtoTcl(storageModes.v[i],newRows[,i]),sep="} {")
                }
            }
          tmp <- paste("{{",tmp,"}}",sep="")
          
          # insert consecutively all rows 
          tmp <- paste(tmp,collapse=" ")
          if (index=="end")
            {  
              .Tcl(paste(tclTableName,"insert",index,tmp))
            } 
          else
            {
              .Tcl(paste(tclTableName,"insert",as.numeric(index)-1,tmp))
            }
        
          ## if logical add the chechbox
          areLogical <- m.columnInfo["colClass",]=="logical"
          tmp <- seq(from=nbrows,length.out=nbNewRows)
          for (i in (1:ncols)[areLogical])
            {
              colValues <- convertRtoTcl(storageModes.v[i],newRows[,i])
              cols <- i-1
              .Tcl(paste(tclTableName," cellconfigure ",tmp,",",cols,
                     " -image ",checkedImg[colValues],sep="",collapse=";"))
            }
        }
      else
        {
          # then newRows must be a vector!
          nbNewRows = 1
          tmp <- paste(newRows,collapse="} {")
          tmp <- paste("{{",tmp,"}}",sep="")
          if (index=="end")
            {  
              .Tcl(paste(tclTableName,"insert",index,tmp))
            } 
          else
            {
              .Tcl(paste(tclTableName,"insert",as.numeric(index)-1,tmp))
            }
          
          ## if logical add the chechbox
          areLogical <- m.columnInfo["colClass",]=="logical"
          tmp <- seq(from=nbrows,length.out=nbNewRows)
          for (i in (1:ncols)[areLogical])
            {
              colValues <- convertRtoTcl(storageModes.v[i],newRows[,i])
              cols <- i-1
              .Tcl(paste(tclTableName," cellconfigure ",tmp,",",cols,
                     " -image ",checkedImg[colValues],sep="",collapse=";"))
            }         
        }
      
      # assign the names to the new rows
      if (requireRowName)
        {
          tmp <- seq(from=nbrows,length.out=nbNewRows)
          if (missing(rowNames))
            {
              .Tcl(paste(tclTableName,"rowconfigure",tmp,"-name",tmp,collapse=";"))
            }
          else
            {
              .Tcl(paste(tclTableName,"rowconfigure",tmp,"-name",rowNames,collapse=";"))
            }
        }
    }
  tablelist[["get.table"]] <- function()
    {
      listname <- as.character(tablelist[["listvariable"]])
      listLength <- as.numeric(.Tcl("llength $" %+% listname))
      if (listLength > 0)
        {
          rnames <- 1:listLength
                                        # determine the length of the sublist
          sublistLength = as.numeric(.Tcl(paste("llength [lindex $" %+% listname,"0 ]")))
          if (sublistLength > 0)
            {
              M <- matrix(nrow=listLength,ncol=sublistLength)
              for (i in 1:listLength)
                {
                  for (j in 1:sublistLength)
                    {
                      M[i,j] <- tclvalue(.Tcl(paste("lindex [lindex $",listname," ",i-1,"] ",
                                                    j-1,sep="")))
                    }
                }
            }
             
          dataFrame <- data.frame(I(convertTcltoR(storageModes.v[1],M[,1])))
          if (sublistLength > 1)
          {
            for (i in 2:sublistLength)
            {
              dataFrame[[i]] <- convertTcltoR(storageModes.v[i],M[,i])
            }
          }
          dimnames(dataFrame) <- list(rnames,tablelist[["columnlabels"]])
          return(dataFrame)
        }
      else
        {
          dataFrame <- as.data.frame(zeroType(storageModes.v),stringsAsFactors=FALSE)
          dimnames(dataFrame) <- list(NULL,tablelist[["columnlabels"]])
          return(dataFrame)
        }
    }
  tablelist[["get.nbrows"]] <- function()
    {
      return(as.numeric(tclvalue(.Tcl(paste(tclTableName,"index end")))))
    }
  tablelist[["get.selection.rowNumber"]] <- function(adjust=TRUE)
    {
      selection <- as.numeric(tcl(tablelist[["tablelist"]],"curselection"))
      
      if (length(selection)==0) return(numeric(0))
      
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
  tablelist[["hidecolumns"]] <- function(hide=0,columns="0",adjust=TRUE)
    {
      ## columns must be a vector of names or integers
      if (length(columns)==0) return()
        
      if (is.integer(columns) & adjust)
        {
          columns=as.character(columns-1)
        }
      .Tcl(paste(tclTableName," columnconfigure {",columns,"} -hide ",hide,sep="",collapse=";"))
    }
  tablelist[["selection.clear"]] <- function(start=0,end="end")
    {
     if (missing(end))
      {
        .Tcl(paste(tclTableName,"selection clear",start,end))
        return()      
      }

     .Tcl(paste(tclTableName,"selection clear {",start,end,"}"))
    }

  tablelist[["finishediting"]] <- function () {.Tcl(paste(tclTableName,"finishediting"))}
                                        # return the list
  return(tablelist)
}
                                        # end tablelist widget





create_selectionTablelist <- function(
                             parent=topWindow,dataFrame1,dataFrame2,title="",width=60,height=15,
                             withScrollBarX=FALSE,withScrollBarY=FALSE,sortColumn=FALSE,
                             multiSortColumn=TRUE,selectmode="multiple",columnInfo=list(),grabWindow)
  {
    tablelist <- list() # create list containing the widget
    tablelist[["frame"]] <- ttkframe(parent)

                                          # get the dimensions of the dataFrame
    ncols <- dim(dataFrame1)[2]
    columnlabels <- dimnames(dataFrame1)[[2]]

    # split according to the available and desired fields
    tmp <- available.desired.dataFrames(dataFrame1,dataFrame2)
    dataFrame1 <- tmp[["dataFrame1"]]
    dataFrame2 <- tmp[["dataFrame2"]]
    rm(tmp)

    # construct the tablelist widgets
    if (missing(columnInfo))
       {
         tablelist1 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame1,
                        width=width,height=height,withScrollBarX=withScrollBarX,
                        withScrollBarY=withScrollBarY,sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode) #,color="#e0e8f0"

         tablelist2 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame2,
                        width=width,height=height,withScrollBarX=withScrollBarX,
                        withScrollBarY=withScrollBarY,sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode) #,color="linen"
       } else {
         tablelist1 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame1,
                        width=width,height=height,withScrollBarX=withScrollBarX,
                        withScrollBarY=withScrollBarY,sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode,columnInfo)

         tablelist2 <-  create_tablelist(parent=tablelist[["frame"]],dataFrame=dataFrame2,
                        width=width,height=height,withScrollBarX=withScrollBarX,
                        withScrollBarY=withScrollBarY,sortColumn=sortColumn,multiSortColumn=multiSortColumn,
                        selectmode=selectmode,columnInfo) #,color="linen"
      }


    labelTitle <- ttklabel(tablelist[["frame"]],text=title)

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


    label1 <- ttklabel(tablelist[["frame"]],text="Available")
    label2 <- ttklabel(tablelist[["frame"]],text="Selected")

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


create_ttkslider <- function(parent=topWindow,orient="horizontal",
                          from=0,to=100,length=100,initialValue=from,
                          command)
  {
    slider <- list()
    sliderValue <- tclVar(initialValue)
    slider[["slider"]] <- .Tk.subwin(parent)
    .Tcl(paste("ttk::scale",slider[["slider"]]$ID))
    tkconfigure(slider[["slider"]],from=from,to=to,variable=sliderValue,
                length=length,orient=orient)
    if (!missing(command)) tkconfigure(slider[["slider"]],command=command)

    slider[["set.value"]] <- function(value)
      {
        tclvalue(sliderValue) <<- value
      }

    slider[["get.value"]] <- function()
      {
        return(as.numeric(tclvalue(sliderValue)))
      }
    return(slider)
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
    slider[["set.value"]] <- function(value)
      {
        tclvalue(sliderValue) <<- value
      }

    slider[["get.value"]] <- function()
      {
        return(as.numeric(tclvalue(sliderValue)))
      }
    return(slider)
}



create_frame <- function(parent=topWindow)
{
  widget <- list() # create list containing the widget, used "widget" instead of
                                        # "frame" because frame is a reserved name
  widget[["frame"]] <- ttkframe(parent)
  tkconfigure(widget[["frame"]],background="#EDEEF1")
  return(widget)
}

create_labelFrame <- function(parent=topWindow,text="",labelanchor="nw")
{
  labelFrame <- list() # create list containing the widget
  labelFrame[["labelFrame"]] <- ttklabelframe(parent,text=text,
                                labelanchor=labelanchor,background="#EDEEF1")
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
  myFrame <- ttkframe(parent)
  b.save <- create_button(parent=myFrame,text="Save",command=onSave)
  b.cancel <- create_button(parent=myFrame,text="Cancel",command=onCancel)

  tkgrid(b.save[["button"]],b.cancel[["button"]],padx=padx,pady=pady)
  return(myFrame)
}


create_okCancelButton <- function(parent=topWindow,onOk=function(){},onCancel=function(){})
{
  myFrame <- ttkframe(parent)
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
