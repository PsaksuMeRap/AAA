library("tcltk")
tclRequire("BWidget")
tclRequire("Iwidgets")

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

create_tabnotebook <- function(parent,label=c("uno"),tabpos="n",tabbackground="white",
                               background="lightgray",width=600,height=400)
  {
    # create the widget
    tbn <- list()

    tbn[["tabnotebook"]] <- tkwidget(parent, "iwidgets::tabnotebook")
    tkconfigure(tbn[["tabnotebook"]],tabpos="n",tabbackground=tabbackground,
                background=background,angle=0,bevelamount=2,gap=2,margin=2,
                tabborders=0,width=width,height=height)

    nbTabs <- length(label)
    for (i in 1:nbTabs)
      {
        t <- tclvalue(tkadd(tbn[["tabnotebook"]], label=label[i]))
        tbw <- .Tk.newwin(t)
        tkpack(tbw)
        name <- paste("frame",i,sep="")
        tbn[[name]] <- tkframe(tbw)
        tkpack(tbn[[name]])
        ID <- paste(tbn[["tabnotebook"]]$ID, evalq(num.subwin<-num.subwin+1,
                    tbn[["tabnotebook"]]$env), sep="."
                   )
        win <- .Tk.newwin(ID)
        assign(ID, tbw, envir = tbn[["tabnotebook"]]$env)
        assign("parent", tbn[["tabnotebook"]], envir = tbw$env)
      }
    rm(name)

    tbn[["select.tab"]] <- function(i=1,adjust=TRUE)
      {
        if (adjust) i <- i - 1
        tkselect(tbn[["tabnotebook"]], i)
        return()
      }

    tbn[["delete.tab"]] <- function(from=1,to=from,adjust=TRUE)
      {
        if (adjust) from <- from - 1
        if (adjust & !missing(to)) to <- to - 1
        tkdelete(tbn[["tabnotebook"]],from,to)
        nbTabs <<- nbTabs - (to - from + 1)
      }

    tbn[["add.tab"]] <- function(frameName,label)
      {
        if (missing(frameName))
          {
            frameName <- paste("frame",nbTabs+1,sep="")
          }
        if (missing(label)) label <- frameName
        nbTabs <<- nbTabs + 1
        t <- tclvalue(tkadd(tbn[["tabnotebook"]], label=label))
        tbw <- .Tk.newwin(t)
        tkpack(tbw)

        attach(tbn)
        tbn[[frameName]] <- tkframe(tbw)
        detach(tbn)
        tkpack(tbn[[frameName]])
        ID <- paste(tbn[["tabnotebook"]]$ID, evalq(num.subwin<-num.subwin+1,
                    tbn[["tabnotebook"]]$env), sep="."
                   )
        win <- .Tk.newwin(ID)
        assign(ID, tbw, envir = tbn[["tabnotebook"]]$env)
        assign("parent", tbn[["tabnotebook"]], envir = tbw$env)
      }

    tbn[["nbTabs"]] <- function() {return(nbTabs)}

    tbn[["select.tab"]]()
    tbn[["frame"]] <- environment()
    
    return(tbn)
  }
  

topWindow <- tktoplevel()  #used in get.riskFactors
tktitle(topWindow) <- "Expectations analysis"


 ## create the tabnotebook widget
  tbn.notebook <- create_tabnotebook (parent=topWindow,label=c("Data","Report"),height=100,width=300,
                                        tabpos="n",tabbackground="white",background="systemButtonFace")
remove.tab <- function()
{
 tbn.notebook[["delete.tab"]](from=1,to=2)
}

add.tab <- function()
{
 tbn.notebook[["add.tab"]](frameName="a1",label="pippo")
 browser()
 b.test <- create_button(parent=tbn.notebook[["a1"]],text="tet",command=remove.tab)
 #tkgrid(b.test[["button"]])
}
  b.test <- create_button(topWindow,text="teset",command=remove.tab)
  b.test1 <- create_button(topWindow,text="teset1",command=add.tab)
  
tkgrid(tbn.notebook[["tabnotebook"]],padx=padx,pady=pady,row=0,columnspan=2)
tkgrid(b.test[["button"]],b.test1[["button"]])

