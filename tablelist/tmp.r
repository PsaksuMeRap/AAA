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
            menu[[i+1]] <- tkadd(parentMenu,"radio",label=labels[i],variable=menu[["variable"]])
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

  
require(tcltk)
tt <- tktoplevel()
topMenu <- create_topmenu(tt)
alpha <- tclVar("pippo")

fileMenu <- create_menu(topMenu[["menu"]])

openRecentMenu <- create_menu(topMenu[["menu"]])
openRecentMenu[["addCommand"]](label="Recent File 1",
        command=function() tkmessageBox(message="I don't know how to open Recent File 1",
        icon="error"))


openRecentMenu[["addCommand"]](label="Recent File 2",
command=function() tkmessageBox(message="I don't know how to open Recent File 2",icon="error"))

openRecentMenu[["addRadio"]](label="pippo",variable=alpha,value="pippo")
fileMenu[["addCascadeMenu"]](menu=openRecentMenu[["menu"]],label="Open recent file")
fileMenu[["addCommand"]](label="Quit",command=function() tkdestroy(tt))
topMenu[["addCascadeMenu"]](menu=fileMenu[["menu"]],label="File")

#tkfocus(tt)
  
  
  