
create_bottons <- function (parent=topWindow)
{
    onContinue = function () {print ("Continue")}
    onCancel = function () {print ("Cancel")} 
    b <- tklabel(parent, text="Selezione del modello")
    # Continue and Cancel buttons
    # <- list(frame_buttons=tkframe(parent))
    labeltext <- tclVar("Continue")
    c <- tkbutton(parent, text=tclvalue(labeltext), command=onContinue)
    d <- tkbutton(parent, text="Cancel", command=onCancel)
    tkgrid(c, d, padx="3", pady="6")
    return(list(etichetta=b,labella=labeltext,continua=c,cancella=d))
}


remm <- function()
  {
    require(tcltk)


    # Create the exchange environment
    main <- new.env()
    
    # Create the project list containing all the information necessary to
    # create the initialization's files

    # Global variables
    # Building blocks of the structure "project" (to be removed after generation of "project")
    str <- list(valid=FALSE)

    top <- tktoplevel()

    frame_buttons <- tkframe(top)
    title <- tklabel(top, text="Model Selection")
    
    tkgrid(title, sticky="we", columnspan="2")
    tktitle(top) <- "Model Selection"

    # Show the widgets
    tkgrid(title, padx="2m", pady=c("0","2m"))
    b1 = create_bottons(frame_buttons)
    #frame_buttons

    modify <- function () {tclvalue(b1[[2]]) <<- "cicco"}
    tkgrid(frame_buttons)
    
    aa <- tclVar("Modifica")
    
    c <- tkbutton(top, text=tclvalue(aa), command=modify)
    tkgrid(c)
    tkfocus(top) 
    tclvalue(aa) <- "pippo" 
}


