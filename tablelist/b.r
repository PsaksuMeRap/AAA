



#create_bottons <- function (top=top)
#{
#    a <- tclVar("Etichetta iniziale")
#    b <- tklabel(top, text=tclvalue(a))
#    tkconfigure(b,textvariable=a)
#    labeltext <- tclVar("Bottone iniziale")
#    tkgrid(b, padx="3", pady="6")
#    f1 <- function ()
#	{
#		tclvalue(a) <<- "This text label has changed!"
#	}
#    return(list(change_etichetta=f1))
#}

create_bottons <- function (env=top, OnOk=OnOk, OnCancel=OnCancel)
{
	ok <- tkbutton(env, text="Ok", command=OnOk)
	Cancel <- tkbutton(env, text="Cancel", command=OnCancel)
	tkgrid(ok, Cancel)
}

remm <- function()
  {
    require(tcltk)
    top <- tktoplevel()
    button_frame <- tkframe(top)

    OnOk <- function ()
	{
		print("Benissimo")
	}

    OnCancel <- function ()
	{
		tkdestroy(top)
	}

    label.1 <- tklabel(top, text="Questo è un esempio!")

    # Show the widgets
    tkgrid(label.1, padx="2m", pady=c("0","2m"))
    create_bottons(button_frame, OnOk, OnCancel)
    tkgrid(button_frame)
    tkfocus(top)
}


