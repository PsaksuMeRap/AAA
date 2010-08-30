## mettere un text field per i dati

?usare data.entry?
minimumAcceptableReturn = 2
frame.minimumAcceptableReturn <- tkframe(tabNotebook[["pages"]][[3]])
l.minimumAcceptableReturn <- create_label(parent=frame.minimumAcceptableReturn,value="Minimum acceptable return:  ")
entry.minimumAcceptableReturn <- create_entry(parent=frame.minimumAcceptableReturn, value=minimumAcceptableReturn, width="10", dataFormat=list(type="%",option=-1))

tkgrid(l.minimumAcceptableReturn[["label"]],entry.minimumAcceptableReturn[["entry"]])
tkgrid(frame.minimumAcceptableReturn,padx=padx,pady=pady)