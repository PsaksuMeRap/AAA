minimumAcceptableReturn = ""
minNbObs = ""
maxStdev = ""
maxDrawdown = ""

frame.minimumAcceptableReturn <- tkframe(tabNotebook[["pages"]][[3]])

l.minNbObs <- create_label(parent=frame.minimumAcceptableReturn,value="Minimal number of observations:  ")
l.minimumAcceptableReturn <- create_label(parent=frame.minimumAcceptableReturn,value="Minimum acceptable return p.a.:  ")
l.maxStdev <- create_label(parent=frame.minimumAcceptableReturn,value="Maximal annualized stdev:  ")
l.maxDrawdown <- create_label(parent=frame.minimumAcceptableReturn,value="Limit on the maximal % drawdown:  ")


entry.minNbObs <- create_entry(parent=frame.minimumAcceptableReturn, value=minNbObs, width="10")
entry.minimumAcceptableReturn <- create_entry(parent=frame.minimumAcceptableReturn, value=minimumAcceptableReturn, width="10", dataFormat=list(type="%",option=-1))
entry.maxStdev <- create_entry(parent=frame.minimumAcceptableReturn, value=maxStdev, width="10", dataFormat=list(type="%",option=-1))
entry.maxDrawdown <- create_entry(parent=frame.minimumAcceptableReturn, value=maxDrawdown, width="10", dataFormat=list(type="%",option=-1))


tkgrid(l.minNbObs[["label"]],entry.minNbObs[["entry"]],sticky="w")
tkgrid(l.minimumAcceptableReturn[["label"]],entry.minimumAcceptableReturn[["entry"]],sticky="w")
tkgrid(l.maxStdev[["label"]],entry.maxStdev[["entry"]],sticky="w")
tkgrid(l.maxDrawdown[["label"]],entry.maxDrawdown[["entry"]],sticky="w")
tkgrid(frame.minimumAcceptableReturn,padx=padx,pady=pady)
