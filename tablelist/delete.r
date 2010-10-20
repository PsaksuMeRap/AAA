library(tcltk)
library(tkrplot)
tclRequire("Iwidgets")

tt <- tktoplevel()
tkpack(tn <- tkwidget(tt, "iwidgets::tabnotebook"))
tkconfigure(tn,                         # prettyfication taken from incrtcl
            tabpos="n")

nm <- LETTERS[1:3]
for (t in 1:length(nm)) {
    tbn <- tclvalue(tkadd(tn, label=nm[t]))
    tkpack(tbw <- .Tk.newwin(tbn))
    tkpack(fr <- tkframe(tbw))
    tkpack(lb <- tklabel(fr, text=paste("This is tab", nm[t])))
    ID <- paste(tn$ID, evalq(num.subwin<-num.subwin+1, tn$env), sep=".")
    win <- .Tk.newwin(ID)
    assign(ID, tbw, envir = tn$env)
    assign("parent", tn, envir = tbw$env)
}
tkbind(tt, "<Destroy>", function() tkdestroy(tn))
tkselect(tn, 0)