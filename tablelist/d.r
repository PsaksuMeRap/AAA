require(tcltk)
tclRequire("BWidget")
tclRequire("Tktable")
tclRequire("Tablelist")


tablelist::tablelist $test -columns {0 "Col1" 0 "Col2"} -labelcommand tablelist::sortByColumn -sortcommand demo::compareAsSet -height 15 -width 20 -stretch all
.Tcl("$tablelist::library")
.Tcl("toplevel .test")
.Tcl("tablelist::tablelist $test -columns {0 "Col1" 0 "Col2"} ")
.Tcl("toplevel .test")
.Tcl("tablelist::tablelist $test.t -columns {0 "Col1" 0 "Col2"} ")
.Tcl("frame .test.frame")
.Tcl("tablelist::tablelist test.t -columns {0 "Col1" 0 "Col2"} ")
?tktablelist
?tablelist
.Tcl("toplevel .top")
.Tcl("set tf $top.tf")
.Tcl("set tf .top.tf")
.Tcl("frame .top.tf")
.Tcl("frame $tf")
.Tcl("set tb1 $tf.tb1")
.Tcl("")
paste("tablelist::tablelist $tb1 -columns {0 "AAA"}")
paste("tablelist::tablelist $tb1 -columns {0 "AAA"}")
.Tcl(paste("tablelist::tablelist $tb1 -columns {0 "AAA"}"))
.Tcl("pack $tb1")
.Tcl("pack $tf")
.TkRoot$env
ls(.TkRoot$env)
ls(.TkRoot$top)
.Tcl("pack $top")


top <- tktoplevel() # in questo caso il comando top ritorna .2 quale ID
.Tcl("tablelist::tablelist .1.1 -columns {0 \"Col1\" 0 \"Col2\"} ") # crea una tablelist nella finestra appena creata
tkpack(".1.1")
.Tcl(".1.1 insertcolumns 0 10 'mamma'") #uno alla volta!!
tkconfigure(".1.1", width=60)
tkconfigure(".1.1", stretch="all")

TK example
package require Tablelist
tablelist::tablelist .1 -columns {9 "123456789" 5 "12345"}
pack .1
.1 insertcolumns 1 3 "mamma"
.1 configure -width 30

Per beccare le informazioni disponibili:


require(tcltk)
n <- evalq(TclVarCount <- TclVarCount + 1, .TkRoot$env)
name <- paste("::RTcl", n,sep = "")
l <- list(env = new.env())
assign(name, NULL, envir = l$env)
reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
class(l) <- "tablelist"



a <- tclvalue(.Tcl(paste("winfo children ",top$ID)))
is.vector(a)
if (a[1] == "") print("è nullo")
n <- nchar(top$ID)
substring(a,n+2,nchar(a))



# esempio 1: uso di odbc e tablelist
require(tcltk)
tclRequire("BWidget")
tclRequire("Iwidgets")
tclRequire("Tktable")
tclRequire("Tablelist")
library(RODBC)


# Viene impostata la connessione al database
odbcConnection = setOdbcConnection("DBMarkowitz")


dati <- data.frame(I(c("Claudio Ortelli","Luca Ortelli","Ballabio Mauro","Arrigoni Stefano","Maurizio Ferri","Geronimo","a","a","a","Geronimo","a","a","a",,"Geronimo","a","a","a")), 2:18)
dimnames(dati)[[2]] <- c("Nome","Numero")

source("tcltk_utilities.R")
parent <- tktoplevel()
  
a <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE)
tkgrid(a$tablelist,a$yscr)
tkgrid.configure(a$yscr,sticky="ns")
tkgrid(a$xscr)
tkgrid.configure(a$xscr,sticky="we")


# esempio 2: uso di tkrplot
source("C:/Documents and Settings/claudio.AYRTON/Desktop/R_tablelist/tcltk_utilities.R")

topWindow <- tktoplevel()

a <- create_panedwindow(parent=topWindow,width="600",height="400",
                      fraction=c(50,50))

plot1 <- function()
{
 plot(1:10/10,sin(1:10/10))
}

plot2 <- function()
{
 plot(1:10,sqrt(1:10))
}

b <- create_tkplot(a[[2]],func=plot1,hscale=0.5,vscale=0.5)
c <- create_tkplot(a[[3]],func=plot1,hscale=0.5,vscale=0.5)
tkgrid(b[[1]])
tkgrid(c[[1]])
tkgrid(a[[1]])
