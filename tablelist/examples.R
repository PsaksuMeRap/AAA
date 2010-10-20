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

tkgrid(label.1, padx="2m", pady=c("0","2m"))
tkgrid(title, sticky="we", columnspan="2")
tktitle(top) <- "Model Selection"
tkgrid(title, padx="2m", pady=c("0","2m"))
tkgrid(c)
tkfocus(top)


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


# esempio 3: uso di odbc & tablelist
source("C:/Documents and Settings/claudio.AYRTON/Desktop/R_tablelist/tcltk_utilities.R")
# Viene impostata la connessione al database
odbcConnection = setOdbcConnection("DBMarkowitz")
dati <- get.table(odbcConnection, "Copia_DBAzioni")
parent <- tktoplevel()

b <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE)
tkgrid(b$frame)




# esempio 4: costruzione di panedwindow con tkrplot
tt <- tktoplevel()
a <- create_panedwindow(tt,
a <- create_tkplot(tt)
b <- crate_tkplot(tt)
tkgrid(a[[1]])

create_panedwindow <- function(parent=topWindow,width="300",height="200",
                      fraction,nbPanes,orient="vertical")


> a[["replot"]](func=p,vscale=1.5,hscale=1.2)
Error in a[["replot"]](func = p, vscale = 1.5, hscale = 1.2) :
        unused argument(s) (vscale ...)
> a[["replot"]](func=p,vsc=1.5,hsc=1.2)
<Tcl> Rplot6
> source(file("clipboard"))
> tt <- tktoplevel()
> a <- create_tkplot(tt)
> tkgrid(a[[1]])
<Tcl>
> a[["replot"]](func=p,vscale=1.5,hscale=1.2)
<Tcl> Rplot7
> a[["replot"]](func=p)
<Tcl> Rplot7



# esempio 5: costruzione di maschera usando tkgrid
get_tickers <- function (data.iniziale, data.finale) {
  channel.psa.VAR <- odbcConnect("prezzi_storici_VAR", "sa", "")
  query <- paste("SELECT DISTINCT Ticker FROM TotalePrezziStorico", sep="")
  #query <- paste("SELECT DISTINCT Ticker FROM TotalePrezziStorico WHERE (TRADE_DATE >= '", sep="")
  #query <- paste(query, tclvalue(data.iniziale), "') AND Ticker IN (SELECT DISTINCT TICKER FROM TotalePrezziStorico WHERE ", sep="")
  #query <- paste(query, "(TRADE_DATE <= '", tclvalue(data.finale), "'))", sep="")
  #query <- paste(query," ORDER BY Ticker", sep="")

  result <- sqlQuery(channel.psa.VAR, query, as.is=TRUE)
  return(as.vector(result[,1]))
}


library(RODBC)
require(tcltk)

main <- function () {

fontHeading <- tkfont.create(family="times",size=14,weight="bold",slant="italic")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)

# Definisci la data iniziale e finale
date.end <- tclVar()
tclvalue(date.end) <- format.Date(Sys.Date()-1, "%m-%d-%Y")
date.start <- tclVar()
tclvalue(date.start) <- format.Date(Sys.Date()-1090, "%m-%d-%Y")

# Recupera la lista di Tickers
Tickers <- get_tickers(date.start, date.end)

# Crea la finestra principale
tt <- tktoplevel()

# Crea una label principale
lab.main.string <- tclVar()
tclvalue(lab.main.string) <- paste("Time Series from ", tclvalue(date.start), " to ", tclvalue(date.end), sep="")
lab.main <- tklabel(tt, text=tclvalue(lab.main.string),textvariable=lab.main.string,font=fontHeading)
tkgrid(lab.main,columnspan=2)

# Costruisci la lista dei Tickers
scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(lst.Tickers,...))
lst.Tickers <- tklistbox(tt,height=10,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
tkgrid(lst.Tickers,scr)
tkgrid.configure(lst.Tickers,sticky="nswe")
tkgrid.configure(scr,columnspan=2,sticky="nsw")


# Riempi la lista
for (Ticker in Tickers) {
  tkinsert(lst.Tickers,"end",Ticker)
}

# Set the default ticker as 0 (Indexing starts at zero).
tkselection.set(lst.Tickers,0)

f.modify.dates <- function() {

  ok <- function() {
    tclvalue(lab.main.string) <<- paste("Time Series from ", tclvalue(date.start), " to ", tclvalue(date.end), sep="")
    return
  }

  tt.modify.dates <- tktoplevel()
  heading <- tklabel(tt.modify.dates,text="Modify Range")
  lab.empty <- tklabel(tt.modify.dates,text="")
  lab.format <- tklabel(tt.modify.dates,text="mm-dd-yyyy")

  label.data.inizio <- tklabel(tt.modify.dates,text="Data d'inizio")
  label.data.fine <- tklabel(tt.modify.dates,text="Data di fine")
  entry.data.inizio <- tkentry(tt.modify.dates,textvariable=date.start,width=10)
  entry.data.fine <- tkentry(tt.modify.dates,textvariable=date.end,width=10)

  tkgrid(heading, columnspan=2)
  tkgrid(lab.empty, lab.empty)
  tkgrid(lab.empty, lab.format)

  tkgrid(label.data.inizio, entry.data.inizio)
  tkgrid(label.data.fine, entry.data.fine)
  tkgrid.configure(entry.data.inizio, entry.data.fine, sticky="w")
  tkgrid.configure(label.data.inizio, label.data.fine, sticky="w")
  tkgrid(tkbutton(tt.modify.dates,text="OK",command=ok),
         tkbutton(tt.modify.dates,text="Cancel",command=function() tkdestroy(tt.modify.dates)))
  # Distruggi e ricrea la maschera con la lista
  # tkdestroy(tt)

}

tkgrid(tkbutton(tt,text="Modify dates",command=f.modify.dates),
       tkbutton(tt,text="Cancel",command=function() tkdestroy(tt)))

}

main()



# tree con picture

require(tcltk)
tclRequire("BWidget")

tt <- tktoplevel()
tkwm.title(tt,"Tree (Drill-Down) Widget")

xScr       <- tkscrollbar(tt,command=function(...)tkxview(treeWidget,...),orient="horizontal")
yScr       <- tkscrollbar(tt,command=function(...)tkyview(treeWidget,...))
treeWidget <- tkwidget(tt,"Tree",xscrollcommand=function(...)tkset(xScr,...),
                                 yscrollcommand=function(...)tkset(yScr,...),width=30,height=15)
tkgrid(treeWidget,yScr)
tkgrid.configure(yScr,stick="nsw")
tkgrid(xScr)
tkgrid.configure(xScr,stick="new")

image1 <- tclVar()
image2 <- tclVar()
tkcmd("image","create","photo",image1,file="chf.gif")
tkcmd("image","create","photo",image2,file="chf.gif",width=20,height=14)
tkcmd(image2, "copy", image1,"shrink")

# Insert at the end of the nodes in "root" a new node, called
# "Record1Node", which displays the text "Record 1", etc.
tkinsert(treeWidget,"end","root","Record1Node",image=image1)
tkinsert(treeWidget,"end","root","Record2Node",text="Record 2")


# create a tablelist con checkbutton
require(tcltk)
tclRequire("BWidget")
tclRequire("Iwidgets")
tclRequire("Tktable")
tclRequire("Tablelist")
library(RODBC)
source("C:/Documents and Settings/claudio.AYRTON/Desktop/R_tablelist/tcltk_utilities.R")

dati <- data.frame(pippo=as.numeric(rnorm(10)>0),mamma=as.numeric(rnorm(10)<0))

parent <- tktoplevel()

b <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE,editable=rep("checkbutton",2))
tkgrid(b$frame)


# Definire procedure in Tcl
  # define the tcl variable containing the reference to the R function
f <- funcion(...) print("Prova da tcl") # Nota: i ... sono necessari in quanto
                                        # in questo caso sono passati 4 comandi
                                        # se non venisse passato nulla non sareb-
                                        # necessario ... .
x = .Tcl.args(f)
print(x)
  # togli le parentesi graffe (veramente necessario)
x = gsub(' \\{ R_call','R_call',x)
x = gsub(' \\}$','',x)
print(as.character(x))

# definisci la variabile tcl contenente il riferimento tcl al comando R
print(paste("set ","::rTl",gsub('\\.+', '',tablelist[["tablelist"]]$ID)," ","\"",x,"\"",sep=""))
.Tcl(paste("set ","::rTl",gsub('\\.+', '',tablelist[["tablelist"]]$ID)," ","\"",x,"\"",sep=""))
#.Tcl(paste("set ","::rTl",gsub('\\.+', '',tablelist[["tablelist"]]$ID)," ",x,sep=""))
editCmdString <- paste(
       "proc editStartCmd {tbl row col text} {\n ",
       "set ::rTablelist.tbl $tbl; set ::rTablelist.row $row; set ::rTablelist.col $col; set ::rTablelist.text $text \n ",
       "regsub -all {\\.} $tbl {} a1 \n ",
       "set a2 \"::rTl\" \n ",
       "set a3 $a2$a1 \n ",
       "eval [set [set a3]] { a b c {d e}} \n ",
       "return $a2 \n ",
       "}"
       )
       .Tcl(editCmdString)

tkconfigure(tablelist[["tablelist"]],editstartcommand="editStartCmd")


# ---------------------------------------------
# create a tablelist con checkbutton e ComboBox
require(tcltk)
tclRequire("BWidget")
tclRequire("Iwidgets")
tclRequire("Tktable")
tclRequire("Tablelist")
library(RODBC)

source("C:/R/R_tablelist/tcltk_utilities.R")
source("C:/R/R_tablelist/odbc_utilities.R")


# Viene impostata la connessione al database
odbcConnection = setOdbcConnection("DBMarkowitz")
#dati <- tcl.get.table(odbcConnection, "Cancellame",requiredFields=c("Data","Numero float","Vero"),columnNames=c("mia a","cla","fede"),updateDb=T)
#mtrace(tcl.get.table)


dati <- tcl.get.table(odbcConnection,"Universo_Markowitz",requiredFields=list(uno="Ticker","due 2"="Company","Branche","Specification Branche"),updateDb=T)


parent <- tktoplevel()

#b <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE,editable=c("Entry","ComboBox","Entry"),
#     editStartValue=list("cla"=c(1.0,1.1,1.223)),colFormats=c("no","Round2","no"),updateDb=T)

b <- create_tablelist(parent,dati,withScrollBarX=TRUE,withScrollBarY=TRUE,width=100,
editable=c("no","no","ComboBox","no"),editStartValue=list("Branche"=c("mamma","papa")),updateDb=T)


tkgrid(b[["frame"]])

 
query = "sp_pkeys 'Cancellame'"
query = "sp_columns 'DBUniverso_per_ottimizzazione'"
dat <- get.table(odbcConnection, query=query)

#sp_statistics [@table_name =]
#SQLStatistics


# Esempio 10: come creare widget che si espandono in proporzione definita
    # place the left and right frames
    tkgrid(f.left[["labelFrame"]],f.right,padx=padx,pady=pady, sticky="nswe")
    tkgrid(tklabel(parent=f.left[["labelFrame"]],text="Client:"),l.client[["label"]],
           padx=padx,pady=pady,sticky="nw")
    tkgrid(tklabel(parent=f.left[["labelFrame"]],text="Currency:"),l.currency[["label"]],
           padx=padx,pady=pady,sticky="nw")
    tkgrid(r.currency[["label"]],sticky="w")

    .Tcl(paste("grid columnconfigure",window.SectorsCurrencies$ID,0,"-weight 1"))
    .Tcl(paste("grid columnconfigure",window.SectorsCurrencies$ID,1,"-weight 10"))
    .Tcl(paste("grid rowconfigure",window.SectorsCurrencies$ID,0,"-weight 1"))
    # .Tcl(paste("grid rowconfigure",f.left[["labelFrame"]],0,"-weight 1"))
    # .Tcl(paste("grid rowconfigure",f.left[["labelFrame"]],1,"-weight 2"))

