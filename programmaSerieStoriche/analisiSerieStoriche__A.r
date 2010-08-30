## on unix systems it requires installation of 
## tablelist - tktable - Biwidget - tkrplot
## rodbc - unixodbc - "unixodbc-bin"
# source("/home/claudio/Dropbox/analisi serie storiche/analisiSerieStoriche__A.r")

rm(list=ls(all=TRUE))

require("fUtilities")
# detach("package:tkutilities", unload=TRUE)
library("tcltk")
#tclRequire("tile")
require("tkrplot")
#addTclPath("C:/Tcl/lib")
#try(detach("package:tkutilities", unload=TRUE))

library("tkutilities")

padx=10
pady=5

availableStatistics=c("Frequency",
"Nb. Obs.","Start date","End date",
"Min","Min at",
"Max","Max at",
"Mean","Annualized mean",
"Median","1st Qu.","3st Qu.",
"Stdev","Annualized stdev",
"Skewness","Excess kurtosis",
"Drawdown","Max Drawdown","Max Drawdown %",
"Sortino ratio","Bias ratio"
)
desiredLevels=c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,
FALSE,FALSE)
desiredReturns=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)
statistics <- data.frame(available=I(availableStatistics),desiredLevels=desiredLevels,desiredReturns=desiredReturns)
rm(availableStatistics,desiredLevels,desiredReturns)

env <- topenv()


## set the default timeseries frequency
tsFreq = "Daily"
etichettaDataInizio = "NA"
etichettaDataFine = "NA"



topWindow <- tktoplevel()
tktitle(topWindow) <- "Time series analysis"
tkconfigure(topWindow,background="#D8D8D8")

## crea il menu principale
source("analisiSerieStoriche__B__menu.r")



## crea il frame contenente le serie da selezionare
slb.timeSeries <- create_selectionListBox(parent=topWindow,title="\nTime series\n",
#                  values1=NULL,values2=NULL,mode="extended",height=10,
                  values1=NULL,values2=NULL,mode="extended",height=15,
                  width=50,withScrollBarX=TRUE,withScrollBarY=TRUE,align="v",
                  order=c(TRUE,TRUE),grabWindow=topWindow)

## crea il labeledradio contentente il tipo di serie da plottare
## e cioè levels, returns, logreturns
source("analisiSerieStoriche__B__timeSeriesType.r")
f.chartType <- ttkframe(topWindow)
l.chartType <- create_label(parent=f.chartType,value="Plot timeseries of:  ")
r.tsType <- create_radio(parent=f.chartType,values=c("Levels","Log-returns","Returns"),
                position="h",text=c("Levels","Log-returns","Returns"),command=onTsType)

## inserisci lo slider delle date
source("analisiSerieStoriche__B__slider.r")

f.buttons <- ttkframe(topWindow)
source("analisiSerieStoriche__B__tabnotebook.r")
source("analisiSerieStoriche__B__plotSelectedSeries.r")
source("analisiSerieStoriche__B__createReport.r")
source("analisiSerieStoriche__B__reportConfiguration.r")
tkgrid(b.plotSelectedSeries[["button"]],b.createReport[["button"]],b.configureReport[["button"]],padx=5)
source("analisiSerieStoriche__C__tabnotebook_Inputs.r")


## crea la finestra di testo che contiene il report
report.txt <- create_textwindow(parent=tabNotebook[["pages"]][[2]],bg="white",font="courier",state="disabled",
                              height=32,width=60,withScrollBarY=TRUE)
report.txt[["setTag"]](tagName="title",tagStructure="-font {-family helvetica -size 14}")
report.txt[["setTag"]](tagName="nomeSerie",tagStructure="-font {-family helvetica -size 12}")
report.txt[["setFont"]](font="courier",size=10)

## posiziona i frame ed i widget singoli
row=0
# tkgrid(topMenu[["menu"]],row=row,column=0,sticky="nw");row=row+1
# tkgrid(menu[["menu"]],row=row,column=0,sticky="nw");row=row+1
tkgrid(f.buttons,pady=pady,row=2,column=1,sticky="s")
tkgrid(slb.timeSeries[["frame"]],row=row,column=0,padx=padx,pady=pady);row=row+1
tkgrid(l.chartType[["label"]],r.tsType[["frame"]])
tkgrid(f.chartType,row=row,column=0);row=row+1
tkgrid(dateFrame,row=row,column=0,sticky="w");row=row+1

tkgrid(frame.tabnotebook,padx=padx,pady=pady,row=0,column=1,rowspan=3,sticky="n")
# tkgrid(f.buttons,pady=pady,row=2,column=1,sticky="s") #,gistuo!

tkpack(report.txt[["frame"]],pady=10,padx=10,anchor="center")
.Tcl(paste("ttk::style", "theme use", "clam"))

#.Tcl(paste("ttk::style configure TButton -font \"helvetica 10\""))

#nomiConStili <- read.csv(file="/home/claudio/XP/analisi serie storiche/lyxor/Lyxor Platform Track Record (Class B) 2009-03-10+Manac 2x #strategies.csv",header=TRUE,as.is=TRUE)
#rownames(nomiConStili) <- nomiConStili[[1]]