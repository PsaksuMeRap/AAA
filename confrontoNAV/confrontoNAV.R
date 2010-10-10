# TODO: Add comment
# 
# Author: claudio
###############################################################################


datiBacep <- read.csv("/home/claudio/eclipse/AAA/confrontoNAV/Dati/Serie fondi FI.csv",as.is=TRUE)
dati <- read.csv("/home/claudio/eclipse/AAA/confrontoNAV/Dati/nav fondi bacep 2010-10-10.csv",as.is=TRUE)

rownames(datiBacep) <- datiBacep[["Data"]]
rownames(dati) <- dati[["Data"]]

giorni <- sort(unique(c(datiBacep[["Data"]]),dati[["Data"]]))

confronto.dat <- data.frame(date=giorni,bacep=NA_real_,ds=NA_real_,stringsAsFactors=FALSE)
rownames(confronto.dat) <- giorni

x <- datiBacep[,"Data"]
confronto.dat[x,"bacep"] = datiBacep[x,"NAV"]

x <- dati[,"Data"]
confronto.dat[x,"ds"] = dati[x,"FI"]