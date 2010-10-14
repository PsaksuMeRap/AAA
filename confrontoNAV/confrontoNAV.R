# TODO: Add comment
# 
# Author: claudio
###############################################################################

# confronto dati fixed income

datiBacep <- read.csv("/home/claudio/eclipse/AAA/confrontoNAV/Dati/Serie fondi FI.csv",as.is=TRUE)
datiDs <- read.csv("/home/claudio/eclipse/AAA/confrontoNAV/Dati/nav fondi bacep 2010-10-10.csv",as.is=TRUE)

rownames(datiBacep) <- datiBacep[["Data"]]
rownames(datiDs) <- datiDs[["Data"]]

giorni <- sort(unique(c(datiBacep[["Data"]],datiDs[["Data"]])))

confronto.dat <- data.frame(Data=giorni,bacep=NA_real_,ds=NA_real_,stringsAsFactors=FALSE)
rownames(confronto.dat) <- giorni

x <- datiBacep[,"Data"]
confronto.dat[x,"bacep"] = datiBacep[x,"NAV"]

x <- datiDs[,"Data"]
confronto.dat[x,"ds"] = datiDs[x,"FI"]


# analizza solo il periodo da quando Datastream ha le osservazioni
# cioè dal 2007-02-02 incluso in avanti

rimuovi <- !is.na(confronto.dat[["ds"]])
confronto <- confronto.dat[rimuovi,]


sonoDateBacepMancanti <- is.na(confronto[["bacep"]])
numeroDateBacepMancanti <- sum(sonoDateBacepMancanti)
percDateBacepMancanti <- numeroDateBacepMancanti / length(sonoDateBacepMancanti)



datiBacepAnalisi <- confronto[!sonoDateBacepMancanti,"bacep"] 
datiDsAnalisi <- confronto[!sonoDateBacepMancanti,"ds"]


datiBacepAnalisi[2:length(datiBacepAnalisi)] - datiDsAnalisi[1:(length(datiDsAnalisi)-1)]


