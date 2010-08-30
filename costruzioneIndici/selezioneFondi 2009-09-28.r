## ---------------------------------------------------------------------------
## -------------------------- INIZIO ANALISI ---------------------------------
## ---------------------------------------------------------------------------


## codice per analisi hedge funds di capitalgest
setwd("/home/claudio/Dropbox/analisi serie storiche/")
#source("/home/claudio/Dropbox/analisi serie storiche/analisiSerieStoriche__A.r")
source("/home/claudio/Dropbox/analisi serie storiche/capitalgest/importazione.r")
source("/home/claudio/Dropbox/analisi serie storiche/capitalgest/selezioneFondiCodice.r")
#source("/home/claudio/Dropbox/analisi serie storiche/capitalgest/costruzioneIndiceStorico.r")

source("/home/claudio/Dropbox/analisi serie storiche/capitalgest/procedure_esterne.r")

env <- topenv()
### seleziona la frequenza e le statistiche necessarie alla selezione (da fare una sola volta)
setTsFrequency(freq="Monthly")
# filtro valute
soloValute <- c("usd","eur","gbp","chf","jpy")
### ---------------------------------------------------------------------------


### importa le serie storiche dei rendimenti e trasformale in livelli
CTA <- importaHedgeFunds("CTA",soloValute=soloValute)
## CTA <- importaHedgeFunds("CTA",soloValute="tutte",valuteDaAggiustare=c())
CTA_matrix <- CTA$matrix
CTA_AUM.df <- CTA$AUM.df
rendimentoForward.df <- CTA$rendimentoForward.df
rm(CTA)


ELS <- importaHedgeFunds("ELS",soloValute=soloValute)
## ELS <- importaHedgeFunds("ELS",soloValute="tutte",valuteDaAggiustare=c())
ELS_matrix <- ELS$matrix
ELS_AUM.df <- ELS$AUM.df
rendimentoForward <- ELS$rendimentoForward
rm(ELS)


MAC <- importaHedgeFunds("Macro",soloValute=soloValute)
## MAC <- importaHedgeFunds("Macro",soloValute="tutte",valuteDaAggiustare=c())
MAC_matrix <- MAC$matrix
MAC_AUM.df <- MAC$AUM.df
rendimentoForward <- MAC$rendimentoForward
rm(MAC)
### -------------------- FINE IMPORTAZIONE DATI -----------------------------------



## Applica un altro filtro per la cross validation 
dataMassima <- as.Date("2007-07-31")
selection <- as.Date(rownames(MAC_matrix)) <= dataMassima

MAC_submatrix <- MAC_matrix[selection,]
MAC_sub.l <- list(matrix=MAC_submatrix,AUM=MAC_AUM.df)
ELS_submatrix <- ELS_matrix[selection,]
ELS_sub.l <- list(matrix=ELS_submatrix,AUM=ELS_AUM.df)
CTA_submatrix <- CTA_matrix[selection,]
CTA_sub.l <- list(matrix=CTA_submatrix,AUM=CTA_AUM.df)
### --------------------- FINE FILTRO CROSS VALIDATION -----------------------------




### ---------------------------------------------------------------------------------
### seleziona i dati per la strategia "CTA"
### ---------------------------------------------------------------------------------

importDataMatrixDirect("CTA_submatrix")

slb.timeSeries[["set.values"]](newValues=colnames(ts.df),widget=2)
slb.timeSeries[["reset"]](widget=1)


onCreateReport()
strategiaCTAStat <- computedStatistics # salva i risultati
quantile(computedStatistics[[2]]["Annualized stdev"][,1],seq(0.1,0.7,by=0.05)) 


## crea il data.frame con i risultati sia per i livelli che per i logReturns
CTA_sub_Stat.df <- data.frame(computedStatistics[[1]],computedStatistics[[2]],check.names=FALSE,stringsAsFactors=FALSE)

## filtra i fondi
CTA_sub_Filtrati <- filterFunds(CTA_sub.l,CTA_sub_Stat.df,MaxDrawdownPerc=15,NbObs=24,AnnualizedMean=0.02,AnnualizedStdev=14.6,AUM.mio=100,strategia="CTA",desiredNbFunds="all",startDate=-36,indexWeight=0.2,selectionStrategy=c("Annualized mean") )

## crea la matrice dei dati filtrati sul sottoperiodo per analizzarla col programma grafico
CTA_sub_filtrati_matrix <- CTA_sub_Filtrati$matriceDati
CTA_sub_filtrati_matrix <- CTA_matrix[,CTA_sub_Filtrati$nomiFondi]

## stampa il numero di fondi per valuta
summary(filtroValuta(CTA_sub_Filtrati$statistiche.df,verbose=FALSE))

#CTA_Gruppi <- aggregaFondi(CTA_Filtrati,k=5)
#plottaFondiAggregati(CTA_Gruppi,CTA_Stat)
### ---------------------------------------------------------------------------------
### CTA - FINE SELEZIONE FONDI CTA
### ---------------------------------------------------------------------------------


### ---------------------------------------------------------------------------------
### seleziona i dati per la strategia "ELS"
### ---------------------------------------------------------------------------------
importDataMatrixDirect("ELS_submatrix")

slb.timeSeries[["set.values"]](newValues=colnames(ts.df),widget=2)
slb.timeSeries[["reset"]](widget=1)


onCreateReport()
strategiaELSStat <- computedStatistics # salva i risultati
quantile(computedStatistics[[2]]["Annualized stdev"][,1],seq(0.1,0.7,by=0.05))

## crea il data.frame con i risultati sia per i livelli che per i logReturns
ELS_sub_Stat.df <- data.frame(computedStatistics[[1]],computedStatistics[[2]],check.names=FALSE,stringsAsFactors=FALSE)

## filtra i fondi
ELS_sub_Filtrati <- filterFunds(ELS_sub.l,ELS_sub_Stat.df,MaxDrawdownPerc=15,NbObs=24,AnnualizedMean=0.02,AnnualizedStdev=8.41,AUM.mio=100,strategia="ELS",
                            desiredNbFunds="all",startDate=-36,indexWeight=0.2,selectionStrategy=c("Annualized mean") )
## crea la matrice dei dati filtrati sul sottoperiodo per analizzarla col programma grafico
ELS_sub_filtrati_matrix <- ELS_sub_Filtrati$matriceDati
ELS_sub_filtrati_matrix <- ELS_matrix[,ELS_sub_Filtrati$nomiFondi]

## stampa il numero di fondi per valuta
summary(filtroValuta(ELS_sub_Filtrati$statistiche.df,verbose=FALSE))

#ELS_Gruppi <- aggregaFondi(ELS_Filtrati,k=5)
#plottaFondiAggregati(ELS_Gruppi,ELS_Stat)
### ---------------------------------------------------------------------------------
### ELS - FINE SELEZIONE FONDI 
### ---------------------------------------------------------------------------------









### ---------------------------------------------------------------------------------
### seleziona i dati per la strategia "MAC"
### ---------------------------------------------------------------------------------
importDataMatrixDirect("MAC_submatrix")

slb.timeSeries[["set.values"]](newValues=colnames(ts.df),widget=2)
slb.timeSeries[["reset"]](widget=1)


onCreateReport()
strategiaMACStat <- computedStatistics # salva i risultati
quantile(computedStatistics[[2]]["Annualized stdev"][,1],seq(0.1,0.7,by=0.05))

## crea il data.frame con i risultati sia per i livelli che per i logReturns
MAC_sub_Stat.df <- data.frame(computedStatistics[[1]],computedStatistics[[2]],check.names=FALSE,stringsAsFactors=FALSE)

## filtra i fondi
MAC_sub_Filtrati <- filterFunds(MAC_sub.l,MAC_sub_Stat.df,MaxDrawdownPerc=15,NbObs=24,AnnualizedMean=0.02,AnnualizedStdev=12,AUM.mio=100,strategia="MAC",
                            desiredNbFunds="all",startDate=-36,indexWeight=0.2,selectionStrategy=c("Annualized mean") )

## crea la matrice dei dati filtrati sul sottoperiodo per analizzarla col programma grafico
MAC_sub_filtrati_matrix <- MAC_sub_Filtrati$matriceDati
MAC_sub_filtrati_matrix <- MAC_matrix[,MAC_sub_Filtrati$nomiFondi]

## stampa il numero di fondi per valuta
summary(filtroValuta(MAC_sub_Filtrati$statistiche.df,verbose=FALSE))

#MAC_Gruppi <- aggregaFondi(MAC_Filtrati,k=5)
#plottaFondiAggregati(MAC_Gruppi,MAC_Stat)
### ------------ FINE SELEZIONE FONDI MAC -------------------------------------------

write.csv(CTA_sub_filtrati_matrix,"CTA.csv")
write.csv(ELS_sub_filtrati_matrix,"ELS.csv")
write.csv(MAC_sub_filtrati_matrix,"MAC.csv")



nomi_fondi_selezionatiELS <- slb.timeSeries$get.selected()
costruzioneIndiceStorico(dati=list(CTA_Filtrati,ELS_Filtrati,MAC_Filtrati)) 
