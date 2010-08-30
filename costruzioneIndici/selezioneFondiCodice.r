selectionCriteria <- function(desiredNbFunds,criteria.df) {

   if (missing(criteria.df)) {
     selected <- unique(round(runif(desiredNbFunds, min=1, max=nbFunds)))
     while (length(selected) < desiredNbFunds) {
        selected <- unique(c(selected,round(runif(desiredNbFunds-length(selected), min=1, max=nbFunds))))
     }
     return(selected)
   }

   criteriaNames <- colnames(criteria.df)
   nbCriteria <- length(criteriaNames)
   if (nbCriteria == 0 | nbCriteria > 5) return()
   signum <- c(1,-1,-1,1,-1)
   names(signum) <- c("Max Drawdown %","Nb. Obs.","Annualized mean","Annualized stdev","AUM")
   if (is.element("Nb. Obs.",criteriaNames)) {
     criteria.df[,"Nb. Obs."] <- as.integer(matrix(unlist(strsplit(x=criteria.df[,"Nb. Obs."],split=" \\(")),ncol=2,byrow=TRUE)[,1])
   }

   if (!all(is.element(criteriaNames,names(signum)))) {
     tkmessageBox(message="The selection strategy contains non valid elements",icon="error")
     return()
   } 

   tmp <- signum[criteriaNames]
   tmp <- t(t(as.matrix(criteria.df,drop=FALSE))*tmp) 
   if (length(criteriaNames) == 1) order.v <- order(tmp[,1])
   if (length(criteriaNames) == 2) order.v <- order(tmp[,1],tmp[,2])
   if (length(criteriaNames) == 3) order.v <- order(tmp[,1],tmp[,2],tmp[,3])
   if (length(criteriaNames) == 4) order.v <- order(tmp[,1],tmp[,2],tmp[,3],tmp[,4])
   if (length(criteriaNames) == 5) order.v <- order(tmp[,1],tmp[,2],tmp[,3],tmp[,4],tmp[,5])
   return(order.v[1:desiredNbFunds])
}



filterFunds <- function (dati.l,statistiche.df,MaxDrawdownPerc=15,NbObs=100,AnnualizedMean=0,AnnualizedStdev=12,AUM.mio=Inf,strategia="",
                         desiredNbFunds="all",indexWeight=0.4,startDate=1,endDate=nrow(matriceDati),selectionStrategy)
{

   ## se endDate < 0 allora prendi come valore nrow(matriceDati) + endDate
   ## se startDate < 0 allora inizia da endDate + startDate
   ## selectionStrategy contiene uno o più di questi campi "Max Drawdown %","Nb. Obs.","Annualized mean","Annualized stdev","AUM"


   ## selezioneFondiCodice.r

   ## statistiche.df è un data.frame contenente le seguenti colonne: 
   ## "Frequency","Nb. Obs.","Start date","End date","Drawdown","Max Drawdown","Max Drawdown %","Annualized mean",
   ## "Annualized stdev","Skewness","Excess kurtosis"

   ## questa procedura restituisce la lista list(nomiFondi,fondi.df,criteri). La prima componente è un vettore dei nomi dei
   ## fondi selezionati, la seconda è un data.frame contenente le prime 3 colonne opzionali c("Nome","AUM","Currency") e 
   ## poi il data.frame coi dati. Infine la terza componente contiene il vettore dei criteri utilizzati con i rispettivi valori.

   criteriEsclusione <- list("Max Drawdown %"=MaxDrawdownPerc,"Nb. Obs."=NbObs,"Annualized mean"=AnnualizedMean,"Annualized stdev"=AnnualizedStdev,AUM.mio=AUM.mio)

   if (strategia=="") {
     tkmessageBox(message="Please specify a value for the 'strategia' input parameter.",icon="error")
     return()
   }


   dati_AUM.df <- dati.l$AUM
   matriceDati <- dati.l$matrix

   ## seleziona solo le serie con "Max Drawdown %" < livello specificato
   filtro_MDD <- statistiche.df$"Max Drawdown %" > criteriEsclusione$"Max Drawdown %"
   filtro_NbObs <- statistiche.df$"Nb. Obs." < criteriEsclusione$"Nb. Obs."
   filtro_AnnualizedMean <- statistiche.df$"Annualized mean" < criteriEsclusione$"Annualized mean"
   filtro_AnnualizedStdev <- statistiche.df$"Annualized stdev" > criteriEsclusione$"Annualized stdev"

   fondi.df <- statistiche.df
   nomi_serie <<- rownames(fondi.df)
   rownames(fondi.df) <- nomi_serie

   filtro_all = filtro_MDD | filtro_NbObs | filtro_AnnualizedMean | filtro_AnnualizedStdev

   names(filtro_all) <- nomi_serie

   fondiSelezionati <- nomi_serie[!filtro_all]

   if (AUM.mio < Inf) {
     ## crea i fattori di conversione per le varie monete, moneta di riferimento il dollaro
     conversionUSD_XXX=c(1,0.761,0.682,1.147,96.0,96.0,8.13,1.397,1.21,6.71,3.458,7.75,4.24,8.756)
     names(conversionUSD_XXX) = c("USD","EUR","GBP","CHF","jpy","JPY","SEK","AUD","CAD","NOK","PLN","HKD","ILS","ZAR")


     filtro_AUM <- ((dati_AUM.df[,"AUM"]/conversionUSD_XXX[dati_AUM.df[,"Currency"]]) >= AUM.mio*1000000) | (dati_AUM.df[,"AUM"] == 0)

     fondiNotOk <- dati_AUM.df[!filtro_AUM,"Nome"]
     fondiSelezionati <- setdiff(fondiSelezionati,fondiNotOk)
   }

   fondi.df <- data.frame(dati_AUM.df[fondiSelezionati,],fondi.df[fondiSelezionati,],check.names = FALSE,stringsAsFactors = FALSE)
   matriceDati <- matriceDati[,fondiSelezionati]

   ## crea la funzione per selezionare i fondi rispetto alle osservazioni mancanti
   filterFundsWithNA <- function(dataMatrix) {
     notAvailable <- is.na(dataMatrix)
     toDiscard <- apply(X=notAvailable,MARGIN=2,FUN=any)
     return(!toDiscard)
   }

   ## crea una matrice di dati temporanea con solo le informazioni desiderate
   if (endDate <= 0) endD <- nrow(matriceDati) + endDate else endD <- endDate
   if (startDate <= 0) startD <- endD + startDate else startD <- startDate
   matriceDatiTmp <- matriceDati[startD:endD,]
   rm(startD,endD)

   ## elimina i fondi con dei NA
   daTenere <- filterFundsWithNA(matriceDatiTmp)
   rm(matriceDatiTmp)
   fondiSelezionati = fondiSelezionati[daTenere]
   fondi.df <- fondi.df[daTenere,]
   matriceDati <- matriceDati[,daTenere]

   ## -------------- Temporaneamente ------------------------------
   ## crea la funzione per selezionare i fondi rispetto alla valuta 

   # elimina i fondi non in usd
   #isDesired <- fondi.df[,"Currency"] == currency
   #matriceDati <- matriceDati[,isDesired]
   #fondiSelezionati <- fondiSelezionati[isDesired]
   #fondi.df <- fondi.df[isDesired,]

   #rm(currency,isDesired)
   ## -------------- Fine temporaneamente -------------------------

   ## verifica che i fondi siano sufficienti rispetto al numero desiderato

  if (desiredNbFunds != "all") { 
    nbFunds <- ncol(matriceDati)
    notOk <- nbFunds < desiredNbFunds
    if (notOk) {
       tkmessageBox(message=paste("La strategia", strategia, "ha un numero di fondi insufficiente: disponibili",nbFunds,"desiderati",desiredNbFunds,"."),icon="error")
       return()
    }

    ## seleziona i fondi "migliori"
    if (nbFunds == desiredNbFunds) {
       return(list(nomiFondi=fondiSelezionati,matriceDati=matriceDati,statistiche.df=fondi.df,
               criteri=criteriEsclusione,desiredNbFunds=desiredNbFunds,indexWeight=indexWeight,
               startDate=startDate,endDate=endDate,selectionStrategy=selectionStrategy,strategia=rep(strategia,length(fondiSelezionati)))
            )
    }

    if (!missing(selectionStrategy)) selected <- selectionCriteria(des=desiredNbFunds,fondi.df[,selectionStrategy,drop=FALSE]) else selected <- selectionCriteria(des=desiredNbFunds)

    return(list(nomiFondi=fondiSelezionati[selected],matriceDati=matriceDati[,selected,drop=FALSE],statistiche.df=fondi.df[selected,,drop=FALSE],
               criteri=criteriEsclusione,desiredNbFunds=desiredNbFunds,indexWeight=indexWeight,
               startDate=startDate,endDate=endDate,selectionStrategy=selectionStrategy,strategia=rep(strategia,length(fondiSelezionati[selected])))
          )
    }

    return(list(nomiFondi=fondiSelezionati,matriceDati=matriceDati,statistiche.df=fondi.df,
              criteri=criteriEsclusione,desiredNbFunds=desiredNbFunds,indexWeight=indexWeight,
              startDate=startDate,endDate=endDate,selectionStrategy=selectionStrategy,strategia=rep(strategia,length(fondiSelezionati)))
          )
}

aggregaFondi <- function(serieFiltrate,k=5) {
  ## selezioneFondiCodice.r
  nomiFondi = serieFiltrate$NomiFondi
  correlazioni <- cor(tsLogReturns.df[,nomiFondi],use="pair")
  distanza <- as.dist(sqrt(1-correlazioni^2))
  tmp <- cutree(hclust(distanza),k=k)
  nrClassi <- max(unique(tmp))
  result <- list()
  for (i in 1:nrClassi) {
    result[[i]] <- names(tmp[tmp == i])
  }
   
  result = list(hedgeClusters=result,tsLogReturns.df=tsLogReturns.df[,nomiFondi])
  class(result) <- "hedgeCluster"
  return(result)
}

summary.hedgeCluster <- function(gruppi.l) {
  ## selezioneFondiCodice.r
  if (!exists("tsFreq",envir=.GlobalEnv)) conversionFactor <- 12 else conversionFactor <- annualizeFactor(tsFreq)^2
  k= length(gruppi.l$hedgeClusters)
  if (k==0) {
    print("Ci sono 0 gruppi.")
    return()
  }

  clusterMean <- function(nomiSerie,dati.df) {
    return(mean(mean(dati.df[,nomiSerie],na.rm=TRUE)))
  }

  lengths <- sapply(gruppi.l$hedgeClusters,length)
  means   <- sapply(gruppi.l$hedgeClusters,clusterMean,gruppi.l$tsLogReturns.df)*conversionFactor
  print (paste("Nr di fondi per gruppo: ",lengths,". Media del rendimento del gruppo: ",means,sep=""))
}

plottaFondiAggregati <- function(gruppi.l,stat,title="") {
  ## selezioneFondiCodice.r

  ## gruppi.l è una lista proveniente da aggregaFondi
  ## stat è la lista che contiene i risultati della procedura onCreateReport()
  ## cioè "Levels" - "LogReturns" - "Percreturns"
  gruppi.l = gruppi.l$hedgeClusters
  k = length(gruppi.l)
  if (k == 0) return()

  ## determina il range delle x ed il range delle y
  daPlottare <- is.element(rownames(stat[["LogReturns"]]),unlist(gruppi.l))
  rangeX <- range(stat$LogReturns[["Annualized stdev"]][daPlottare])
  rangeY <- range(stat$LogReturns[["Annualized mean"]][daPlottare])
  daPlottare <- is.element(rownames(stat[["LogReturns"]]),gruppi.l[[1]])
  plot(stat$LogReturns[["Annualized stdev"]][daPlottare],
       stat$LogReturns[["Annualized mean"]][daPlottare],
       xlab="Volatility",ylab="Average return",
       xlim=rangeX,ylim=rangeY,main=title)
  myColors <- c("blue","red","brown" ,"yellow","violet","coral1", "cyan" ,"gold","gray" ,"green",
                "lavender","linen","magenta","navy","orange","pink" ,"salmon","tomato")

  if (k==1) return()
  lmyColors <- length(myColors)
  if (lmyColors < k) {
    tkmessageBox(message="Il numero di colori è insufficiente rispetto al numero di gruppi.\nAlcuni colori saranno ripetuti",icon="info")
  }
  for (i in 2:k) {
    daPlottare <- is.element(rownames(stat[["LogReturns"]]),gruppi.l[[i]])
    points(stat$LogReturns[["Annualized stdev"]][daPlottare],
           stat$LogReturns[["Annualized mean"]][daPlottare], 
           col = myColors[i%%lmyColors]
           )
  }
}
# fondiFiltrati <- filterFunds()
