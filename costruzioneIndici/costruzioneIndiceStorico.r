costruzioneIndiceStorico <- function(dati,rebalFreq=9) {
## costruzioneIndiceStorico.r

## datiXXX è la lista di output della procedura filterFunds che consiste
## nel vettore "nomiFondi", nella matrice dei dati ""matriceDati"", 
## nel data.frame "fondi.df", nel vettore dei "criteri" e nella stringa "strategia"


  strategyNames <- vector(mode="character")
  nbFundsDesired=vector(mode="integer")
  indexWeights=vector(mode="numeric")
  startDates=vector(mode="integer")
  endDates=vector(mode="integer")
  for (l in dati) {
     strategyNames  <-  c(strategyNames,l$strategia)
     nbFundsDesired <-  c(nbFundsDesired,l$nbFundsDesired)
     indexWeights   <-  c(indexWeights,l$indexWeight)

     if (l$endDate <= 0) endD <- nrow(l$matriceDati) - l$endDate else endD <- l$endDate
     if (l$startDate <= 0) startD <- endD + l$startDate else startD <- l$startDate
     startDates     <-  c(startDates,startD)
     endDates       <-  c(endDates,endD)
  }

  if (sum(indexWeights) != 1) {
    tkmessageBox(message="La data somma dei pesi non è 1!",icon="error") #'
    return()
  }

  rm(startD,endD)
  startDate <- max(startDates)
  endDate <- min(endDates)

  if (startDate > endDate) {
    tkmessageBox(message="La data d'inizio è posteriore a quella di fine.",icon="error") #'
    return()
  }

  rebalanceRequired <- function(rebalFreq,presentWeights,weightsDesired) {
     if (rebalancing == rebalFreq) return(TRUE)
     return(FALSE)
  }

  rebalance <- function(presentQuantities,prices,weightsDesired) {
     ## questa funzione esegue un ribilanciamento dei pesi
     quantities <- presentQuantities
     Mo <- presentQuantities*prices
     Vo <- sum(Mo)
     presentWeights <- Mo/Vo
     rebalancing <<- rebalancing + 1

     if (rebalanceRequired(rebalFreq,presentWeights,weightsDesired)) {
       M <- weightsDesired*Vo
       quantities <- M/prices
       rebalancing <<- 0
     }
     return(quantities)
  }


  ## crea la matrice dei dati
  prezzi.m <- dati[[1]]$matriceDati[startDate:endDate,]
  if (length(dati)>1) {
    for (i in 2:length(dati)) prezzi.m <- cbind(prezzi.m,dati[[i]]$matriceDati[startDate:endDate,])
  }
  weightsDesired <- rep(indexWeights/nbFundsDesired,nbFundsDesired)
  rebalancing <- 0
  date <- rownames(prezzi.m)
  fondi <- colnames(prezzi.m)
  prezzi.m = prezzi.m %*% diag(1/prezzi.m[1,])
  dimnames(prezzi.m) <- list(date,fondi)

  weightsHistory.m <- matrix(0,ncol=length(date),nrow=ncol(prezzi.m))
  weightsHistory.m[,1] = weightsDesired
  portfolio <- vector(mode="numeric",length=length(date))
  portfolio[1] <- 100

  if (length(date) == 1) return("caso non interessante, solo una data!")

  for (i in 2:length(date)) {
    weightsHistory.m[,i] <- rebalance(presentQuantities=weightsHistory.m[,i-1],prices=prezzi.m[i,],weightsDesired=weightsDesired)
    portfolio[i] <- sum(prezzi.m[i,]*weightsHistory.m[,i-1])
  }
  return(list(portfolio,t(weightsHistory.m)))
  # scan(nlines=1,what="character")

}