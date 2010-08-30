
replaceZeroWithNA_real_ <- function(vectorOfReals) {
  # identifica 0. se in posizione "i" ci sono NA, isZero[i] sara' uguale a NA
  isZero <- vectorOfReals == 0
  # sostituisci NA con FALSE nel vettore isZero
  isZero[is.na(isZero)] <- FALSE
  if (any(isZero))  vectorOfReals[isZero] <- NA
  return(vectorOfReals)
}


computePercReturns <- function(prezzi.df) {
  matrice_prezzi <- sapply(prezzi.df,replaceZeroWithNA_real_)
  dimnames(matrice_prezzi) <- dimnames(prezzi.df)
  nbObsReturns <- nrow(matrice_prezzi) - 1
  matrice_rendimenti = (matrice_prezzi[-1,] - matrice_prezzi[1:nbObsReturns,]) / matrice_prezzi[1:nbObsReturns,]
  return(as.data.frame(matrice_rendimenti))
}

computeLogReturns <- function(prezzi.df) {
  names <- dimnames(prezzi.df)
  matrice_prezzi <- sapply(prezzi.df,replaceZeroWithNA_real_)
  dimnames(matrice_prezzi) <- dimnames(prezzi.df)
  nbObsReturns <- nrow(matrice_prezzi) - 1
  matrice_rendimenti = log(matrice_prezzi[-1,]/matrice_prezzi[1:nbObsReturns,])
  return(as.data.frame(matrice_rendimenti))
}
