

drawDown <- function(x,T=length(x)) {
  # x is a vector with possibly NA's
  # T is the upper limit for the computation of the drawdown.

  lth <- length(x)

  if (T==0 | lth == 0 ) return(NA_real_)

  if (T>lth) T <- lth
  x <- x[1:T]

  # identify the NA observation
  areNAObservations <- is.na(x)

  if (all(areNAObservations)) return(NA_real_)

  # identify the last not NA observation
  areNotNAObservations <- !areNAObservations
  lastGood <- max((1:T)[areNotNAObservations])
  T <- lastGood
  
  areNotNAObservations <- areNotNAObservations[1:T]
  if (sum(areNotNAObservations)<2) return(0)

  return(max(0,max(x[1:(T-1)],na.rm=TRUE)-x[T],na.rm=TRUE))
}


maxDrawDown <- function(x,T=length(x),percentage=FALSE) {
  if (T==0) return(NA_real_)

  lth <- length(x)
  if (T>lth) T <- lth

  DD <- rep(NA_real_,T)
  peak <- -Inf
  validIndices <- !is.na(x[1:T])
  if (validIndices[1]) {
    DD[1] = 0
    if (peak < x[1]) peak <- x[1]
  }

  if (T==1) return(DD)
  for (i in 2:T) {
    if (validIndices[i]) {
      if (x[i] > peak) peak <- x[i]
      if (percentage) DD[i] = 100 * (peak - x[i]) / peak else DD[i] = peak - x[i]
    } else {
      DD[i] = DD[i-1]
    }
  }
  return(max(DD,na.rm=TRUE))
}


annualizeFactor <- function(frequency) {
  if (frequency == "Daily") return(sqrt(252))
  if (frequency == "Weekly") return(sqrt(52))
  if (frequency == "Monthly") return(sqrt(12))
  if (frequency == "Quarterly") return(2.0)
  if (frequency == "Yearly") return(1.0)
  if (frequency == "Irregular") return(1.0)
}

annualizedMean <- function(x,frequency="Monthly") {
  annualizedM <- mean(x,na.rm=TRUE)*annualizeFactor(frequency)^2
  if (is.nan(annualizedM)) annualizedM <- NA_real_
  return( annualizedM )
}

annualizedStdev <- function(x,frequency="Monthly") {
  return( sqrt(var(x,na.rm=TRUE))*annualizeFactor(frequency) )
}


# da rimuovere
a <- function(n=1000) {
x <- rnorm(n)
y <- rnorm(n) + x

q1 <- cor(x,y)
q2 <- cor(x,y,method="spearman")
q3 <- cor(x,y,method="kendall")
print(q1)
print(q2)
print(q3)

}


